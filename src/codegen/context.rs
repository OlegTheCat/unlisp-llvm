use crate::repr::HIR;
use crate::runtime;

use super::common::*;
use super::top_level::compile_top_level_hirs;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use std::collections::{HashMap, HashSet};

use std::rc::Rc;

pub type CompiledFn = JitFunction<unsafe extern "C" fn() -> runtime::defs::Object>;

pub struct CodegenContext<'a> {
    pub llvm_ctx: &'a Context,
    pub builder: Builder,
    pub pass_manager: PassManager,

    execution_engine: ExecutionEngine,
    counter: u64,
    module: Module,
    blocks_stack: Vec<Rc<BasicBlock>>,
    envs: Vec<HashMap<String, BasicValueEnum>>,
    defined_str_literals: HashSet<String>,
    str_literal_globals: HashMap<String, GlobalValue>,
}

impl<'a> CodegenContext<'a> {
    fn gen_unique_int(&mut self) -> u64 {
        self.counter += 1;
        self.counter
    }

    pub fn mangle_str(&mut self, s: impl Into<String>) -> String {
        format!("{}__unlisp_{}", s.into(), self.gen_unique_int())
    }

    fn make_pass_manager(module: &Module) -> PassManager {
        let fpm = PassManager::create_for_function(module);

        // fpm.add_instruction_combining_pass();
        // fpm.add_reassociate_pass();
        // fpm.add_gvn_pass();
        // fpm.add_cfg_simplification_pass();
        // fpm.add_basic_alias_analysis_pass();
        // fpm.add_promote_memory_to_register_pass();
        // fpm.add_instruction_combining_pass();
        // fpm.add_reassociate_pass();

        fpm.initialize();

        fpm
    }

    pub fn new(llvm_ctx: &'a Context) -> Self {
        let module = llvm_ctx.create_module("mod_0");
        let ee = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("couldn't create execution engine");
        let builder = llvm_ctx.create_builder();

        runtime::defs::gen_defs(llvm_ctx, &module);
        runtime::exceptions::gen_defs(llvm_ctx, &module);

        Self {
            counter: 0,
            llvm_ctx: llvm_ctx,
            pass_manager: Self::make_pass_manager(&module),
            module: module,
            execution_engine: ee,
            builder: builder,
            blocks_stack: vec![],
            envs: vec![],
            defined_str_literals: HashSet::new(),
            str_literal_globals: HashMap::new(),
        }
    }

    pub fn reinitialize(&mut self) {
        let module = self
            .llvm_ctx
            .create_module(format!("mod_{}", self.gen_unique_int()).as_str());

        self.execution_engine
            .add_module(&module)
            .expect("couldn't add module to execution engine");

        runtime::defs::gen_defs(self.llvm_ctx, &module);
        runtime::exceptions::gen_defs(self.llvm_ctx, &module);

        self.pass_manager.finalize();
        self.pass_manager = Self::make_pass_manager(&module);

        self.blocks_stack = vec![];
        self.module = module;
        self.str_literal_globals = HashMap::new();
    }

    fn declare_str_literal(&mut self, lit: String) -> GlobalValue {
        let array_ty = self.llvm_ctx.i8_type().array_type((lit.len() + 1) as u32);
        let global = self.module.add_global(array_ty, None, lit.as_str());
        self.str_literal_globals.insert(lit, global.clone());

        global
    }

    fn define_str_literal(&mut self, lit: String) -> GlobalValue {
        let mut charcodes: Vec<_> = lit.clone().chars().map(|c| c as u8).collect();
        charcodes.push(0);

        let array_ty = self.llvm_ctx.i8_type().array_type(charcodes.len() as u32);
        let array_vals: Vec<_> = charcodes
            .iter()
            .map(|v| self.llvm_ctx.i8_type().const_int((*v).into(), false))
            .collect();

        let global = self.module.add_global(array_ty, None, lit.as_str());
        global.set_initializer(&self.llvm_ctx.i8_type().const_array(array_vals.as_slice()));

        self.defined_str_literals.insert(lit.clone());
        self.str_literal_globals.insert(lit.clone(), global.clone());

        global
    }

    pub fn get_or_globalize_str_literal(&mut self, lit: impl Into<String>) -> GlobalValue {
        let lit = lit.into();
        if self.defined_str_literals.get(&lit).is_some() {
            if let Some(g_val) = self.str_literal_globals.get(&lit) {
                g_val.clone()
            } else {
                self.declare_str_literal(lit)
            }
        } else {
            self.define_str_literal(lit)
        }
    }

    pub fn str_literal_as_i8_ptr(&mut self, lit: impl Into<String>) -> BasicValueEnum {
        let global = self.get_or_globalize_str_literal(lit);
        self.builder.build_bitcast(
            global.as_pointer_value(),
            self.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic),
            "lit_as_i8_ptr",
        )
    }

    pub fn lookup_known_type(&self, name: &str) -> BasicTypeEnum {
        self.module
            .get_type(name)
            .expect(format!("known type {} not found", name).as_str())
    }

    pub fn lookup_known_fn(&self, name: &str) -> FunctionValue {
        self.module
            .get_function(name)
            .expect(format!("known function {} not found", name).as_str())
    }

    pub fn get_module(&self) -> &Module {
        &self.module
    }

    pub fn push_env(&mut self) {
        self.envs.push(HashMap::new())
    }

    pub fn save_env_mapping(&mut self, name: String, val: BasicValueEnum) {
        let len = self.envs.len();
        self.envs[len - 1].insert(name, val);
    }

    pub fn lookup_name(&self, name: &String) -> Option<BasicValueEnum> {
        for env in self.envs.iter().rev() {
            if let Some(val) = env.get(name) {
                return Some(val.clone());
            }
        }

        None
    }

    pub fn pop_env(&mut self) {
        self.envs.pop();
    }

    pub fn replace_cur_block(&mut self, block: Rc<BasicBlock>) -> Rc<BasicBlock> {
        let idx = self.blocks_stack.len() - 1;
        self.blocks_stack[idx] = block.clone();
        self.builder.position_at_end(&block);
        block
    }

    pub fn cur_block(&self) -> Rc<BasicBlock> {
        self.blocks_stack.last().cloned().expect("no current block")
    }

    pub fn append_block(&self) -> BasicBlock {
        let function = self.cur_block().get_parent().unwrap();

        self.llvm_ctx.append_basic_block(&function, "entry")
    }

    pub fn enter_block(&mut self) -> Rc<BasicBlock> {
        let function = self.cur_block().get_parent().unwrap();

        self.enter_fn_block(&function)
    }

    pub fn enter_fn_block(&mut self, function: &FunctionValue) -> Rc<BasicBlock> {
        let block = self.llvm_ctx.append_basic_block(&function, "entry");
        let block_rc = Rc::new(block);

        self.builder.position_at_end(&block_rc);
        self.blocks_stack.push(block_rc.clone());
        block_rc
    }

    pub fn exit_block(&mut self) -> Rc<BasicBlock> {
        let block = self.blocks_stack.pop().expect("no block to exit");
        self.builder
            .position_at_end(self.blocks_stack.last().unwrap());
        block
    }

    pub fn verify_or_panic(&self, f: &FunctionValue, tag: &str) {
        if !f.verify(true) {
            self.module.print_to_stderr();
            panic!("[{}] function verification failed", tag);
        }
    }

    pub unsafe fn compile_hirs(&mut self, hirs: &[HIR]) -> GenResult<CompiledFn> {
        let top_level_fn_name = compile_top_level_hirs(self, hirs)?;

        Ok(self
            .execution_engine
            .get_function(top_level_fn_name.as_str())
            .expect("couldn't find top-level function in execution engine"))
    }
}
