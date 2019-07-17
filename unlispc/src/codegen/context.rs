use crate::error;
use crate::repr::HIR;
use crate::runtime_defs;

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
use unlisp_rt;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub type CompiledFn = JitFunction<unsafe extern "C" fn() -> unlisp_rt::defs::Object>;

pub struct CodegenContext<'a> {
    pub llvm_ctx: &'a Context,
    pub builder: Builder,
    pub pass_manager: PassManager<FunctionValue>,

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

    fn make_pass_manager(module: &Module) -> PassManager<FunctionValue> {
        let fpm = PassManager::<FunctionValue>::create(module);

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

        runtime_defs::gen_defs(llvm_ctx, &module);

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

        runtime_defs::gen_defs(self.llvm_ctx, &module);

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

    pub fn codegen_hirs(&mut self, hirs: &[HIR]) -> GenResult<String> {
        compile_top_level_hirs(self, hirs)
    }

    pub fn compile_hirs_with_main(&mut self, hirs: &[HIR]) -> GenResult<()> {
        let code_init_fn_name = self.codegen_hirs(hirs)?;

        let sym = unlisp_rt::symbols::get_or_intern_symbol("-main".to_string());

        unsafe {
            if (*sym).function.is_null() {
                Err(error::Error::new_compilation_error(
                    "-main function is not defined",
                ))?;
            } else if (*(*sym).function).arg_count != 0 || (*(*sym).function).has_restarg {
                Err(error::Error::new_compilation_error(
                    "-main function should have zero arity",
                ))?;
            }
        }

        let i32_ty = self.llvm_ctx.i32_type();

        let main_fn_ty = i32_ty.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_ty, None);

        self.enter_fn_block(&main_fn);

        let init_rt_fn = self.lookup_known_fn("unlisp_rt_init_runtime");
        let code_init_fn = self.lookup_known_fn(&code_init_fn_name);

        self.builder.build_call(init_rt_fn, &[], "init_rt");
        self.builder.build_call(code_init_fn, &[], "init_code");

        let sym_name_ptr = self.str_literal_as_i8_ptr("-main");
        let intern_fn = self.lookup_known_fn("unlisp_rt_intern_sym");
        let interned_sym_ptr = self
            .builder
            .build_call(intern_fn, &[sym_name_ptr.into()], "main_symbol")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        let fn_obj_ptr_ptr = unsafe {
            self.builder
                .build_struct_gep(interned_sym_ptr, 1, "main_fn_obj_ptr_ptr")
        };

        let fn_obj_ptr = self
            .builder
            .build_load(fn_obj_ptr_ptr, "main_fn_obj_ptr")
            .into_pointer_value();

        let run_with_ex_handler_fn = self.lookup_known_fn("unlisp_rt_run_with_global_ex_handler");

        let ret_code = self
            .builder
            .build_call(run_with_ex_handler_fn, &[fn_obj_ptr.into()], "ret_code")
            .try_as_basic_value()
            .left()
            .unwrap();

        self.builder.build_return(Some(&ret_code));

        self.verify_or_panic(&main_fn, "main");

        Ok(())
    }

    pub unsafe fn compile_hirs(&mut self, hirs: &[HIR]) -> GenResult<CompiledFn> {
        let top_level_fn_name = self.codegen_hirs(hirs)?;

        Ok(self
            .execution_engine
            .get_function(top_level_fn_name.as_str())
            .expect("couldn't find top-level function in execution engine"))
    }
}