use crate::repr::*;
use crate::runtime;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue};
use inkwell::{AddressSpace, IntPredicate};
use std::collections::{HashMap, HashSet};

use std::error::Error;
use std::fmt;
use std::iter;
use std::rc::Rc;

type GenResult<T> = Result<T, Box<Error>>;
type CompileResult = GenResult<BasicValueEnum>;

#[derive(Debug, Clone)]
pub struct UndefinedSymbol {
    symbol_name: String,
}

impl UndefinedSymbol {
    pub fn new(symbol_name: impl Into<String>) -> Self {
        Self {
            symbol_name: symbol_name.into(),
        }
    }
}

impl fmt::Display for UndefinedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "undefined {} {}", "symbol", self.symbol_name)
    }
}

impl Error for UndefinedSymbol {}

pub struct CodegenContext<'a> {
    counter: u64,
    llvm_ctx: &'a Context,
    module: Module,
    builder: Builder,
    blocks_stack: Vec<Rc<BasicBlock>>,
    envs: Vec<HashMap<String, BasicValueEnum>>,
    defined_sym_names: HashSet<String>,
    sym_names_globals: HashMap<String, GlobalValue>,
}

impl<'a> CodegenContext<'a> {
    fn gen_unique_int(&mut self) -> u64 {
        self.counter += 1;
        self.counter
    }

    fn mangle_str(&mut self, s: impl Into<String>) -> String {
        format!("{}__unlisp_{}", s.into(), self.gen_unique_int())
    }

    pub fn new(llvm_ctx: &'a Context) -> Self {
        let module = llvm_ctx.create_module("mod_0");
        let builder = llvm_ctx.create_builder();

        runtime::defs::gen_defs(llvm_ctx, &module);

        Self {
            counter: 0,
            llvm_ctx: llvm_ctx,
            module: module,
            builder: builder,
            blocks_stack: vec![],
            envs: vec![],
            defined_sym_names: HashSet::new(),
            sym_names_globals: HashMap::new(),
        }
    }

    pub fn reinitialize(&mut self) {
        let module = self
            .llvm_ctx
            .create_module(format!("mod_{}", self.gen_unique_int()).as_str());

        runtime::defs::gen_defs(self.llvm_ctx, &module);

        self.blocks_stack = vec![];
        self.module = module;
        self.sym_names_globals = HashMap::new();
    }

    fn declare_sym_name(&mut self, name: String) -> GlobalValue {
        let array_ty = self.llvm_ctx.i8_type().array_type((name.len() + 1) as u32);
        let global = self.module.add_global(array_ty, None, name.as_str());
        self.sym_names_globals.insert(name, global.clone());

        global
    }

    fn define_sym_name(&mut self, name: String) -> GlobalValue {
        let mut charcodes: Vec<_> = name.clone().chars().map(|c| c as u8).collect();
        charcodes.push(0);

        let array_ty = self.llvm_ctx.i8_type().array_type(charcodes.len() as u32);
        let array_vals: Vec<_> = charcodes
            .iter()
            .map(|v| self.llvm_ctx.i8_type().const_int((*v).into(), false))
            .collect();

        let global = self.module.add_global(array_ty, None, name.as_str());
        global.set_initializer(&self.llvm_ctx.i8_type().const_array(array_vals.as_slice()));

        self.defined_sym_names.insert(name.clone());
        self.sym_names_globals.insert(name.clone(), global.clone());

        global
    }

    fn get_or_globalize_sym_name(&mut self, name: impl Into<String>) -> GlobalValue {
        let name = name.into();
        if self.defined_sym_names.get(&name).is_some() {
            if let Some(g_val) = self.sym_names_globals.get(&name) {
                g_val.clone()
            } else {
                self.declare_sym_name(name)
            }
        } else {
            self.define_sym_name(name)
        }
    }

    fn name_as_i8_ptr(&mut self, name: impl Into<String>) -> BasicValueEnum {
        let global = self.get_or_globalize_sym_name(name);
        self.builder.build_bitcast(
            global.as_pointer_value(),
            self.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic),
            "sym_name_to_i8_ptr",
        )
    }

    fn lookup_known_type(&self, name: &str) -> BasicTypeEnum {
        self.module
            .get_type(name)
            .expect(format!("known type {} not found", name).as_str())
    }

    fn lookup_known_fn(&self, name: &str) -> FunctionValue {
        self.module
            .get_function(name)
            .expect(format!("known function {} not found", name).as_str())
    }

    pub fn get_module(&self) -> &Module {
        &self.module
    }

    fn compile_top_level_hir(&mut self, hirs: &[HIR]) -> GenResult<String> {
        let obj_struct_ty = self.lookup_known_type("unlisp_rt_object");
        let fn_ty = obj_struct_ty.fn_type(&[], false);
        let fn_name = self.mangle_str("__repl_form");
        let function = self.module.add_function(&fn_name, fn_ty, None);

        self.enter_fn_block(&function);

        // let buf_ptr = self
        //     .builder
        //     .build_alloca(self.lookup_known_type("setjmp_buf"), "setjmp_buf");
        // let setjmp = self
        //     .builder
        //     .build_call(self.lookup_known_fn("setjmp"), &[buf_ptr.into()], "setjmp")
        //     .try_as_basic_value()
        //     .left()
        //     .unwrap()
        //     .into_int_value();

        // let ok_block = self.llvm_ctx.append_basic_block(&function, "ok");
        // let err_block = self.llvm_ctx.append_basic_block(&function, "err");

        // let br = self
        //     .builder
        //     .build_conditional_branch(setjmp, &err_block, &ok_block);

        // self.builder.position_at_end(&ok_block);
        // self.blocks_stack.push(Rc::new(ok_block));

        let val = compile_hirs(self, hirs)?;

        // self.builder.build_call(
        //     self.lookup_known_fn("longjmp"),
        //     &[
        //         buf_ptr.into(),
        //         self.llvm_ctx.i32_type().const_int(1, false).into(),
        //     ],
        //     "longjmp",
        // );

        self.builder.build_return(Some(&val));

        // self.blocks_stack.pop();
        // self.builder.position_at_end(&err_block);
        // let val = compile_hirs(self, &[HIR::Literal(Literal::IntegerLiteral(2))])?;

        // self.builder.build_return(Some(&val));

        function.verify(true);

        Ok(fn_name)
    }

    pub fn compile_top_level(&mut self, form: &Form) -> GenResult<String> {
        let hir = form_to_hir_with_transforms(form)?;
        self.compile_top_level_hir(&[hir])
    }

    fn push_env(&mut self) {
        self.envs.push(HashMap::new())
    }

    fn save_env_mapping(&mut self, name: String, val: BasicValueEnum) {
        let len = self.envs.len();
        self.envs[len - 1].insert(name, val);
    }

    fn lookup_name(&self, name: &String) -> Option<BasicValueEnum> {
        for env in self.envs.iter().rev() {
            if let Some(val) = env.get(name) {
                return Some(val.clone());
            }
        }

        None
    }

    fn pop_env(&mut self) {
        self.envs.pop();
    }

    fn replace_cur_block(&mut self, block: BasicBlock) -> Rc<BasicBlock> {
        let rc = Rc::new(block);
        let idx = self.blocks_stack.len() - 1;
        self.blocks_stack[idx] = rc.clone();
        self.builder.position_at_end(&rc);
        rc
    }

    fn cur_block(&self) -> Rc<BasicBlock> {
        self.blocks_stack.last().cloned().expect("no current block")
    }

    fn append_block(&self) -> BasicBlock {
        let function = self.cur_block()
            .get_parent()
            .unwrap();

        self.llvm_ctx.append_basic_block(&function, "entry")
    }

    fn enter_block(&mut self) -> Rc<BasicBlock> {
        let function = self.cur_block()
            .get_parent()
            .unwrap();

        self.enter_fn_block(&function)
    }

    fn enter_fn_block(&mut self, function: &FunctionValue) -> Rc<BasicBlock> {
        let block = self.llvm_ctx.append_basic_block(&function, "entry");
        let block_rc = Rc::new(block);

        self.builder.position_at_end(&block_rc);
        self.blocks_stack.push(block_rc.clone());
        block_rc
    }

    fn exit_block(&mut self) -> Rc<BasicBlock> {
        let block = self.blocks_stack.pop().expect("no block to exit");
        self.builder
            .position_at_end(self.blocks_stack.last().unwrap());
        block
    }
}

fn compile_integer(ctx: &mut CodegenContext, i: i64) -> BasicValueEnum {
    let ty = ctx.llvm_ctx.i64_type();
    let int_val = ty.const_int(i as u64, true);
    let call = ctx.builder.build_call(
        ctx.lookup_known_fn("unlisp_rt_object_from_int"),
        &[int_val.into()],
        "call",
    );

    call.try_as_basic_value().left().unwrap()
}

// fn codegen_fun(ctx: &mut CodegenContext, list: &[LispForm]) -> FunctionValue {
//     let name = object::to_symbol(&list[1]);
//     let arglist = object::to_list(&list[2]);
//     let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");

//     let arg_tys: Vec<_> = iter::repeat(obj_struct_ty).take(arglist.len()).collect();

//     let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), false);
//     let fn_name = ctx.mangle_str(name.clone());
//     let function = ctx.module.add_function(&fn_name, fn_ty, None);

//     ctx.push_env();

//     for (arg, arg_name) in function.get_param_iter().zip(arglist.iter()) {
//         let arg_name = object::to_symbol(arg_name);
//         arg.as_struct_value().set_name(arg_name);
//         ctx.save_env_mapping(arg_name.clone(), arg);
//     }

//     let basic_block = ctx.llvm_ctx.append_basic_block(&function, "entry");

//     let prev_block = ctx.builder.get_insert_block();

//     ctx.builder.position_at_end(&basic_block);

//     let val = compile_forms(ctx, &list[3..]);

//     ctx.builder.build_return(Some(&val));

//     function.verify(true);

//     prev_block.as_ref().map(|b| ctx.builder.position_at_end(b));

//     ctx.pop_env();

//     function
// }

// fn compile_defun(ctx: &mut CodegenContext, list: &[LispForm]) -> BasicValueEnum {
//     let codegen_fun = codegen_fun(ctx, list);

//     let name = object::to_symbol(&list[1]);
//     let sym_ptr = ctx.name_as_i8_ptr(name.clone());

//     let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
//     let interned_sym_ptr = ctx
//         .builder
//         .build_call(intern_fn.clone(), &[sym_ptr.into()], "call");
//     let interned_sym_ptr = interned_sym_ptr.try_as_basic_value().left().unwrap();

//     let fun_ptr = codegen_fun.as_global_value().as_pointer_value();
//     let i8_ptr_ty = ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic);
//     let fun_ptr_cast = ctx.builder.build_bitcast(fun_ptr, i8_ptr_ty, "i8_fn_cast");

//     let res = ctx.builder.build_call(
//         ctx.lookup_known_fn("unlisp_rt_set_fn_for_sym"),
//         &[fun_ptr_cast, interned_sym_ptr],
//         "set_fn",
//     );

//     res.try_as_basic_value().left().unwrap()
// }

fn compile_call(ctx: &mut CodegenContext, call: &Call) -> CompileResult {
    let sym_name_ptr = ctx.name_as_i8_ptr(call.fn_name.as_str());

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
    let interned_sym_ptr = ctx
        .builder
        .build_call(intern_fn, &[sym_name_ptr.into()], "intern")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let function_ptr_ptr = unsafe {
        ctx.builder
            .build_struct_gep(interned_sym_ptr, 1, "sym_fn_gep")
    };

    let function_ptr = ctx
        .builder
        .build_load(function_ptr_ptr, "fun_ptr")
        .into_pointer_value();

    let invoke_ptr_ptr = unsafe { ctx.builder.build_struct_gep(function_ptr, 5, "invoke_gep") };

    let invoke_ptr = ctx
        .builder
        .build_load(invoke_ptr_ptr, "invoke_ptr")
        .into_pointer_value();

    let function_ptr_ty = ctx
        .lookup_known_type("unlisp_rt_function")
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let object_ty = ctx.lookup_known_type("unlisp_rt_object");
    let mut arg_tys: Vec<_> = iter::repeat(object_ty).take(call.args.len()).collect();
    arg_tys.push(function_ptr_ty.into());
    arg_tys.reverse();

    let invoke_fn_ty = object_ty.fn_type(arg_tys.as_slice(), false);
    let invoke_fn_ptr_ty = invoke_fn_ty.ptr_type(AddressSpace::Generic);

    let invoke_ptr = ctx
        .builder
        .build_bitcast(invoke_ptr, invoke_fn_ptr_ty, "fn_ptr_cast");

    let mut compiled_args = call
        .args
        .iter()
        .map(|arg| compile_hir(ctx, arg))
        .collect::<Result<Vec<_>, _>>()?;

    compiled_args.reverse();
    compiled_args.push(function_ptr.into());
    compiled_args.reverse();

    Ok(ctx
        .builder
        .build_call(
            invoke_ptr.into_pointer_value(),
            compiled_args.as_slice(),
            "invoke_call",
        )
        .try_as_basic_value()
        .left()
        .unwrap())
}

fn codegen_raw_fn(ctx: &mut CodegenContext, closure: &Closure) -> GenResult<FunctionValue> {
    let fn_name = closure
        .lambda
        .name
        .as_ref()
        .map_or("lambda", |n| n.as_str());
    let fn_name = ctx.mangle_str(fn_name);

    let pars_count = closure.free_vars.len() + closure.lambda.arglist.len();
    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");

    let arg_tys: Vec<_> = iter::repeat(obj_struct_ty).take(pars_count).collect();

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), false);
    let function = ctx.module.add_function(&fn_name, fn_ty, None);

    ctx.push_env();
    ctx.enter_fn_block(&function);

    let args_iter = closure
        .free_vars
        .iter()
        .chain(closure.lambda.arglist.iter());

    for (arg, arg_name) in function.get_param_iter().zip(args_iter) {
        arg.as_struct_value().set_name(arg_name);
        ctx.save_env_mapping(arg_name.clone(), arg);
    }

    let val = compile_hirs(ctx, closure.lambda.body.as_slice())?;

    ctx.builder.build_return(Some(&val));

    function.verify(true);

    ctx.exit_block();
    ctx.pop_env();

    Ok(function)
}

fn codegen_closure_struct(ctx: &mut CodegenContext, closure: &Closure) -> StructType {
    let struct_name = closure.lambda.name.as_ref().map_or_else(
        || "closure_struct".to_string(),
        |n| format!("{}_closure_struct", n),
    );

    let struct_name = ctx.mangle_str(struct_name);

    let struct_ty = ctx.llvm_ctx.opaque_struct_type(struct_name.as_str());

    let ty_ty = ctx.llvm_ctx.i32_type();
    let ty_name = ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic);
    let ty_arglist = ctx
        .llvm_ctx
        .i8_type()
        .ptr_type(AddressSpace::Generic)
        .ptr_type(AddressSpace::Generic);
    let ty_arg_count = ctx.llvm_ctx.i64_type();
    let ty_is_macro = ctx.llvm_ctx.bool_type();
    let ty_invoke_f_ptr = ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic);
    let ty_apply_to_f_ptr = ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic);

    let mut body_tys = vec![
        ty_ty.into(),
        ty_name.into(),
        ty_arglist.into(),
        ty_arg_count.into(),
        ty_is_macro.into(),
        ty_invoke_f_ptr.into(),
        ty_apply_to_f_ptr.into(),
    ];

    let object_ty = ctx.lookup_known_type("unlisp_rt_object");

    for _ in closure.free_vars.iter() {
        body_tys.push(object_ty.clone().into());
    }

    struct_ty.set_body(body_tys.as_slice(), false);

    struct_ty
}

fn codegen_invoke_fn(
    ctx: &mut CodegenContext,
    closure: &Closure,
    struct_ty: StructType,
    raw_fn: FunctionValue,
) -> FunctionValue {
    let fn_name = closure
        .lambda
        .name
        .as_ref()
        .map_or_else(|| "invoke_closure".to_string(), |n| format!("invoke_{}", n));

    let fn_name = ctx.mangle_str(fn_name);

    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");

    let mut arg_tys: Vec<_> = iter::repeat(obj_struct_ty)
        .take(closure.lambda.arglist.len())
        .collect();
    arg_tys.push(struct_ty.ptr_type(AddressSpace::Generic).into());
    arg_tys.reverse();

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), false);
    let function = ctx.module.add_function(&fn_name, fn_ty, None);

    ctx.enter_fn_block(&function);

    let mut par_iter = function.get_param_iter();
    let struct_ptr_par = par_iter.next().unwrap().into_pointer_value();
    struct_ptr_par.set_name("fn_obj");

    let mut raw_fn_args = vec![];

    for (i, _) in closure.free_vars.iter().enumerate() {
        let arg_ptr = unsafe {
            ctx.builder
                .build_struct_gep(struct_ptr_par, 7 + i as u32, "free_var_ptr")
        };
        let arg = ctx.builder.build_load(arg_ptr, "free_var");
        raw_fn_args.push(arg);
    }

    for (par, name) in par_iter.zip(closure.lambda.arglist.iter()) {
        par.as_struct_value().set_name(name);
        raw_fn_args.push(par);
    }

    let raw_call = ctx
        .builder
        .build_call(raw_fn, raw_fn_args.as_slice(), "raw_fn_call")
        .try_as_basic_value()
        .left()
        .unwrap();

    ctx.builder.build_return(Some(&raw_call));

    function.verify(true);

    ctx.exit_block();

    function
}

fn compile_closure(ctx: &mut CodegenContext, closure: &Closure) -> CompileResult {
    let raw_fn = codegen_raw_fn(ctx, closure)?;
    let struct_ty = codegen_closure_struct(ctx, closure);
    let invoke_fn = codegen_invoke_fn(ctx, closure, struct_ty, raw_fn);

    let struct_ptr_ty = struct_ty.ptr_type(AddressSpace::Generic);
    let struct_ptr_null = struct_ptr_ty.const_null();

    let size = unsafe {
        ctx.builder.build_gep(
            struct_ptr_null,
            &[ctx.llvm_ctx.i32_type().const_int(1, false)],
            "size",
        )
    };

    let size = ctx
        .builder
        .build_ptr_to_int(size, ctx.llvm_ctx.i32_type(), "size_i32");

    let malloc = ctx.lookup_known_fn("malloc");
    let struct_ptr = ctx
        .builder
        .build_call(malloc, &[size.into()], "malloc")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();
    let struct_ptr = ctx
        .builder
        .build_bitcast(struct_ptr, struct_ptr_ty, "closure_ptr")
        .into_pointer_value();

    let struct_ty_ptr = unsafe { ctx.builder.build_struct_gep(struct_ptr, 0, "ty_ptr") };

    ctx.builder
        .build_store(struct_ty_ptr, ctx.llvm_ctx.i32_type().const_int(1, false));

    let name = closure
        .lambda
        .name
        .as_ref()
        .map_or("lambda", |n| n.as_str());
    let name_ptr = ctx.name_as_i8_ptr(name);

    let struct_name_ptr = unsafe { ctx.builder.build_struct_gep(struct_ptr, 1, "name_ptr") };
    ctx.builder.build_store(struct_name_ptr, name_ptr);

    //TODO: arglist

    let struct_arg_count_ptr =
        unsafe { ctx.builder.build_struct_gep(struct_ptr, 3, "arg_count_ptr") };
    ctx.builder.build_store(
        struct_arg_count_ptr,
        ctx.llvm_ctx
            .i64_type()
            .const_int(closure.lambda.arglist.len() as u64, false),
    );

    let struct_is_macro_ptr =
        unsafe { ctx.builder.build_struct_gep(struct_ptr, 4, "is_macro_ptr") };
    ctx.builder.build_store(
        struct_is_macro_ptr,
        ctx.llvm_ctx.bool_type().const_int(0, false),
    );

    let struct_invoke_fn_ptr = unsafe { ctx.builder.build_struct_gep(struct_ptr, 5, "invoke_ptr") };

    let invoke_fn_cast = ctx.builder.build_bitcast(
        invoke_fn.as_global_value().as_pointer_value(),
        ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic),
        "cast_invoke",
    );

    ctx.builder
        .build_store(struct_invoke_fn_ptr, invoke_fn_cast);

    for (i, var) in closure.free_vars.iter().enumerate() {
        let var_val = ctx
            .lookup_name(var)
            .ok_or_else(|| UndefinedSymbol::new(var.as_str()))?;

        let free_var_ptr = unsafe {
            ctx.builder
                .build_struct_gep(struct_ptr, 7 + i as u32, "free_var_ptr")
        };
        ctx.builder.build_store(free_var_ptr, var_val);
    }

    let struct_ptr_cast = ctx.builder.build_bitcast(
        struct_ptr,
        ctx.lookup_known_type("unlisp_rt_function")
            .as_struct_type()
            .ptr_type(AddressSpace::Generic),
        "function_obj_ptr",
    );

    let object = ctx
        .builder
        .build_call(
            ctx.lookup_known_fn("unlisp_rt_object_from_function"),
            &[struct_ptr_cast.into()],
            "object_from_fn",
        )
        .try_as_basic_value()
        .left()
        .unwrap();

    Ok(object)
}

fn compile_nil_literal(ctx: &CodegenContext) -> BasicValueEnum {
    ctx.builder
        .build_call(ctx.lookup_known_fn("unlisp_rt_nil_object"), &[], "nil_obj")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn compile_literal(ctx: &mut CodegenContext, literal: &Literal) -> CompileResult {
    match literal {
        Literal::ListLiteral(vec) if vec.is_empty() => Ok(compile_nil_literal(ctx)),
        Literal::IntegerLiteral(i) => Ok(compile_integer(ctx, *i)),
        Literal::SymbolLiteral(s) => {
            let val = ctx
                .lookup_name(s)
                .ok_or_else(|| UndefinedSymbol::new(s.as_str()))?;
            Ok(val)
        }
        _ => panic!("unsupported literal"),
    }
}

fn compile_quoted_symbol(ctx: &mut CodegenContext, name: &String) -> BasicValueEnum {
    let sym_name_ptr = ctx.name_as_i8_ptr(name.as_str());

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
    let interned_sym_ptr = ctx
        .builder
        .build_call(intern_fn, &[sym_name_ptr.into()], "intern")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_object_from_symbol");

    ctx.builder
        .build_call(intern_fn, &[interned_sym_ptr.into()], "object")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn compile_quoted_literal(ctx: &mut CodegenContext, literal: &Literal) -> CompileResult {
    match literal {
        Literal::SymbolLiteral(s) => Ok(compile_quoted_symbol(ctx, s)),
        _ => unimplemented!(),
    }
}

fn compile_if(ctx: &mut CodegenContext, if_hir: &If) -> CompileResult {
    let merge_block = ctx.append_block();

    let compiled_cond = compile_hir(ctx, &if_hir.cond)?;

    let enter_then_block = ctx.enter_block();
    let compiled_then = compile_hir(ctx, &if_hir.then_hir)?;
    ctx.builder.build_unconditional_branch(&merge_block);
    let exit_then_block = ctx.exit_block();


    let enter_else_block = ctx.enter_block();
    let compiled_else = match if_hir.else_hir.as_ref() {
        Some(hir) => compile_hir(ctx, hir)?,
        None => compile_nil_literal(ctx)
    };
    ctx.builder.build_unconditional_branch(&merge_block);
    let exit_else_block = ctx.exit_block();

    let is_nil = ctx
        .builder
        .build_call(
            ctx.lookup_known_fn("unlisp_rt_object_is_nil"),
            &[compiled_cond.into()],
            "is_nil",
        )
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value();

    ctx.builder
        .build_conditional_branch(is_nil, &enter_else_block, &enter_then_block);

    ctx.replace_cur_block(merge_block);

    let phi = ctx
        .builder
        .build_phi(ctx.lookup_known_type("unlisp_rt_object"), "phi");
    phi.add_incoming(&[(&compiled_then, &exit_then_block), (&compiled_else, &exit_else_block)]);

    Ok(phi.as_basic_value())
}

fn compile_hir(ctx: &mut CodegenContext, hir: &HIR) -> CompileResult {
    match hir {
        HIR::Literal(literal) => compile_literal(ctx, literal),
        HIR::Call(call) => compile_call(ctx, call),
        HIR::Closure(closure) => compile_closure(ctx, closure),
        HIR::Lambda(_) => panic!("cannot compile raw lambda"),
        HIR::If(if_hir) => compile_if(ctx, if_hir),
        HIR::Quote(quote) => compile_quoted_literal(ctx, &quote.body),
        _ => panic!("unsupported HIR"),
    }
}

fn compile_hirs(ctx: &mut CodegenContext, hirs: &[HIR]) -> CompileResult {
    let mut val_opt = None;

    for hir in hirs {
        val_opt = Some(compile_hir(ctx, hir)?);
    }

    let val_or_nil = val_opt.unwrap_or_else(|| compile_nil_literal(ctx));

    Ok(val_or_nil)
}
