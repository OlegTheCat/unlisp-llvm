use crate::object;
use crate::object::LispForm;
use crate::runtime;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, GlobalValue};
use inkwell::AddressSpace;
use std::collections::{HashMap, HashSet};

use std::iter;

pub struct CodegenContext<'a> {
    pub counter: i64,
    pub llvm_ctx: &'a Context,
    pub module: Module,
    pub builder: Builder,
    pub defined_sym_names: HashSet<String>,
    pub sym_names_globals: HashMap<String, GlobalValue>
}

impl<'a> CodegenContext<'a> {
    pub fn gen_unique_int(&mut self) -> i64 {
        self.counter += 1;
        self.counter
    }

    pub fn new(llvm_ctx: &'a Context) -> Self {
        let module = llvm_ctx.create_module("mod_0");
        let builder = llvm_ctx.create_builder();

        runtime::gen_runtime_defs(llvm_ctx, &module);

        Self {
            counter: 0,
            llvm_ctx: llvm_ctx,
            module: module,
            builder: builder,
            defined_sym_names: HashSet::new(),
            sym_names_globals: HashMap::new()
        }
    }

    pub fn reinitialize(&mut self) {
        let module = self
            .llvm_ctx
            .create_module(format!("mod_{}", self.gen_unique_int()).as_str());

        runtime::gen_runtime_defs(self.llvm_ctx, &module);

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

    pub fn get_or_globalize_sym_name(&mut self, name: impl Into<String>) -> GlobalValue {
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

    pub fn sym_name_as_i8_ptr(&mut self, name: impl Into<String>) -> BasicValueEnum {
        let global = self.get_or_globalize_sym_name(name);
        self.builder.build_bitcast(
            global.as_pointer_value(),
            self.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic),
            "sym_name_to_i8_ptr",
        )
    }

    pub fn lookup_known_type(&self, name: &str) -> BasicTypeEnum {
        self.module.get_type(name).expect(format!("known type {} not found", name).as_str())
    }

    pub fn lookup_known_fn(&self, name: &str) -> FunctionValue {
        self.module.get_function(name).expect(format!("known function {} not found", name).as_str())
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

fn compile_add(
    ctx: &mut CodegenContext,
    list: &[LispForm],
) -> BasicValueEnum {
    let cast_fn = ctx.lookup_known_fn("unlisp_rt_int_from_obj");
    let compiled_arg1 = compile_form(ctx, &list[1]);
    let compiled_arg2 = compile_form(ctx, &list[2]);

    let cast_arg1 = ctx.builder.build_call(cast_fn, &[compiled_arg1], "cast1");
    let cast_arg1 = cast_arg1.try_as_basic_value().left().unwrap();

    let cast_arg2 = ctx.builder.build_call(cast_fn, &[compiled_arg2], "cast2");
    let cast_arg2 = cast_arg2.try_as_basic_value().left().unwrap();

    let sum = ctx.builder.build_int_add(
        cast_arg1.into_int_value(),
        cast_arg2.into_int_value(),
        "add",
    );

    let sum_packed = ctx.builder.build_call(
        ctx.lookup_known_fn("unlisp_rt_object_from_int"),
        &[sum.into()],
        "pack",
    );

    sum_packed.try_as_basic_value().left().unwrap()
}

fn codegen_fun(
    ctx: &mut CodegenContext,
    list: &[LispForm],
) -> FunctionValue {
    let name = object::to_symbol(&list[1]);
    let arglist = object::to_list(&list[2]);
    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");

    let arg_tys: Vec<_> = iter::repeat(obj_struct_ty).take(arglist.len()).collect();

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), false);
    let function = ctx.module.add_function(name, fn_ty, None);
    let basic_block = ctx.llvm_ctx.append_basic_block(&function, "entry");

    let prev_block = ctx.builder.get_insert_block();

    ctx.builder.position_at_end(&basic_block);

    let val = compile_forms(ctx, &list[3..]);

    ctx.builder.build_return(Some(&val));

    function.verify(true);

    prev_block.as_ref().map(|b| ctx.builder.position_at_end(b));

    function
}

fn compile_defun(
    ctx: &mut CodegenContext,
    list: &[LispForm],
) -> BasicValueEnum {
    let codegen_fun = codegen_fun(ctx, list);

    let name = object::to_symbol(&list[1]);
    let sym_ptr = ctx.sym_name_as_i8_ptr(name.clone());

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
    let interned_sym_ptr = ctx.builder.build_call(intern_fn.clone(), &[sym_ptr.into()], "call");
    let interned_sym_ptr = interned_sym_ptr.try_as_basic_value().left().unwrap();

    let fun_ptr = codegen_fun.as_global_value().as_pointer_value();
    let i8_ptr_ty = ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic);
    let fun_ptr_cast = ctx.builder.build_bitcast(fun_ptr, i8_ptr_ty, "i8_fn_cast");

    let res = ctx.builder.build_call(
        ctx.lookup_known_fn("unlisp_rt_set_fn_for_sym"),
        &[fun_ptr_cast, interned_sym_ptr],
        "set_fn",
    );

    res.try_as_basic_value().left().unwrap()
}

fn compile_call(
    ctx: &mut CodegenContext,
    list: &[LispForm],
) -> BasicValueEnum {
    let sym_name = object::to_symbol(&list[0]);
    let sym_name_ptr = ctx.sym_name_as_i8_ptr(sym_name.clone());

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
    let interned_sym_ptr = ctx.builder.build_call(intern_fn, &[sym_name_ptr.into()], "call");

    let f_ptr = ctx.builder
        .build_call(
            ctx.lookup_known_fn("unlisp_rt_f_ptr_from_sym"),
            &[interned_sym_ptr.try_as_basic_value().left().unwrap()],
            "f_ptr",
        )
        .try_as_basic_value()
        .left()
        .unwrap();

    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");
    let fn_ty = obj_struct_ty.fn_type(&[], false);
    let fn_ty = fn_ty.ptr_type(AddressSpace::Generic);

    let f_ptr = ctx.builder.build_bitcast(f_ptr, fn_ty, "f_ptr_cast");

    ctx.builder
        .build_call(f_ptr.into_pointer_value(), &[], "f_ptr_call")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn compile_list_form(
    ctx: &mut CodegenContext,
    list: &[LispForm],
) -> BasicValueEnum {
    let first = &list[0];

    let val = match first {
        LispForm::Symbol(s) if *s == "add".to_string() => compile_add(ctx, list),

        LispForm::Symbol(s) if *s == "defun".to_string() => {
            compile_defun(ctx, list)
        }

        LispForm::Symbol(_) => compile_call(ctx, list),

        _ => panic!("unsupported form: {:?}", &list),
    };

    val
}

fn compile_forms(ctx: &mut CodegenContext, forms: &[LispForm]) -> BasicValueEnum {
    let mut val = None;

    for form in forms {
        val = Some(compile_form(ctx, form));
    }

    val.unwrap()
}

pub fn compile_form(
    ctx: &mut CodegenContext,
    obj: &LispForm,
) -> BasicValueEnum {
    match obj {
        LispForm::Integer(i) => compile_integer(ctx, *i),
        LispForm::List(list) => compile_list_form(ctx, list),
        _ => panic!("unsuported form"),
    }
}

pub fn compile_toplevel(ctx: &mut CodegenContext, forms: &[LispForm]) {
    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");
    let fn_ty = obj_struct_ty.fn_type(&[], false);
    let function = ctx.module.add_function("__repl_form", fn_ty, None);
    let basic_block = ctx.llvm_ctx.append_basic_block(&function, "entry");
    ctx.builder.position_at_end(&basic_block);

    let val = compile_forms(ctx, forms);

    ctx.builder.build_return(Some(&val));

    function.verify(true);
}
