use crate::object;
use crate::object::LispForm;
use crate::runtime;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

use std::iter;

fn compile_integer(ctx: &Context, module: &Module, builder: &Builder, i: i64) -> BasicValueEnum {
    let ty = ctx.i64_type();
    let int_val = ty.const_int(i as u64, true);
    let call = builder.build_call(
        module.get_function("unlisp_rt_object_from_int").unwrap(),
        &[int_val.into()],
        "call",
    );

    call.try_as_basic_value().left().unwrap()
}

fn compile_add(
    ctx: &Context,
    module: &Module,
    builder: &Builder,
    list: Vec<LispForm>,
) -> BasicValueEnum {
    let cast_fn = module.get_function("unlisp_rt_int_from_obj").unwrap();
    let compiled_arg1 = compile_form(ctx, module, builder, list[1].clone());
    let compiled_arg2 = compile_form(ctx, module, builder, list[2].clone());

    let cast_arg1 = builder.build_call(cast_fn, &[compiled_arg1], "cast1");
    let cast_arg1 = cast_arg1.try_as_basic_value().left().unwrap();

    let cast_arg2 = builder.build_call(cast_fn, &[compiled_arg2], "cast2");
    let cast_arg2 = cast_arg2.try_as_basic_value().left().unwrap();

    let sum = builder.build_int_add(
        cast_arg1.into_int_value(),
        cast_arg2.into_int_value(),
        "add",
    );

    let sum_packed = builder.build_call(
        module.get_function("unlisp_rt_object_from_int").unwrap(),
        &[sum.into()],
        "pack",
    );

    sum_packed.try_as_basic_value().left().unwrap()
}

fn codegen_fun(
    ctx: &Context,
    module: &Module,
    builder: &Builder,
    list: Vec<LispForm>,
) -> FunctionValue {
    let name = object::to_symbol(&list[1]);
    let arglist = object::to_list(&list[2]);
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();

    let arg_tys: Vec<_> = iter::repeat(obj_struct_ty).take(arglist.len()).collect();

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), false);
    let function = module.add_function(name, fn_ty, None);
    let basic_block = ctx.append_basic_block(&function, "entry");

    let prev_block = builder.get_insert_block();

    builder.position_at_end(&basic_block);

    let val = compile_form(&ctx, &module, &builder, list[3].clone());

    builder.build_return(Some(&val));

    function.verify(true);

    prev_block.as_ref().map(|b| builder.position_at_end(b));

    function
}

fn compile_defun(
    ctx: &Context,
    module: &Module,
    builder: &Builder,
    list: Vec<LispForm>,
) -> BasicValueEnum {
    let codegen_fun = codegen_fun(ctx, module, builder, list.clone());

    let name = object::to_symbol(&list[1]);
    let mut charcodes: Vec<_> = name.clone().chars().map(|c| c as u8).collect();
    charcodes.push(0);

    let array_ty = ctx.i8_type().array_type(charcodes.len() as u32);
    let global = module.add_global(array_ty, Some(AddressSpace::Const), name);

    let hello_ascii_vals: Vec<_> = charcodes
        .iter()
        .map(|v| ctx.i8_type().const_int((*v).into(), false))
        .collect();

    global.set_initializer(&ctx.i8_type().const_array(hello_ascii_vals.as_slice()));

    let global_ptr = unsafe {
        builder.build_gep(
            global.as_pointer_value(),
            &[
                ctx.i32_type().const_int(0, false),
                ctx.i32_type().const_int(0, false),
            ],
            "gep",
        )
    };

    let intern_fn = module.get_function("unlisp_rt_intern_sym").unwrap();
    let interned_sym_ptr = builder.build_call(intern_fn.clone(), &[global_ptr.into()], "call");

    let interned_sym_ptr = interned_sym_ptr.try_as_basic_value().left().unwrap();

    let fun_ptr = codegen_fun.as_global_value().as_pointer_value();
    let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let fun_ptr_cast = builder.build_bitcast(fun_ptr, i8_ptr_ty, "i8_fn_cast");

    let res = builder.build_call(
        module.get_function("unlisp_rt_set_fn_for_sym").unwrap(),
        &[fun_ptr_cast, interned_sym_ptr],
        "set_fn",
    );

    res.try_as_basic_value().left().unwrap()
}

fn compile_call(
    ctx: &Context,
    module: &Module,
    builder: &Builder,
    list: Vec<LispForm>,
) -> BasicValueEnum {
    let sym_name = object::to_symbol(&list[0]);
    let sym_name = module.get_global("foo.1").unwrap();

    let sym_name_ptr = unsafe {
        builder.build_gep(
            sym_name.as_pointer_value(),
            &[
                ctx.i32_type().const_int(0, false),
                ctx.i32_type().const_int(0, false),
            ],
            "gep",
        )
    };

    let intern_fn = module.get_function("unlisp_rt_intern_sym").unwrap();
    let interned_sym_ptr = builder.build_call(intern_fn, &[sym_name_ptr.into()], "call");

    let f_ptr = builder
        .build_call(
            module.get_function("unlisp_rt_f_ptr_from_sym").unwrap(),
            &[interned_sym_ptr.try_as_basic_value().left().unwrap()],
            "f_ptr",
        )
        .try_as_basic_value()
        .left()
        .unwrap();

    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_ty = obj_struct_ty.fn_type(&[], false);
    let fn_ty = fn_ty.ptr_type(AddressSpace::Generic);

    let f_ptr = builder.build_bitcast(f_ptr, fn_ty, "f_ptr_cast");

    builder
        .build_call(f_ptr.into_pointer_value(), &[], "f_ptr_call")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn compile_list_form(
    ctx: &Context,
    module: &Module,
    builder: &Builder,
    list: Vec<LispForm>,
) -> BasicValueEnum {
    let first = &list[0];

    println!("list form first {:?}", first);

    let val = match first {
        LispForm::Symbol(s) if *s == "add".to_string() => compile_add(ctx, module, builder, list),

        LispForm::Symbol(s) if *s == "defun".to_string() => {
            compile_defun(ctx, module, builder, list)
        }

        LispForm::Symbol(_) => compile_call(ctx, module, builder, list),

        _ => panic!("unsupported form: {:?}", &list),
    };

    val
}

pub fn compile_form(
    ctx: &Context,
    module: &Module,
    builder: &Builder,
    obj: LispForm,
) -> BasicValueEnum {
    match obj {
        LispForm::Integer(i) => compile_integer(ctx, &module, &builder, i),
        LispForm::List(l) => compile_list_form(ctx, module, builder, l.clone()),
        _ => panic!("unsuported form"),
    }
}

pub fn compile_toplevel(ctx: &Context, forms: Vec<LispForm>) -> Module {
    let module = ctx.create_module("repl");
    let builder = ctx.create_builder();

    runtime::init(ctx, &module);

    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_ty = obj_struct_ty.fn_type(&[], false);
    let function = module.add_function("__repl_form", fn_ty, None);
    let basic_block = ctx.append_basic_block(&function, "entry");
    builder.position_at_end(&basic_block);

    let mut val = None;

    for form in forms {
        val = Some(compile_form(&ctx, &module, &builder, form));
    }

    builder.build_return(Some(val.as_ref().unwrap()));

    function.verify(true);

    module
}
