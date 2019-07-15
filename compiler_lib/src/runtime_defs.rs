use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::BasicType;
use inkwell::AddressSpace;

fn object_gen_def(context: &Context) {
    let int8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
    let int32_ty = context.i32_type();

    let struct_ty = context.opaque_struct_type("unlisp_rt_object");
    struct_ty.set_body(&[int32_ty.into(), int8_ptr_ty.into()], false);
}


fn list_gen_def(context: &Context, _module: &Module) {
    let i64_ty = context.i64_type();
    let i8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
    let struct_ty = context.opaque_struct_type("unlisp_rt_list");

    struct_ty.set_body(&[i8_ptr_ty.into(), i64_ty.into()], false);
}

fn symbol_gen_def(context: &Context, module: &Module) {
    let func_struct_ty = module
        .get_type("unlisp_rt_function")
        .unwrap()
        .into_struct_type();

    let name_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
    let func_ptr_ty = func_struct_ty.ptr_type(AddressSpace::Generic);

    let struct_ty = context.opaque_struct_type("unlisp_rt_symbol");

    struct_ty.set_body(&[name_ptr_ty.into(), func_ptr_ty.into()], false);
}

fn function_gen_def(context: &Context) {
        let fn_struct_ty = context.opaque_struct_type("unlisp_rt_function");

        let ty_ty = context.i32_type();
        let ty_name = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_arglist = context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .ptr_type(AddressSpace::Generic);
        let ty_arg_count = context.i64_type();
        let ty_is_macro = context.bool_type();
        let ty_invoke_f_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_apply_to_f_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_has_restarg = context.bool_type();

        fn_struct_ty.set_body(
            &[
                ty_ty.into(),
                ty_name.into(),
                ty_arglist.into(),
                ty_arg_count.into(),
                ty_is_macro.into(),
                ty_invoke_f_ptr.into(),
                ty_apply_to_f_ptr.into(),
                ty_has_restarg.into(),
            ],
            false,
        );
}

fn va_gen_def(ctx: &Context, module: &Module) {
    let i32_ty = ctx.i32_type();
    let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);

    let va_list_ty = ctx.opaque_struct_type("va_list");

    va_list_ty.set_body(
        &[
            i32_ty.into(),
            i32_ty.into(),
            i8_ptr_ty.into(),
            i8_ptr_ty.into(),
        ],
        false,
    );

    let va_start_end_ty = ctx.void_type().fn_type(&[i8_ptr_ty.into()], false);
    module.add_function("llvm.va_start", va_start_end_ty, Some(Linkage::External));
    module.add_function("llvm.va_end", va_start_end_ty, Some(Linkage::External));
}

fn unlisp_rt_intern_sym_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let sym_struct_ty = module.get_type("unlisp_rt_symbol").unwrap();
    let sym_struct_ptr_ty = sym_struct_ty
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let fn_type = sym_struct_ptr_ty.fn_type(&[arg_ty.into()], false);
    module.add_function("unlisp_rt_intern_sym", fn_type, Some(Linkage::External));
}

fn unlisp_rt_object_from_int_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i64_type();
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_int",
        fn_type,
        Some(Linkage::External),
    );
}

fn unlisp_rt_object_from_string_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_string",
        fn_type,
        Some(Linkage::External),
    );
}

fn unlisp_rt_int_from_obj_gen_def(ctx: &Context, module: &Module) {
    let i64_ty = ctx.i64_type();
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = i64_ty.fn_type(&[obj_struct_ty.into()], false);
    module.add_function("unlisp_rt_int_from_obj", fn_type, Some(Linkage::External));
}

fn unlisp_rt_object_from_function_gen_def(_: &Context, module: &Module) {
    let arg_ty = module
        .get_type("unlisp_rt_function")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_function",
        fn_type,
        Some(Linkage::External),
    );
}

fn unlisp_rt_object_from_symbol_gen_def(_: &Context, module: &Module) {
    let arg_ty = module
        .get_type("unlisp_rt_symbol")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_symbol",
        fn_type,
        Some(Linkage::External),
    );
}

fn unlisp_rt_object_from_list_gen_def(_: &Context, module: &Module) {
    let arg_ty = module.get_type("unlisp_rt_list").unwrap();

    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_list",
        fn_type,
        Some(Linkage::External),
    );
}

fn unlisp_rt_object_is_nil_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = module.get_type("unlisp_rt_object").unwrap();

    let fn_type = ctx.bool_type().fn_type(&[arg_ty.into()], false);
    module.add_function("unlisp_rt_object_is_nil", fn_type, Some(Linkage::External));
}

fn unlisp_rt_nil_object_gen_def(_ctx: &Context, module: &Module) {
    let obj_ty = module.get_type("unlisp_rt_object").unwrap();

    let fn_type = obj_ty.fn_type(&[], false);
    module.add_function("unlisp_rt_nil_object", fn_type, Some(Linkage::External));
}

fn unlisp_rt_check_arity_gen_def(ctx: &Context, module: &Module) {
    let bool_ty = ctx.bool_type();
    let fn_struct_ptr_ty = module
        .get_type("unlisp_rt_function")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let fn_ty = bool_ty.fn_type(&[fn_struct_ptr_ty.into(), ctx.i64_type().into()], false);
    module.add_function("unlisp_rt_check_arity", fn_ty, Some(Linkage::External));
}

fn unlisp_rt_va_list_into_list_gen_def(ctx: &Context, module: &Module) {
    let obj_ty = module.get_type("unlisp_rt_object").unwrap();
    let va_list_ty = module
        .get_type("va_list")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let i64_ty = ctx.i64_type();

    let fn_ty = obj_ty.fn_type(&[i64_ty.into(), va_list_ty.into()], false);

    module.add_function(
        "unlisp_rt_va_list_into_list",
        fn_ty,
        Some(Linkage::External),
    );
}

fn unlisp_rt_list_first_gen_def(_ctx: &Context, module: &Module) {
    let obj_ty = module.get_type("unlisp_rt_object").unwrap();
    let list_ty = module.get_type("unlisp_rt_list").unwrap();

    let fn_ty = obj_ty.fn_type(&[list_ty], false);

    module.add_function("unlisp_rt_list_first", fn_ty, Some(Linkage::External));
}

fn unlisp_rt_list_rest_gen_def(_ctx: &Context, module: &Module) {
    let list_ty = module.get_type("unlisp_rt_list").unwrap();

    let fn_ty = list_ty.fn_type(&[list_ty], false);

    module.add_function("unlisp_rt_list_rest", fn_ty, Some(Linkage::External));
}

fn unlisp_rt_list_cons_gen_def(_ctx: &Context, module: &Module) {
    let obj_ty = module.get_type("unlisp_rt_object").unwrap();
    let list_ty = module.get_type("unlisp_rt_list").unwrap();

    let fn_ty = list_ty.fn_type(&[obj_ty, list_ty], false);

    module.add_function("unlisp_rt_list_cons", fn_ty, Some(Linkage::External));
}

fn unlisp_rt_empty_list_gen_def(_ctx: &Context, module: &Module) {
    let list_ty = module.get_type("unlisp_rt_list").unwrap();

    let fn_type = list_ty.fn_type(&[], false);
    module.add_function("unlisp_rt_empty_list", fn_type, Some(Linkage::External));
}

fn unlisp_rt_init_runtime_gen_def(ctx: &Context, module: &Module) {
    let fn_type = ctx.void_type().fn_type(&[], false);
    module.add_function("unlisp_rt_init_runtime", fn_type, None);
}

fn raise_arity_error_gen_def(ctx: &Context, module: &Module) {
    let void_ty = ctx.void_type();
    let fn_ty = void_ty.fn_type(
        &[
            ctx.i8_type().ptr_type(AddressSpace::Generic).into(),
            ctx.i64_type().into(),
            ctx.i64_type().into(),
        ],
        false,
    );

    module.add_function("raise_arity_error", fn_ty, Some(Linkage::External));
}

fn raise_undef_fn_error_gen_def(ctx: &Context, module: &Module) {
    let void_ty = ctx.void_type();
    let fn_ty = void_ty.fn_type(
        &[ctx.i8_type().ptr_type(AddressSpace::Generic).into()],
        false,
    );

    module.add_function("raise_undef_fn_error", fn_ty, Some(Linkage::External));
}

pub fn gen_defs(ctx: &Context, module: &Module) {
    object_gen_def(ctx);
    list_gen_def(ctx, module);
    function_gen_def(ctx);
    symbol_gen_def(ctx, module);

    unlisp_rt_intern_sym_gen_def(ctx, module);
    unlisp_rt_object_from_int_gen_def(ctx, module);
    unlisp_rt_int_from_obj_gen_def(ctx, module);
    unlisp_rt_object_from_function_gen_def(ctx, module);
    unlisp_rt_object_from_symbol_gen_def(ctx, module);
    unlisp_rt_object_from_list_gen_def(ctx, module);
    unlisp_rt_object_from_string_gen_def(ctx, module);
    unlisp_rt_object_is_nil_gen_def(ctx, module);
    unlisp_rt_nil_object_gen_def(ctx, module);
    unlisp_rt_check_arity_gen_def(ctx, module);
    va_gen_def(ctx, module);
    unlisp_rt_va_list_into_list_gen_def(ctx, module);
    unlisp_rt_list_first_gen_def(ctx, module);
    unlisp_rt_list_rest_gen_def(ctx, module);
    unlisp_rt_list_cons_gen_def(ctx, module);
    unlisp_rt_empty_list_gen_def(ctx, module);
    unlisp_rt_init_runtime_gen_def(ctx, module);

    raise_arity_error_gen_def(ctx, module);
    raise_undef_fn_error_gen_def(ctx, module);
}
