use crate::repr::Closure;
use crate::runtime::defs::Function;

use inkwell::types::{BasicType, StructType};
use inkwell::values::{BasicValueEnum, FunctionValue};
use inkwell::AddressSpace;

use super::common::*;
use super::context::CodegenContext;
use super::top_level::compile_hirs;

use std::iter;

fn codegen_raw_fn(ctx: &mut CodegenContext, closure: &Closure) -> GenResult<FunctionValue> {
    let fn_name = closure
        .lambda
        .name
        .as_ref()
        .map_or("lambda", |n| n.as_str());
    let fn_name = ctx.mangle_str(fn_name);

    let mut arity = closure.free_vars.len() + closure.lambda.arglist.len();

    if closure.lambda.restarg.is_some() {
        arity += 1;
    }

    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");

    let arg_tys: Vec<_> = iter::repeat(obj_struct_ty).take(arity).collect();

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), false);
    let function = ctx.get_module().add_function(&fn_name, fn_ty, None);

    ctx.push_env();
    ctx.enter_fn_block(&function);

    let args_iter = closure
        .free_vars
        .iter()
        .chain(closure.lambda.arglist.iter())
        .chain(closure.lambda.restarg.iter());

    let param_iter = function.get_param_iter();

    for (arg, arg_name) in param_iter.zip(args_iter) {
        arg.as_struct_value().set_name(arg_name);
        ctx.save_env_mapping(arg_name.clone(), arg);
    }

    let val = compile_hirs(ctx, closure.lambda.body.as_slice())?;

    ctx.builder.build_return(Some(&val));

    ctx.verify_or_panic(&function, "raw");
    ctx.pass_manager.run_on(&function);

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
    let ty_has_restarg = ctx.llvm_ctx.bool_type();

    let mut body_tys = vec![
        ty_ty.into(),
        ty_name.into(),
        ty_arglist.into(),
        ty_arg_count.into(),
        ty_is_macro.into(),
        ty_invoke_f_ptr.into(),
        ty_apply_to_f_ptr.into(),
        ty_has_restarg.into(),
    ];

    let object_ty = ctx.lookup_known_type("unlisp_rt_object");

    for _ in closure.free_vars.iter() {
        body_tys.push(object_ty.clone().into());
    }

    struct_ty.set_body(body_tys.as_slice(), false);

    struct_ty
}

fn codegen_valist_into_list_conversion(
    ctx: &mut CodegenContext,
    n_varargs: BasicValueEnum,
) -> BasicValueEnum {
    let va_start = ctx.lookup_known_fn("llvm.va_start");
    let va_end = ctx.lookup_known_fn("llvm.va_end");
    let va_list_ty = ctx.lookup_known_type("va_list");

    let va_list_ptr = ctx.builder.build_alloca(va_list_ty, "va_list_ptr");
    let va_list_i8_ptr = ctx.builder.build_bitcast(
        va_list_ptr,
        ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic),
        "va_list_i8_ptr",
    );
    ctx.builder
        .build_call(va_start, &[va_list_i8_ptr.into()], "va_start_call");

    let convert = ctx.lookup_known_fn("unlisp_rt_va_list_into_list");
    let list = ctx
        .builder
        .build_call(convert, &[n_varargs, va_list_ptr.into()], "list")
        .try_as_basic_value()
        .left()
        .unwrap();

    ctx.builder
        .build_call(va_end, &[va_list_i8_ptr.into()], "va_end_call");

    list
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
    let has_restarg = closure.lambda.restarg.is_some();

    let mut arg_tys: Vec<_> = iter::repeat(obj_struct_ty)
        .take(closure.lambda.arglist.len())
        .collect();

    if has_restarg {
        arg_tys.push(ctx.llvm_ctx.i64_type().into());
    }

    arg_tys.push(struct_ty.ptr_type(AddressSpace::Generic).into());
    arg_tys.reverse();

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), has_restarg);
    let function = ctx.get_module().add_function(&fn_name, fn_ty, None);

    ctx.enter_fn_block(&function);

    let mut par_iter = function.get_param_iter();
    let struct_ptr_par = par_iter.next().unwrap().into_pointer_value();
    struct_ptr_par.set_name("fn_obj");

    let mut n_vararg = None;

    if has_restarg {
        n_vararg = Some(par_iter.next().unwrap());
        n_vararg.unwrap().into_int_value().set_name("n_vararg");
    }

    let mut raw_fn_args = vec![];

    for (i, _) in closure.free_vars.iter().enumerate() {
        let arg_ptr = unsafe {
            ctx.builder.build_struct_gep(
                struct_ptr_par,
                Function::FIELDS_COUNT + i as u32,
                "free_var_ptr",
            )
        };
        let arg = ctx.builder.build_load(arg_ptr, "free_var");
        raw_fn_args.push(arg);
    }

    let args_iter = closure
        .lambda
        .arglist
        .iter()
        .chain(closure.lambda.restarg.iter());

    for (par, name) in par_iter.zip(args_iter) {
        par.as_struct_value().set_name(name);
        raw_fn_args.push(par);
    }

    if has_restarg {
        raw_fn_args.push(codegen_valist_into_list_conversion(ctx, n_vararg.unwrap()));
    }

    let raw_call = ctx
        .builder
        .build_call(raw_fn, raw_fn_args.as_slice(), "raw_fn_call")
        .try_as_basic_value()
        .left()
        .unwrap();

    ctx.builder.build_return(Some(&raw_call));

    ctx.verify_or_panic(&function, "invoke");
    ctx.pass_manager.run_on(&function);

    ctx.exit_block();

    function
}

fn codegen_apply_to_fn(
    ctx: &mut CodegenContext,
    closure: &Closure,
    struct_ty: StructType,
    raw_fn: FunctionValue,
) -> FunctionValue {
    let fn_name = closure
        .lambda
        .name
        .as_ref()
        .map_or_else(|| "apply_closure".to_string(), |n| format!("apply_{}", n));
    let fn_name = ctx.mangle_str(fn_name);
    let list_ty = ctx.lookup_known_type("unlisp_rt_list");
    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");
    let has_restarg = closure.lambda.restarg.is_some();

    let arg_tys: Vec<_> = vec![struct_ty.ptr_type(AddressSpace::Generic).into(), list_ty];

    let fn_ty = obj_struct_ty.fn_type(arg_tys.as_slice(), has_restarg);
    let function = ctx.get_module().add_function(&fn_name, fn_ty, None);

    ctx.enter_fn_block(&function);

    let struct_ptr_par = function.get_nth_param(0).unwrap().into_pointer_value();
    struct_ptr_par.set_name("fn_obj");

    let list_param = function.get_nth_param(1).unwrap();
    list_param.as_struct_value().set_name("args");

    let mut raw_fn_args = vec![];

    for (i, _) in closure.free_vars.iter().enumerate() {
        let arg_ptr = unsafe {
            ctx.builder.build_struct_gep(
                struct_ptr_par,
                Function::FIELDS_COUNT + i as u32,
                "free_var_ptr",
            )
        };
        let arg = ctx.builder.build_load(arg_ptr, "free_var");
        raw_fn_args.push(arg);
    }

    let list_first_fn = ctx.lookup_known_fn("unlisp_rt_list_first");
    let list_rest_fn = ctx.lookup_known_fn("unlisp_rt_list_rest");

    let mut cur_list = list_param;

    for _ in closure.lambda.arglist.iter() {
        let arg = ctx
            .builder
            .build_call(list_first_fn, &[cur_list], "arg")
            .try_as_basic_value()
            .left()
            .unwrap();

        raw_fn_args.push(arg);
        cur_list = ctx
            .builder
            .build_call(list_rest_fn, &[cur_list], "rest")
            .try_as_basic_value()
            .left()
            .unwrap();
    }

    if has_restarg {
        let object_form_list_fn = ctx.lookup_known_fn("unlisp_rt_object_from_list");
        let obj = ctx
            .builder
            .build_call(object_form_list_fn, &[cur_list], "restarg_object")
            .try_as_basic_value()
            .left()
            .unwrap();
        raw_fn_args.push(obj);
    }

    let raw_call = ctx
        .builder
        .build_call(raw_fn, raw_fn_args.as_slice(), "raw_fn_call")
        .try_as_basic_value()
        .left()
        .unwrap();

    ctx.builder.build_return(Some(&raw_call));

    ctx.verify_or_panic(&function, "apply");
    ctx.pass_manager.run_on(&function);

    ctx.exit_block();

    function
}

pub fn compile_closure(ctx: &mut CodegenContext, closure: &Closure) -> CompileResult {
    let raw_fn = codegen_raw_fn(ctx, closure)?;
    let struct_ty = codegen_closure_struct(ctx, closure);
    let invoke_fn = codegen_invoke_fn(ctx, closure, struct_ty, raw_fn);
    let apply_to_fn = codegen_apply_to_fn(ctx, closure, struct_ty, raw_fn);

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
    let name_ptr = ctx.str_literal_as_i8_ptr(name);

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

    let struct_apply_to_fn_ptr =
        unsafe { ctx.builder.build_struct_gep(struct_ptr, 6, "apply_to_ptr") };

    let apply_to_fn_cast = ctx.builder.build_bitcast(
        apply_to_fn.as_global_value().as_pointer_value(),
        ctx.llvm_ctx.i8_type().ptr_type(AddressSpace::Generic),
        "cast_apply",
    );

    ctx.builder
        .build_store(struct_apply_to_fn_ptr, apply_to_fn_cast);

    let struct_has_restarg_ptr =
        unsafe { ctx.builder.build_struct_gep(struct_ptr, 7, "has_restarg") };

    ctx.builder.build_store(
        struct_has_restarg_ptr,
        ctx.llvm_ctx
            .bool_type()
            .const_int(closure.lambda.restarg.is_some() as u64, false),
    );

    for (i, var) in closure.free_vars.iter().enumerate() {
        let var_val = ctx
            .lookup_name(var)
            .ok_or_else(|| UndefinedSymbol::new(var.as_str()))?;

        let free_var_ptr = unsafe {
            ctx.builder.build_struct_gep(
                struct_ptr,
                Function::FIELDS_COUNT + i as u32,
                "free_var_ptr",
            )
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
