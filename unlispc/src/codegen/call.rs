use super::common::*;
use super::context::CodegenContext;
use super::top_level::compile_hir;
use crate::repr::Call;

use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use std::iter;
use std::rc::Rc;

fn codegen_simple_call(
    ctx: &mut CodegenContext,
    call: &Call,
    fn_obj_ptr: PointerValue,
    invoke_ptr: PointerValue,
    mut compiled_args: Vec<BasicValueEnum>,
) -> BasicValueEnum {
    let function_ptr_ty = ctx
        .lookup_known_type("unlisp_rt_function")
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let object_ty = ctx.lookup_known_type("unlisp_rt_object");
    let mut arg_tys: Vec<_> = iter::repeat(object_ty).take(call.args.len()).collect();
    arg_tys.push(function_ptr_ty.into());
    arg_tys.reverse();

    let invoke_fn_ptr_ty = object_ty
        .fn_type(arg_tys.as_slice(), false)
        .ptr_type(AddressSpace::Generic);

    let invoke_ptr_cast =
        ctx.builder
            .build_bitcast(invoke_ptr, invoke_fn_ptr_ty, "invoke_ptr_cast");

    compiled_args.reverse();
    compiled_args.push(fn_obj_ptr.into());
    compiled_args.reverse();

    ctx.builder
        .build_call(
            invoke_ptr_cast.into_pointer_value(),
            compiled_args.as_slice(),
            "invoke_result",
        )
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn codegen_vararg_call(
    ctx: &mut CodegenContext,
    call: &Call,
    fn_obj_ptr: PointerValue,
    arity: BasicValueEnum,
    invoke_ptr: PointerValue,
    mut compiled_args: Vec<BasicValueEnum>,
) -> BasicValueEnum {
    let function_ptr_ty = ctx
        .lookup_known_type("unlisp_rt_function")
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let object_ty = ctx.lookup_known_type("unlisp_rt_object");
    let mut arg_tys: Vec<_> = iter::repeat(object_ty).take(call.args.len()).collect();
    arg_tys.push(ctx.llvm_ctx.i64_type().into());
    arg_tys.push(function_ptr_ty.into());
    arg_tys.reverse();

    let invoke_fn_ptr_ty = object_ty
        .fn_type(arg_tys.as_slice(), false)
        .ptr_type(AddressSpace::Generic);

    let n_varargs = ctx.builder.build_int_sub(
        ctx.llvm_ctx
            .i64_type()
            .const_int(call.args.len() as u64, false),
        arity.into_int_value(),
        "n_varargs",
    );

    let invoke_ptr_cast =
        ctx.builder
            .build_bitcast(invoke_ptr, invoke_fn_ptr_ty, "invoke_ptr_cast");

    compiled_args.reverse();
    compiled_args.push(n_varargs.into());
    compiled_args.push(fn_obj_ptr.into());
    compiled_args.reverse();

    ctx.builder
        .build_call(
            invoke_ptr_cast.into_pointer_value(),
            compiled_args.as_slice(),
            "vararg_invoke_result",
        )
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn codegen_ok_arity_block(
    ctx: &mut CodegenContext,
    call: &Call,
    fn_obj_ptr: PointerValue,
    arity: BasicValueEnum,
) -> CompileResult {
    let invoke_ptr_ptr = unsafe {
        ctx.builder
            .build_struct_gep(fn_obj_ptr, 5, "invoke_ptr_ptr")
    };

    let invoke_ptr = ctx
        .builder
        .build_load(invoke_ptr_ptr, "invoke_ptr")
        .into_pointer_value();

    let compiled_args = call
        .args
        .iter()
        .map(|arg| compile_hir(ctx, arg))
        .collect::<Result<Vec<_>, _>>()?;

    let has_restarg_ptr = unsafe {
        ctx.builder
            .build_struct_gep(fn_obj_ptr, 7, "has_restarg_ptr")
    };
    let has_restarg = ctx.builder.build_load(has_restarg_ptr, "has_restarg");

    let merge_vararg_block = ctx.append_block();

    let vararg_block = ctx.enter_block();
    let vararg_result = codegen_vararg_call(
        ctx,
        call,
        fn_obj_ptr,
        arity,
        invoke_ptr,
        compiled_args.clone(),
    );
    ctx.builder.build_unconditional_branch(&merge_vararg_block);
    ctx.exit_block();

    let no_vararg_block = ctx.enter_block();
    let no_vararg_result =
        codegen_simple_call(ctx, call, fn_obj_ptr, invoke_ptr, compiled_args.clone());
    ctx.builder.build_unconditional_branch(&merge_vararg_block);
    ctx.exit_block();

    ctx.builder.build_conditional_branch(
        has_restarg.into_int_value(),
        &vararg_block,
        &no_vararg_block,
    );

    ctx.replace_cur_block(Rc::new(merge_vararg_block));

    let vararg_phi = ctx
        .builder
        .build_phi(ctx.lookup_known_type("unlisp_rt_object"), "phi");

    vararg_phi.add_incoming(&[
        (&vararg_result, &vararg_block),
        (&no_vararg_result, &no_vararg_block),
    ]);

    Ok(vararg_phi.as_basic_value())
}

fn codegen_fn_exists_block(
    ctx: &mut CodegenContext,
    call: &Call,
    fn_obj_ptr: PointerValue,
    sym_name_ptr: BasicValueEnum,
) -> CompileResult {
    let args_count = call.args.len() as u64;

    let arity_ptr = unsafe { ctx.builder.build_struct_gep(fn_obj_ptr, 3, "arity_ptr") };
    let arity = ctx.builder.build_load(arity_ptr, "arity");

    let is_correct_arg_num = ctx
        .builder
        .build_call(
            ctx.lookup_known_fn("unlisp_rt_check_arity"),
            &[
                fn_obj_ptr.into(),
                ctx.llvm_ctx.i64_type().const_int(args_count, false).into(),
            ],
            "arg_num_ok",
        )
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value();

    let wrong_arity_block = ctx.enter_block();

    ctx.builder.build_call(
        ctx.lookup_known_fn("unlisp_rt_raise_arity_error"),
        &[
            sym_name_ptr,
            arity.into(),
            ctx.llvm_ctx.i64_type().const_int(args_count, false).into(),
        ],
        "raise_arity_err",
    );
    ctx.builder.build_unreachable();
    ctx.exit_block();

    let enter_ok_arity_block = ctx.enter_block();
    let result = codegen_ok_arity_block(ctx, call, fn_obj_ptr, arity)?;
    let exit_ok_arity_block = ctx.exit_block();

    ctx.builder.build_conditional_branch(
        is_correct_arg_num,
        &enter_ok_arity_block,
        &wrong_arity_block,
    );
    ctx.replace_cur_block(exit_ok_arity_block);

    Ok(result)
}

pub fn compile_call(ctx: &mut CodegenContext, call: &Call) -> CompileResult {
    let sym_name_ptr = ctx.str_literal_as_i8_ptr(call.fn_name.as_str());

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
    let interned_sym_ptr = ctx
        .builder
        .build_call(intern_fn, &[sym_name_ptr.into()], "symbol")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let fn_obj_ptr_ptr = unsafe {
        ctx.builder
            .build_struct_gep(interned_sym_ptr, 1, "fn_obj_ptr_ptr")
    };

    let fn_obj_ptr = ctx
        .builder
        .build_load(fn_obj_ptr_ptr, "fn_obj_ptr")
        .into_pointer_value();

    let fn_obj_ptr_int =
        ctx.builder
            .build_ptr_to_int(fn_obj_ptr, ctx.llvm_ctx.i64_type(), "fn_obj_ptr_int");

    let fn_obj_ptr_is_null = ctx.builder.build_int_compare(
        IntPredicate::EQ,
        fn_obj_ptr_int,
        ctx.llvm_ctx.i64_type().const_int(0, false),
        "fn_obj_ptr_is_null",
    );

    let no_fn_block = ctx.enter_block();
    ctx.builder.build_call(
        ctx.lookup_known_fn("unlisp_rt_raise_undef_fn_error"),
        &[sym_name_ptr],
        "raise_undef_fn",
    );
    ctx.builder.build_unreachable();
    ctx.exit_block();

    let fn_exists_block = ctx.enter_block();
    let result = codegen_fn_exists_block(ctx, call, fn_obj_ptr, sym_name_ptr)?;
    let exit_fn_exists_block = ctx.exit_block();

    ctx.builder
        .build_conditional_branch(fn_obj_ptr_is_null, &no_fn_block, &fn_exists_block);
    ctx.replace_cur_block(exit_fn_exists_block);

    Ok(result)
}
