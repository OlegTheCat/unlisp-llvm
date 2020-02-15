use crate::error::{Error, ErrorType};
use crate::repr::*;

use inkwell::types::BasicType;

use super::call::compile_call;
use super::closure::compile_closure;
use super::common::*;
use super::context::CodegenContext;
use super::if_codegen::compile_if;
use super::let_block::compile_let_block;
use super::literal::{compile_literal, compile_nil_t_literal};
use super::quote::compile_quoted_literal;

pub fn compile_hir(ctx: &mut CodegenContext, hir: &HIR) -> CompileResult {
    match hir {
        HIR::Literal(literal) => compile_literal(ctx, literal),
        HIR::SetExpr(e) => compile_set_expr(ctx, e),
        HIR::Call(call) => compile_call(ctx, call),
        HIR::Closure(closure) => compile_closure(ctx, closure),
        HIR::Lambda(_) => panic!("cannot compile raw lambda"),
        HIR::If(if_hir) => compile_if(ctx, if_hir),
        HIR::Quote(quote) => compile_quoted_literal(ctx, &quote.body),
        HIR::LetBlock(let_block) => compile_let_block(ctx, let_block),
        HIR::DeclareVar(decl_var) => {
            ctx.declare_global_var(&decl_var.var_name);
            Ok(compile_nil_t_literal(ctx, false))
        }
    }
}

fn compile_set_expr(ctx: &mut CodegenContext, e: &SetExpr) -> CompileResult {
    // looking up non-captured name to force it into a box in case it is mutated
    // by a closure
    let (non_captured, already_boxed) = ctx.lookup_non_captured_name(&e.name).ok_or_else(|| {
        Error::new(
            ErrorType::Compilation,
            format!("no local symbol: {}", e.name.as_str()),
        )
    })?;

    if !already_boxed {
        let cur_block = ctx.cur_block();

        let struct_val = non_captured.as_struct_value();

        if let Some(instr) = struct_val.as_instruction() {
            let block = instr.get_parent().unwrap();
            if let Some(i) = instr.get_next_instruction() {
                ctx.builder.position_at(&block, &i);
            } else {
                ctx.builder.position_at_end(&block);
            }
        } else {
            // no instruction means we've got function parameter
            let f = ctx.find_function_by_parameter(non_captured).expect("no function for parameter");
            let block = f.get_first_basic_block().unwrap();
            if let Some(i) = block.get_first_instruction() {
                ctx.builder.position_before(&i);
            } else {
                ctx.builder.position_at_end(&block);
            }
        }

        let boxed = ctx
            .builder
            .build_call(
                ctx.lookup_known_fn("unlisp_rt_make_box"),
                // ugly HACK: creating `make_box` call and passing 0 as an argument.
                // we're not passing `non_captured` here, so it is not affected by
                // `replace_all_uses_with` call below
                &[ctx.llvm_ctx.i8_type().const_int(0, true).into()],
                "boxed",
            )
            .try_as_basic_value()
            .left()
            .unwrap();

        let boxed_struct = boxed.as_struct_value();

        // replacing all usages with a box
        boxed_struct.replace_all_uses_with(*struct_val);

        let box_instr = boxed_struct.as_instruction().unwrap();
        // now we can set correct argument to `make_box` call
        box_instr.set_operand(0, non_captured);

        ctx.replace_non_captured_mapping_value_with_box(&e.name, boxed);
        ctx.builder.position_at_end(&cur_block);
    }

    let local = ctx.lookup_local_name(&e.name).unwrap();

    let set_expr_arg = compile_hir(ctx, &e.val)?;
    let set_expr_val = ctx
        .builder
        .build_call(
            ctx.lookup_known_fn("unlisp_rt_box_set"),
            &[local, set_expr_arg],
            "set_expr_val",
        )
        .try_as_basic_value()
        .left()
        .unwrap();

    Ok(set_expr_val)
}

pub fn compile_hirs(ctx: &mut CodegenContext, hirs: &[HIR]) -> CompileResult {
    let mut val_opt = None;

    for hir in hirs {
        val_opt = Some(compile_hir(ctx, hir)?);
    }

    let val_or_nil = val_opt.unwrap_or_else(|| compile_nil_t_literal(ctx, false));

    Ok(val_or_nil)
}

pub fn compile_top_level_hirs(ctx: &mut CodegenContext, hirs: &[HIR]) -> Result<String, Error> {
    let obj_struct_ty = ctx.lookup_known_type("unlisp_rt_object");
    let fn_ty = obj_struct_ty.fn_type(&[], false);
    let fn_name = ctx.mangle_str("__repl_form");
    let function = ctx.get_module().add_function(&fn_name, fn_ty, None);

    ctx.enter_fn_block(&function);

    // let buf_ptr = ctx
    //     .builder
    //     .build_alloca(ctx.lookup_known_type("setjmp_buf"), "setjmp_buf");
    // let setjmp = ctx
    //     .builder
    //     .build_call(ctx.lookup_known_fn("setjmp"), &[buf_ptr.into()], "setjmp")
    //     .try_as_basic_value()
    //     .left()
    //     .unwrap()
    //     .into_int_value();

    // let ok_block = ctx.llvm_ctx.append_basic_block(&function, "ok");
    // let err_block = ctx.llvm_ctx.append_basic_block(&function, "err");

    // let br = ctx
    //     .builder
    //     .build_conditional_branch(setjmp, &err_block, &ok_block);

    // ctx.builder.position_at_end(&ok_block);
    // ctx.blocks_stack.push(Rc::new(ok_block));

    let val = compile_hirs(ctx, hirs)?;

    // ctx.builder.build_call(
    //     ctx.lookup_known_fn("longjmp"),
    //     &[
    //         buf_ptr.into(),
    //         ctx.llvm_ctx.i32_type().const_int(1, false).into(),
    //     ],
    //     "longjmp",
    // );

    ctx.builder.build_return(Some(&val));

    // ctx.blocks_stack.pop();
    // ctx.builder.position_at_end(&err_block);
    // let val = compile_hirs(ctx, &[HIR::Literal(Literal::IntegerLiteral(2))])?;

    // ctx.builder.build_return(Some(&val));

    ctx.verify_or_panic(&function, "top-level");
    ctx.pass_manager.run_on(&function);

    Ok(fn_name)
}
