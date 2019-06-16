use crate::repr::*;

use inkwell::types::BasicType;

use super::call::compile_call;
use super::closure::compile_closure;
use super::common::*;
use super::context::CodegenContext;
use super::if_codegen::compile_if;
use super::let_block::compile_let_block;
use super::literal::{compile_literal, compile_nil_literal};
use super::quote::compile_quoted_literal;

pub fn compile_hir(ctx: &mut CodegenContext, hir: &HIR) -> CompileResult {
    match hir {
        HIR::Literal(literal) => compile_literal(ctx, literal),
        HIR::Call(call) => compile_call(ctx, call),
        HIR::Closure(closure) => compile_closure(ctx, closure),
        HIR::Lambda(_) => panic!("cannot compile raw lambda"),
        HIR::If(if_hir) => compile_if(ctx, if_hir),
        HIR::Quote(quote) => compile_quoted_literal(ctx, &quote.body),
        HIR::LetBlock(let_block) => compile_let_block(ctx, let_block),
    }
}

pub fn compile_hirs(ctx: &mut CodegenContext, hirs: &[HIR]) -> CompileResult {
    let mut val_opt = None;

    for hir in hirs {
        val_opt = Some(compile_hir(ctx, hir)?);
    }

    let val_or_nil = val_opt.unwrap_or_else(|| compile_nil_literal(ctx));

    Ok(val_or_nil)
}

pub fn compile_top_level_hir(ctx: &mut CodegenContext, hirs: &[HIR]) -> GenResult<String> {
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
    ctx.pass_manager.run_on_function(&function);

    Ok(fn_name)
}
