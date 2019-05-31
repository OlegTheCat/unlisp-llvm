use crate::repr::If;

use super::common::CompileResult;
use super::context::CodegenContext;
use super::literal::compile_nil_literal;
use super::top_level::compile_hir;

use std::rc::Rc;

pub fn compile_if(ctx: &mut CodegenContext, if_hir: &If) -> CompileResult {
    let merge_block = ctx.append_block();

    let compiled_cond = compile_hir(ctx, &if_hir.cond)?;

    let enter_then_block = ctx.enter_block();
    let compiled_then = compile_hir(ctx, &if_hir.then_hir)?;
    ctx.builder.build_unconditional_branch(&merge_block);
    let exit_then_block = ctx.exit_block();

    let enter_else_block = ctx.enter_block();
    let compiled_else = match if_hir.else_hir.as_ref() {
        Some(hir) => compile_hir(ctx, hir)?,
        None => compile_nil_literal(ctx),
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

    ctx.replace_cur_block(Rc::new(merge_block));

    let phi = ctx
        .builder
        .build_phi(ctx.lookup_known_type("unlisp_rt_object"), "phi");
    phi.add_incoming(&[
        (&compiled_then, &exit_then_block),
        (&compiled_else, &exit_else_block),
    ]);

    Ok(phi.as_basic_value())
}
