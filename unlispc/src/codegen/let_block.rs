use crate::repr::LetBlock;

use super::common::*;
use super::context::CodegenContext;
use super::top_level::{compile_hir, compile_hirs};

pub fn compile_let_block(ctx: &mut CodegenContext, let_block: &LetBlock) -> CompileResult {
    for (name, hir) in let_block.bindings.iter() {
        let val = compile_hir(ctx, hir)?;
        ctx.push_env();
        ctx.save_env_mapping(name.clone(), val, false);
    }

    let result = compile_hirs(ctx, let_block.body.as_slice())?;

    for _ in let_block.bindings.iter() {
        ctx.pop_env();
    }

    Ok(result)
}
