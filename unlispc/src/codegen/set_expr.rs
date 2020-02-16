use crate::repr::SetExpr;
use crate::error;

use super::top_level::compile_hir;
use super::common::*;
use super::context::CodegenContext;

pub fn compile_set_expr(ctx: &mut CodegenContext, e: &SetExpr) -> CompileResult {
    // looking up non-captured name to force it into a box in case it is mutated
    // by a closure
    let (non_captured, already_boxed) = ctx.lookup_non_captured_name(&e.name).ok_or_else(|| {
        error::Error::new(
            error::ErrorType::Compilation,
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
