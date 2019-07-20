use crate::repr::Literal;
use inkwell::values::BasicValueEnum;

use super::common::CompileResult;
use super::context::CodegenContext;
use super::literal::*;

fn compile_quoted_symbol(ctx: &mut CodegenContext, name: &String) -> BasicValueEnum {
    let interned_sym_ptr = ctx.get_interned_sym(name);
    let to_obj_fn = ctx.lookup_known_fn("unlisp_rt_object_from_symbol");

    ctx.builder
        .build_call(to_obj_fn, &[interned_sym_ptr.into()], "object")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn compile_quoted_list(ctx: &mut CodegenContext, list: &Vec<Literal>) -> CompileResult {
    let cons_fn = ctx.lookup_known_fn("unlisp_rt_list_cons");
    let empty_list_fn = ctx.lookup_known_fn("unlisp_rt_empty_list");
    let object_form_list_fn = ctx.lookup_known_fn("unlisp_rt_object_from_list");

    let mut result = ctx
        .builder
        .build_call(empty_list_fn, &[], "empty")
        .try_as_basic_value()
        .left()
        .unwrap();

    for el in list.iter().rev() {
        let compiled = compile_quoted_literal(ctx, el)?;
        result = ctx
            .builder
            .build_call(cons_fn, &[compiled, result], "result")
            .try_as_basic_value()
            .left()
            .unwrap();
    }

    let result_obj = ctx
        .builder
        .build_call(object_form_list_fn, &[result], "result_obj")
        .try_as_basic_value()
        .left()
        .unwrap();

    Ok(result_obj)
}

pub fn compile_quoted_literal(ctx: &mut CodegenContext, literal: &Literal) -> CompileResult {
    match literal {
        Literal::SymbolLiteral(s) => Ok(compile_quoted_symbol(ctx, s)),
        Literal::ListLiteral(list) => Ok(compile_quoted_list(ctx, list)?),
        Literal::IntegerLiteral(_) | Literal::StringLiteral(_) | Literal::T => {
            compile_literal(ctx, literal)
        }
    }
}
