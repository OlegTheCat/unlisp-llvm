use crate::repr::Literal;
use inkwell::values::BasicValueEnum;

use super::common::CompileResult;
use super::context::CodegenContext;

fn compile_quoted_symbol(ctx: &mut CodegenContext, name: &String) -> BasicValueEnum {
    let sym_name_ptr = ctx.name_as_i8_ptr(name.as_str());

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_intern_sym");
    let interned_sym_ptr = ctx
        .builder
        .build_call(intern_fn, &[sym_name_ptr.into()], "intern")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let intern_fn = ctx.lookup_known_fn("unlisp_rt_object_from_symbol");

    ctx.builder
        .build_call(intern_fn, &[interned_sym_ptr.into()], "object")
        .try_as_basic_value()
        .left()
        .unwrap()
}

pub fn compile_quoted_literal(ctx: &mut CodegenContext, literal: &Literal) -> CompileResult {
    match literal {
        Literal::SymbolLiteral(s) => Ok(compile_quoted_symbol(ctx, s)),
        _ => unimplemented!(),
    }
}
