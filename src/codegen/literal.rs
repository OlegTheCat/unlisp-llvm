use crate::repr::Literal;
use inkwell::values::BasicValueEnum;

use super::common::*;
use super::context::CodegenContext;

fn compile_integer(ctx: &mut CodegenContext, i: i64) -> BasicValueEnum {
    let ty = ctx.llvm_ctx.i64_type();
    let int_val = ty.const_int(i as u64, true);
    let call = ctx.builder.build_call(
        ctx.lookup_known_fn("unlisp_rt_object_from_int"),
        &[int_val.into()],
        "call",
    );

    call.try_as_basic_value().left().unwrap()
}

pub fn compile_nil_literal(ctx: &CodegenContext) -> BasicValueEnum {
    ctx.builder
        .build_call(ctx.lookup_known_fn("unlisp_rt_nil_object"), &[], "nil_obj")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn compile_string_literal(ctx: &mut CodegenContext, s: &String) -> BasicValueEnum {
    let to_obj_fn = ctx.lookup_known_fn("unlisp_rt_object_from_string");
    let literal_ptr = ctx.str_literal_as_i8_ptr(s.as_str());

    ctx.builder
        .build_call(to_obj_fn, &[literal_ptr.into()], "obj_from_literal")
        .try_as_basic_value()
        .left()
        .unwrap()
}

pub fn compile_literal(ctx: &mut CodegenContext, literal: &Literal) -> CompileResult {
    match literal {
        Literal::ListLiteral(vec) if vec.is_empty() => Ok(compile_nil_literal(ctx)),
        Literal::ListLiteral(_) => panic!("cannot compile unquoted list literal"),
        Literal::IntegerLiteral(i) => Ok(compile_integer(ctx, *i)),
        Literal::StringLiteral(s) => Ok(compile_string_literal(ctx, s)),
        Literal::SymbolLiteral(s) => {
            let val = ctx
                .lookup_name(s)
                .ok_or_else(|| UndefinedSymbol::new(s.as_str()))?;
            Ok(val)
        }
        Literal::T => panic!("t literal is not yet supported")
    }
}
