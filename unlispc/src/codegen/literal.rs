use crate::repr::Literal;
use inkwell::values::BasicValueEnum;

use super::common::*;
use super::context::CodegenContext;
use crate::error;

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

pub fn compile_nil_t_literal(ctx: &CodegenContext, t: bool) -> BasicValueEnum {
    let fn_name = if t {
        "unlisp_rt_t_object"
    } else {
        "unlisp_rt_nil_object"
    };
    ctx.builder
        .build_call(ctx.lookup_known_fn(fn_name), &[], "nil_obj")
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
        Literal::ListLiteral(vec) if vec.is_empty() => Ok(compile_nil_t_literal(ctx, false)),
        Literal::ListLiteral(_) => panic!("cannot compile unquoted list literal"),
        Literal::IntegerLiteral(i) => Ok(compile_integer(ctx, *i)),
        Literal::StringLiteral(s) => Ok(compile_string_literal(ctx, s)),
        Literal::SymbolLiteral(s) => {
            let val = ctx.lookup_name_or_gen_global_access(s).ok_or_else(|| {
                error::Error::new(
                    error::ErrorType::Compilation,
                    format!("undefined symbol: {}", s.as_str()),
                )
            })?;
            Ok(val)
        }
        Literal::T => Ok(compile_nil_t_literal(ctx, true)),
    }
}
