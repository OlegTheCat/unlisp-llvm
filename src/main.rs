#![feature(untagged_unions)]

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

mod codegen;
mod cons;
mod error;
mod lexer;
mod pushback_reader;
mod reader;
mod repr;
mod runtime;

pub fn read(s: impl Into<String>) -> repr::Form {
    let s = s.into();
    let mut bytes = s.as_bytes();
    let mut reader = reader::Reader::create(&mut bytes);
    reader.read_form().unwrap().unwrap()
}

fn main() {
    runtime::symbols::init();
    runtime::predefined::init();

    let ctx = Context::create();
    let mut codegen_ctx = codegen::CodegenContext::new(&ctx);

    let fn_name = codegen_ctx
        .compile_top_level(&vec![
            read("(set-fn (quote foo) (lambda (x y) (lambda (z p) (+ x (+ y (+ z p))))))"),
            read("(set-fn (quote bar) (foo 1 2))"),
            read("(bar 3 4)"),
        ])
        .unwrap();

    codegen_ctx.get_module().print_to_stderr();

    let execution_engine = codegen_ctx
        .get_module()
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    unsafe {
        let f: JitFunction<unsafe extern "C" fn() -> runtime::defs::Object> =
            execution_engine.get_function(fn_name.as_str()).unwrap();
        println!("call result: {}", f.call());
    }
}
