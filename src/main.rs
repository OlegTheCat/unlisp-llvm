#![feature(untagged_unions)]
#![feature(c_variadic)]

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

use std::io;
use std::io::Write;

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

fn repl(ctx: &mut codegen::CodegenContext) {

    let execution_engine = ctx
        .get_module()
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let mut stdin = io::stdin();

    let prompt = || {
        print!(">>> ");
        io::stdout().flush().unwrap();
    };

    let mut reader = reader::Reader::create(&mut stdin);

    prompt();
    loop {
        match reader.read_form() {
            Ok(Some(form)) => {
                ctx.reinitialize();
                match ctx.compile_top_level(&form) {
                    Ok(fn_name) => {
                        println!("Expression compiled to LLVM IR:");
                        ctx.get_module().print_to_stderr();
                        execution_engine.add_module(ctx.get_module()).unwrap();

                        unsafe {
                            let f: JitFunction<unsafe extern "C" fn() -> runtime::defs::Object> =
                                execution_engine.get_function(fn_name.as_str()).unwrap();

                            match runtime::exceptions::run_with_global_ex_handler(f) {
                                Ok(obj) => println!("{}", obj),
                                Err(msg) => println!("runtime error: {}", msg)
                            }
                        }
                    }
                    Err(err) => {
                        println!("compilation error: {}", err);
                    }
                }
            }
            Ok(None) => break,
            Err(ref e) => println!("reader error: {}", e),
        }
        prompt();
    }
}

use runtime::defs::*;

fn main() {
    runtime::symbols::init();
    runtime::predefined::init();

    let ctx = Context::create();
    let mut codegen_ctx = codegen::CodegenContext::new(&ctx);

    repl(&mut codegen_ctx);
}
