#![feature(untagged_unions)]
#![feature(c_variadic)]

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

use std::error::Error;
use std::io;
use std::io::{Read, Write};

mod codegen;
mod cons;
mod error;
mod lexer;
mod pushback_reader;
mod reader;
mod repr;
mod runtime;

use codegen::context::CodegenContext;
use codegen::top_level::compile_top_level_hir;

fn read_and_parse<'a, T: Read>(
    reader: &mut reader::Reader<'a, T>,
) -> Result<Option<repr::HIR>, Box<Error>> {
    let form = reader.read_form()?;
    Ok(form
        .as_ref()
        .map(repr::form_to_hir_with_transforms)
        .transpose()?)
}

fn repl(ctx: &mut CodegenContext) {
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
        match read_and_parse(&mut reader) {
            Ok(Some(hir)) => {
                ctx.reinitialize();
                match compile_top_level_hir(ctx, &[hir]) {
                    Ok(fn_name) => {
                        println!("Expression compiled to LLVM IR:");
                        ctx.get_module().print_to_stderr();
                        execution_engine.add_module(ctx.get_module()).unwrap();

                        unsafe {
                            let f: JitFunction<unsafe extern "C" fn() -> runtime::defs::Object> =
                                execution_engine.get_function(fn_name.as_str()).unwrap();

                            match runtime::exceptions::run_with_global_ex_handler(f) {
                                Ok(obj) => println!("{}", obj),
                                Err(msg) => println!("runtime error: {}", msg),
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

fn main() {
    runtime::symbols::init();
    runtime::predefined::init();

    let ctx = Context::create();
    let mut codegen_ctx = CodegenContext::new(&ctx);

    repl(&mut codegen_ctx);
}
