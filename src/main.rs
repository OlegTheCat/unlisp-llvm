#![feature(untagged_unions)]
#![feature(c_variadic)]

use inkwell::context::Context;

use std::error::Error;
use std::io;
use std::io::{Read, Write};

mod codegen;
mod error;
mod lexer;
mod pushback_reader;
mod reader;
mod repr;
mod runtime;

use codegen::context::CodegenContext;

fn read_and_parse<'a, T: Read>(
    reader: &mut reader::Reader<'a, T>,
) -> Result<Option<repr::HIR>, Box<dyn Error>> {
    let form = reader.read_form()?;
    Ok(form
        .as_ref()
        .map(repr::form_to_hir_with_transforms)
        .transpose()?)
}

fn repl(ctx: &mut CodegenContext) {
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
                unsafe {
                    match ctx.compile_hirs(&[hir]) {
                        Ok(compiled_fn) => {
                            // println!("Expression compiled to LLVM IR:");
                            // ctx.get_module().print_to_stderr();
                            match runtime::exceptions::run_with_global_ex_handler(|| {
                                compiled_fn.call()
                            }) {
                                Ok(obj) => println!("{}", obj),
                                Err(err) => println!("runtime error: {}", err),
                            }
                        }
                        Err(err) => {
                            println!("compilation error: {}", err);
                        }
                    }
                }
            }
            Ok(None) => break,
            Err(ref e) => println!("reader error: {}", e),
        }
        ctx.reinitialize();
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
