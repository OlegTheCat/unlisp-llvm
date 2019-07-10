#![feature(untagged_unions)]
#![feature(c_variadic)]

use inkwell::context::Context;

use std::fs;
use std::io;
use std::io::{Read, Write};
use std::env;
use std::error::Error;

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

pub fn eval_stdlib(ctx: &mut CodegenContext) {
    let mut file = fs::File::open("src/stdlib.unl").expect("stdlib file not found");

    let mut reader = reader::Reader::create(&mut file);
    loop {
        match read_and_parse(&mut reader) {
            Ok(Some(hir)) => unsafe {
                match ctx.compile_hirs(&[hir]) {
                    Ok(compiled_fn) => {
                        runtime::exceptions::run_with_global_ex_handler(|| compiled_fn.call())
                            .map_err(|err| panic!("error during stdlib loading: {}", err))
                            .unwrap();
                    }
                    Err(err) => {
                        panic!("error during stdlib loading: {}", err);
                    }
                }
            },
            Ok(None) => break,
            Err(ref e) => panic!("error during stdlib loading: {}", e),
        }
        ctx.reinitialize();
    }
}

fn repl(ctx: &mut CodegenContext, dump_compiled: bool) {
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
                            if dump_compiled {
                                eprintln!("Expression compiled to LLVM IR:");
                                ctx.get_module().print_to_stderr();
                            }
                            match runtime::exceptions::run_with_global_ex_handler(|| {
                                compiled_fn.call()
                            }) {
                                Ok(obj) => println!("{}", obj),
                                Err(err) => eprintln!("runtime error: {}", err),
                            }
                        }
                        Err(err) => {
                            eprintln!("compilation error: {}", err);
                        }
                    }
                }
            }
            Ok(None) => break,
            Err(e) => {
                match e.downcast_ref::<error::Error>() {
                    Some(e) if e.ty == error::ErrorType::Runtime => {
                        eprintln!("macroexpansion error: {}", e)
                    }
                    _ => eprintln!("reader error: {}", e)
                }
            }
        }
        ctx.reinitialize();
        prompt();
    }
}

fn main() {
    let mut dump_compiled_ir = false;

    for arg in env::args().skip(1) {
        match arg.as_str() {
            "--dump-compiled" => dump_compiled_ir = true,
            _ => eprintln!("ignoring unknown option: {}", arg)
        }
    }

    runtime::symbols::init();
    runtime::predefined::init();

    let ctx = Context::create();
    let mut codegen_ctx = CodegenContext::new(&ctx);

    eval_stdlib(&mut codegen_ctx);

    repl(&mut codegen_ctx, dump_compiled_ir);
}
