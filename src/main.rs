#![feature(untagged_unions)]

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::module::Linkage;
use inkwell::targets::{InitializationConfig, Target};
use std::error::Error;

extern crate libc;

mod pushback_reader;
mod cons;
mod error;
mod lexer;
mod reader;
mod object;
mod runtime;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

#[no_mangle]
pub extern "C" fn hello_from_rust() -> () {
    println!("hi!!!");
}

#[used]
static USED: [unsafe extern "C" fn(); 1] = [hello_from_rust];

fn main() -> Result<(), Box<Error>> {
    let context = Context::get_global();
    let module = context.create_module("sum");
    let builder = context.create_builder();
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;

    println!("{:?}", module.get_type("value_t"));

    compile_rust_hello(&context, &module, &builder);

    println!("before compiling");

    let sum = jit_compile_sum(&context, &module, &builder, &execution_engine)
        .ok_or("Unable to JIT compile `sum`")?;

    println!("after compiling");

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
        assert_eq!(sum.call(x, y, z), x + y + z);
    }

    Ok(())
}

fn compile_rust_hello(
    context: &Context,
    module: &Module,
    builder: &Builder,
) -> () // Option<JitFunction<unsafe extern "C" fn()>>
{
    let void_ty = context.void_type();
    let fn_ty = void_ty.fn_type(&[], false);


    let fun = module.add_function("hello_from_rust", fn_ty, Some(Linkage::External));

    // fun.print_to_stderr();
}

fn jit_compile_sum(
    context: &Context,
    module: &Module,
    builder: &Builder,
    execution_engine: &ExecutionEngine,
) -> Option<JitFunction<SumFunc>> {
    let i64_type = context.i64_type();
    let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);

    let function = module.add_function("sum", fn_type, None);
    let basic_block = context.append_basic_block(&function, "entry");

    builder.position_at_end(&basic_block);

    let x = function.get_nth_param(0)?.into_int_value();
    let y = function.get_nth_param(1)?.into_int_value();
    let z = function.get_nth_param(2)?.into_int_value();

    let _ = builder.build_call(module.get_function("hello_from_rust").unwrap(), &[], "xxx");

    let sum = builder.build_int_add(x, y, "sum");
    let sum = builder.build_int_add(sum, z, "sum");

    builder.build_return(Some(&sum));

    function.print_to_stderr();

    unsafe { execution_engine.get_function("sum").ok() }
}
