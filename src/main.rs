#![feature(untagged_unions)]

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::values::GlobalValue;
use inkwell::OptimizationLevel;
use std::error::Error;
use std::mem::{align_of, size_of};
use std::ffi::CStr;
use inkwell::AddressSpace;

extern crate libc;

use libc::c_char;

mod cons;
mod error;
mod lexer;
mod object;
mod pushback_reader;
mod reader;
mod runtime;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

fn main() -> Result<(), Box<Error>> {
    ExecutionEngine::link_in_mc_jit();
    let context = Context::create();
    let module1 = context.create_module("foo");
    let module2 = context.create_module("bar");
    let builder = context.create_builder();
    let execution_engine = module1.create_jit_execution_engine(OptimizationLevel::None)?;

    runtime::init(&context, &module1);

    execution_engine.add_module(&module2).unwrap();
    compile_global_nil(&context, &module2, &builder);

    let f2 = compile_get_nil(&context, &module2, &builder, &execution_engine);


    unsafe { println!("nil = {}", f2.call() ) }

    let module3 = context.create_module("baz");
    let execution_engine = module3.create_jit_execution_engine(OptimizationLevel::None)?;
    // execution_engine.add_module(&module3).unwrap();

    compile_global_nil2(&context, &module3, &builder);

    let f3 = compile_get_nil2(&context, &module3, &builder, &execution_engine);


    unsafe { println!("nil = {}", f3.call() ) }



    Ok(())
}

fn compile_get_nil(
    context: &Context,
    module: &Module,
    builder: &Builder,
    execution_engine: &ExecutionEngine,
)  ->  JitFunction<unsafe extern "C" fn() -> u32>
{
    let fn_ty = context.i32_type().fn_type(&[], false);

    let function = module.add_function("get_nil", fn_ty, None);
    let basic_block = context.append_basic_block(&function, "entry");

    builder.position_at_end(&basic_block);

    let val = builder.build_load(module.get_global("unlisp_rt_nil").unwrap().as_pointer_value(), "load_global");

    builder.build_return(Some(&val));

    // function.verify(true);

    unsafe { execution_engine.get_function("get_nil").unwrap() }
}

fn compile_get_nil2(
    context: &Context,
    module: &Module,
    builder: &Builder,
    execution_engine: &ExecutionEngine,
)  ->  JitFunction<unsafe extern "C" fn() -> u32>
{
    let fn_ty = context.i32_type().fn_type(&[], false);

    let function = module.add_function("get_nil2", fn_ty, None);
    let basic_block = context.append_basic_block(&function, "entry");

    builder.position_at_end(&basic_block);

    let val = builder.build_load(module.get_global("unlisp_rt_nil2").unwrap().as_pointer_value(), "load_global");

    builder.build_return(Some(&val));

    function.verify(true);

    unsafe { execution_engine.get_function("get_nil").unwrap() }
}



fn compile_global_nil(context: &Context, module: &Module, builder: &Builder) -> GlobalValue {
    let int32_ty = context.i32_type();
    let global = module.add_global(int32_ty, None, "unlisp_rt_nil");

    // global.set_initializer(&context.i32_type().const_int(5, false));

    global
}


fn compile_global_nil2(context: &Context, module: &Module, builder: &Builder) -> GlobalValue {
    let int32_ty = context.i32_type();
    let global = module.add_global(int32_ty, None, "unlisp_rt_nil2");

    // global.set_initializer(&context.i32_type().const_int(5, false));

    global
}


fn compile_global(context: &Context, module: &Module, builder: &Builder) -> GlobalValue {
    let hello_ascii = [104, 101, 108, 108, 111, 0];
    let array_ty = context.i8_type().array_type(hello_ascii.len() as u32);
    let global = module.add_global(array_ty, None, "hello_str");

    let hello_ascii_vals: Vec<_> = hello_ascii
        .iter()
        .map(|v| context.i8_type().const_int(*v, false))
        .collect();

    global.set_initializer(&context.i8_type().const_array(hello_ascii_vals.as_slice()));

    global
}

fn compile_save_sym(
    context: &Context,
    module: &Module,
    builder: &Builder,
    execution_engine: &ExecutionEngine,
) -> JitFunction<unsafe extern "C" fn()> {
    let void_ty = context.void_type();
    let fn_ty = void_ty.fn_type(&[], false);

    let function = module.add_function("save_sym", fn_ty, None);
    let basic_block = context.append_basic_block(&function, "entry");

    builder.position_at_end(&basic_block);

    let global = module.get_global("hello_str").unwrap();
    let global_ptr = unsafe {
        builder.build_gep(
            global.as_pointer_value(),
            &[
                context.i32_type().const_int(0, false),
                context.i32_type().const_int(0, false),
            ],
            "gep",
        )
    };
    let _ = builder.build_call(
        module.get_function("unlisp_rt_intern_sym").unwrap(),
        &[global_ptr.into()],
        "call",
    );
    builder.build_return(None);

    function.verify(true);

    unsafe { execution_engine.get_function("save_sym").unwrap() }
}



fn compile_rust_hello(context: &Context, module: &Module, builder: &Builder) -> () // Option<JitFunction<unsafe extern "C" fn()>>
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
