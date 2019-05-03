use crate::object::LispForm;
use crate::runtime;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue};

fn compile_integer(ctx: &Context, module: &Module, builder: &Builder, i: i64) -> BasicValueEnum {
    let ty = ctx.i64_type();
    let int_val = ty.const_int(i as u64, true);
    let call = builder.build_call(
        module.get_function("unlisp_rt_object_from_int").unwrap(),
        &[int_val.into()],
        "call",
    );

    call.try_as_basic_value().left().unwrap()
}

fn wrap_toplevel(ctx: &Context, builder: &Builder, module: &Module, val: BasicValueEnum) {
    let fn_ty = val.get_type().fn_type(&[], false);
    let function = module.add_function("__repl_form", fn_ty, None);
    let basic_block = ctx.append_basic_block(&function, "entry");
    builder.position_at_end(&basic_block);
    builder.build_return(Some(&val));
}

pub fn compile_form(ctx: &Context, obj: LispForm) -> Module {
    let module = ctx.create_module("repl");
    let builder = ctx.create_builder();

    runtime::init(ctx, &module);

    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_ty = obj_struct_ty.fn_type(&[], false);
    let function = module.add_function("__repl_form", fn_ty, None);
    let basic_block = ctx.append_basic_block(&function, "entry");
    builder.position_at_end(&basic_block);

    let val = match obj {
        LispForm::Integer(i) => compile_integer(ctx, &module, &builder, i),
        _ => panic!("unsuported form"),
    };

    builder.build_return(Some(&val));

    module
}
