use inkwell::context::Context;
use inkwell::module::Module;

use unlisp_rt::defs::{Object, Function, List, Symbol, va_gen_llvm_def};
use unlisp_rt::defs;
use unlisp_rt::exceptions;

pub fn gen_defs(ctx: &Context, module: &Module) {
    Object::gen_llvm_def(ctx, module);
    List::gen_llvm_def(ctx, module);
    Function::gen_llvm_def(ctx, module);
    Symbol::gen_llvm_def(ctx, module);

    va_gen_llvm_def(ctx, module);

    defs::rt_fns_llvm_defs::gen_defs(ctx, module);
    exceptions::rt_fns_llvm_defs::gen_defs(ctx, module);
}
