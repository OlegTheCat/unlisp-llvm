use inkwell::context::Context;
use inkwell::module::Module;

use unlisp_rt::defs::*;
use unlisp_rt::exceptions::*;

pub fn gen_defs(ctx: &Context, module: &Module) {
    Object::gen_llvm_def(ctx, module);
    Cons::gen_llvm_def(ctx, module);
    Function::gen_llvm_def(ctx, module);
    Symbol::gen_llvm_def(ctx, module);

    va_gen_llvm_def(ctx, module);

    unlisp_rt_intern_sym_gen_def(ctx, module);
    unlisp_rt_object_from_int_gen_def(ctx, module);
    unlisp_rt_object_from_string_gen_def(ctx, module);
    unlisp_rt_int_from_obj_gen_def(ctx, module);
    unlisp_rt_object_from_function_gen_def(ctx, module);
    unlisp_rt_object_from_symbol_gen_def(ctx, module);
    unlisp_rt_object_from_cons_gen_def(ctx, module);
    unlisp_rt_object_from_list_gen_def(ctx, module);
    unlisp_rt_object_is_nil_gen_def(ctx, module);
    unlisp_rt_nil_object_gen_def(ctx, module);
    unlisp_rt_t_object_gen_def(ctx, module);
    unlisp_rt_check_arity_gen_def(ctx, module);
    unlisp_rt_va_list_into_list_gen_def(ctx, module);
    unlisp_rt_list_car_gen_def(ctx, module);
    unlisp_rt_list_cdr_gen_def(ctx, module);
    unlisp_rt_list_cons_gen_def(ctx, module);
    unlisp_rt_init_runtime_gen_def(ctx, module);
    unlisp_rt_symbol_value_gen_def(ctx, module);
    unlisp_rt_symbol_function_gen_def(ctx, module);

    unlisp_rt_raise_undef_fn_error_gen_def(ctx, module);
    unlisp_rt_raise_arity_error_gen_def(ctx, module);
    unlisp_rt_run_with_global_ex_handler_gen_def(ctx, module);

    unlisp_rt_make_box_gen_def(ctx, module);
    unlisp_rt_box_ref_gen_def(ctx, module);
    unlisp_rt_box_set_gen_def(ctx, module);
}
