use crate::defs::*;
use crate::error::RuntimeError;
use crate::exceptions;
use crate::symbols;

use libc::{c_char, c_void};
use std::ffi::{CStr, CString};
use std::io::Write;
use std::mem;

fn arr_to_raw(arr: &[&str]) -> *const *const c_char {
    let vec: Vec<_> = arr
        .iter()
        .map(|s| CString::new(*s).unwrap().into_raw())
        .collect();
    let ptr = vec.as_ptr();

    mem::forget(vec);

    ptr as *const *const c_char
}

fn init_symbol_fn(
    invoke_fn: *const c_void,
    apply_to_fn: *const c_void,
    name: &str,
    arglist: &[&str],
    vararg: bool,
) {
    let sym = symbols::get_or_intern_symbol(name.to_string());

    let func = Function {
        ty: FunctionType::Function,
        name: CString::new(name).unwrap().into_raw(),
        arglist: arr_to_raw(arglist),
        arg_count: (arglist.len() as u64),
        is_macro: false,
        invoke_f_ptr: invoke_fn,
        apply_to_f_ptr: apply_to_fn,
        has_restarg: vararg,
    };

    let func = Box::into_raw(Box::new(func));

    unsafe { (*sym).function = func };
}

unsafe extern "C" fn native_add_invoke(_: *const Function, n: u64, mut args: ...) -> Object {
    let args = va_list_to_obj_array(n, args.as_va_list());
    let mut sum = 0;

    for i in 0..n {
        sum += (*args.offset(i as isize)).unpack_int();
    }

    Object::from_int(sum)
}

unsafe extern "C" fn native_add_apply(_: *const Function, args: List) -> Object {
    let mut sum = 0;
    let args_count = args.len;
    let mut cur_args = args;

    for _ in 0..args_count {
        sum += cur_args.first().unpack_int();
        cur_args = cur_args.rest();
    }

    Object::from_int(sum)
}

unsafe extern "C" fn native_sub_invoke(
    _: *const Function,
    n: u64,
    x: Object,
    mut args: ...
) -> Object {
    let args = va_list_to_obj_array(n, args.as_va_list());
    let mut result = x.unpack_int();

    for i in 0..n {
        result -= (*args.offset(i as isize)).unpack_int();
    }

    Object::from_int(result)
}

unsafe extern "C" fn native_sub_apply(_: *const Function, args: List) -> Object {
    let mut result = args.first().unpack_int();

    let mut cur_args = args.rest();
    let args_len = cur_args.len;

    for _ in 0..args_len {
        result -= cur_args.first().unpack_int();
        cur_args = cur_args.rest();
    }

    Object::from_int(result)
}

extern "C" fn native_equal_invoke(_: *const Function, x: Object, y: Object) -> Object {
    if x == y {
        x
    } else {
        Object::nil()
    }
}

unsafe extern "C" fn native_equal_apply(f: *const Function, args: List) -> Object {
    native_equal_invoke(f, args.first(), args.rest().first())
}

extern "C" fn native_set_fn_invoke(_: *const Function, sym: Object, func: Object) -> Object {
    let sym = sym.unpack_symbol();
    let func = func.unpack_function();

    unsafe { (*sym).function = func };

    Object::nil()
}

unsafe extern "C" fn native_set_fn_apply(f: *const Function, args: List) -> Object {
    native_set_fn_invoke(f, args.first(), args.rest().first())
}

extern "C" fn native_cons_invoke(_: *const Function, x: Object, list: Object) -> Object {
    let list = list.unpack_list();
    let len = unsafe { (*list).len };

    let node = Node {
        val: Box::into_raw(Box::new(x)),
        next: list,
    };

    let new_list = List {
        node: Box::into_raw(Box::new(node)),
        len: len + 1,
    };

    Object::from_list(Box::into_raw(Box::new(new_list)))
}

unsafe extern "C" fn native_cons_apply(f: *const Function, args: List) -> Object {
    native_cons_invoke(f, args.first(), args.rest().first())
}

extern "C" fn native_rest_invoke(_: *const Function, list: Object) -> Object {
    let list = list.unpack_list();
    let len = unsafe { (*list).len };

    if len == 0 {
        Object::nil()
    } else {
        let rest = unsafe { (*(*list).node).next };
        Object::from_list(rest)
    }
}

unsafe extern "C" fn native_rest_apply(f: *const Function, args: List) -> Object {
    native_rest_invoke(f, args.first())
}

unsafe extern "C" fn native_first_invoke(_: *const Function, list: Object) -> Object {
    let list = list.unpack_list();
    let len = (*list).len;

    if len == 0 {
        exceptions::raise_error("cannot do first on an empty list".to_string());
    } else {
        (*(*(*list).node).val).clone()
    }
}

unsafe extern "C" fn native_first_apply(f: *const Function, args: List) -> Object {
    native_first_invoke(f, args.first())
}

unsafe fn apply_to_list(f: *const Function, args: List) -> Object {
    if !unlisp_rt_check_arity(f, args.len) {
        exceptions::unlisp_rt_raise_arity_error((*f).name, (*f).arg_count, args.len);
    }

    let apply_fn: unsafe extern "C" fn(*const Function, List) -> Object =
        mem::transmute((*f).apply_to_f_ptr);
    apply_fn(f, args)
}

unsafe extern "C" fn native_apply_invoke(
    _: *const Function,
    n: u64,
    f: Object,
    mut args: ...
) -> Object {
    let f = f.unpack_function();
    let args_arr = va_list_to_obj_array(n, args.as_va_list());
    let last_arg = (*args_arr.offset((n as isize) - 1)).unpack_list();
    let args_list = obj_array_to_list(n - 1, args_arr, Some(last_arg));

    apply_to_list(f, (*args_list).clone())
}

unsafe extern "C" fn native_apply_apply(_: *const Function, args: List) -> Object {
    let f = args.first().unpack_function();

    let mut to_cons = vec![];
    let mut f_args = args.rest();

    while f_args.len != 1 {
        to_cons.push(f_args.first());
        f_args = f_args.rest();
    }

    let cons_base = f_args.first().unpack_list();

    let reconsed_args = to_cons
        .into_iter()
        .rev()
        .fold((*cons_base).clone(), |acc, item| acc.cons(item));

    apply_to_list(f, reconsed_args)
}

unsafe extern "C" fn native_symbol_fn_invoke(_: *const Function, sym: Object) -> Object {
    let sym = sym.unpack_symbol();
    let f = unlisp_rt_symbol_function(sym);
    Object::from_function(f)
}

unsafe extern "C" fn native_symbol_fn_apply(f: *const Function, args: List) -> Object {
    native_symbol_fn_invoke(f, args.first())
}

unsafe extern "C" fn native_set_macro_invoke(_: *const Function, f: Object) -> Object {
    let f = f.unpack_function();

    (*f).is_macro = true;

    Object::nil()
}

unsafe extern "C" fn native_set_macro_apply(f: *const Function, args: List) -> Object {
    native_set_macro_invoke(f, args.first())
}

unsafe extern "C" fn native_listp_invoke(_: *const Function, x: Object) -> Object {
    if x.ty == ObjType::List {
        Object::from_symbol(symbols::get_or_intern_symbol("true".to_string()))
    } else {
        Object::nil()
    }
}

unsafe extern "C" fn native_listp_apply(f: *const Function, args: List) -> Object {
    native_set_macro_invoke(f, args.first())
}

unsafe extern "C" fn native_symbolp_invoke(_: *const Function, x: Object) -> Object {
    if x.ty == ObjType::Symbol {
        Object::from_symbol(symbols::get_or_intern_symbol("true".to_string()))
    } else {
        Object::nil()
    }
}

unsafe extern "C" fn native_symbolp_apply(f: *const Function, args: List) -> Object {
    native_symbolp_invoke(f, args.first())
}

pub unsafe fn call_macro(f: *mut Function, args: List) -> Result<Object, RuntimeError> {
    assert!((*f).is_macro);

    let apply_fn: unsafe extern "C" fn(*const Function, List) -> Object =
        mem::transmute((*f).apply_to_f_ptr);

    exceptions::run_with_global_ex_handler(|| {
        if !unlisp_rt_check_arity(f, args.len) {
            exceptions::unlisp_rt_raise_arity_error((*f).name, (*f).arg_count, args.len);
        }
        apply_fn(f, args)
    })
}

unsafe extern "C" fn native_macroexpand_1_invoke(_: *const Function, form: Object) -> Object {
    match &form.ty {
        ObjType::List => {
            let list = form.unpack_list();
            if (*list).len == 0 {
                form
            } else {
                let first = (*list).first();

                match first.ty {
                    ObjType::Symbol => {
                        let sym = first.unpack_symbol();
                        let sym_fn = (*sym).function;

                        if sym_fn.is_null() || !(*sym_fn).is_macro {
                            form
                        } else {
                            match call_macro(sym_fn, (*list).rest()) {
                                Ok(expanded) => expanded,
                                Err(e) => exceptions::raise_error(format!("{}", e)),
                            }
                        }
                    }
                    _ => form,
                }
            }
        }
        _ => form,
    }
}

unsafe extern "C" fn native_macroexpand_1_apply(f: *const Function, args: List) -> Object {
    native_macroexpand_1_invoke(f, args.first())
}

unsafe extern "C" fn native_error_invoke(_: *const Function, msg: Object) -> ! {
    let s = msg.unpack_string();
    let rust_str = CStr::from_ptr(s).to_str().unwrap().to_string();
    exceptions::raise_error(rust_str)
}

unsafe extern "C" fn native_error_apply(f: *const Function, args: List) -> ! {
    native_error_invoke(f, args.first())
}

unsafe extern "C" fn native_print_invoke(_: *const Function, x: Object) -> Object {
    print!("{}", x);
    x
}

unsafe extern "C" fn native_print_apply(f: *const Function, args: List) -> Object {
    native_print_invoke(f, args.first())
}

unsafe extern "C" fn native_println_invoke(_: *const Function, x: Object) -> Object {
    println!("{}", x);
    x
}

unsafe extern "C" fn native_println_apply(f: *const Function, args: List) -> Object {
    native_println_invoke(f, args.first())
}

unsafe extern "C" fn native_stdout_write_invoke(_: *const Function, s: Object) -> Object {
    let s = s.unpack_string();
    let rust_str = CStr::from_ptr(s).to_str().unwrap().to_string();
    let _ = write!(std::io::stdout(), "{}", rust_str)
        .map_err(|e| exceptions::raise_error(format!("{}", e)));
    Object::nil()
}

unsafe extern "C" fn native_stdout_write_apply(f: *const Function, args: List) -> Object {
    native_stdout_write_invoke(f, args.first())
}

unsafe extern "C" fn native_set_val_invoke(_: *const Function, sym: Object, val: Object) -> Object {
    let sym = sym.unpack_symbol();
    (*sym).value = Box::into_raw(Box::new(val));

    Object::nil()
}

unsafe extern "C" fn native_set_val_apply(f: *const Function, args: List) -> Object {
    native_set_val_invoke(f, args.first(), args.rest().first())
}

unsafe extern "C" fn native_symbol_value_invoke(_: *const Function, sym: Object) -> Object {
    unlisp_rt_symbol_value(sym.unpack_symbol())
}

unsafe extern "C" fn native_symbol_value_apply(f: *const Function, args: List) -> Object {
    native_symbol_value_invoke(f, args.first())
}

unsafe extern "C" fn native_boundp_invoke(_: *const Function, sym: Object) -> Object {
    let sym = sym.unpack_symbol();
    if (*sym).value.is_null() {
        Object::nil()
    } else {
        Object::from_symbol(symbols::get_or_intern_symbol("true".to_string()))
    }
}

unsafe extern "C" fn native_boundp_apply(f: *const Function, args: List) -> Object {
    native_boundp_invoke(f, args.first())
}

unsafe extern "C" fn native_fboundp_invoke(_: *const Function, sym: Object) -> Object {
    let sym = sym.unpack_symbol();
    if (*sym).function.is_null() {
        Object::nil()
    } else {
        Object::from_symbol(symbols::get_or_intern_symbol("true".to_string()))
    }
}

unsafe extern "C" fn native_fboundp_apply(f: *const Function, args: List) -> Object {
    native_fboundp_invoke(f, args.first())
}


pub fn init() {
    init_symbol_fn(
        native_add_invoke as *const c_void,
        native_add_apply as *const c_void,
        "+",
        &[],
        true,
    );

    init_symbol_fn(
        native_sub_invoke as *const c_void,
        native_sub_apply as *const c_void,
        "-",
        &["x"],
        true,
    );

    init_symbol_fn(
        native_equal_invoke as *const c_void,
        native_equal_apply as *const c_void,
        "equal",
        &["x", "y"],
        false,
    );

    init_symbol_fn(
        native_set_fn_invoke as *const c_void,
        native_set_fn_apply as *const c_void,
        "set-symbol-function",
        &["sym", "func"],
        false,
    );

    init_symbol_fn(
        native_symbol_fn_invoke as *const c_void,
        native_symbol_fn_apply as *const c_void,
        "symbol-function",
        &["sym"],
        false,
    );

    init_symbol_fn(
        native_cons_invoke as *const c_void,
        native_cons_apply as *const c_void,
        "cons",
        &["x", "list"],
        false,
    );
    init_symbol_fn(
        native_rest_invoke as *const c_void,
        native_rest_apply as *const c_void,
        "rest",
        &["list"],
        false,
    );
    init_symbol_fn(
        native_first_invoke as *const c_void,
        native_first_apply as *const c_void,
        "first",
        &["list"],
        false,
    );

    init_symbol_fn(
        native_apply_invoke as *const c_void,
        native_apply_apply as *const c_void,
        "apply",
        &["f"],
        true,
    );

    init_symbol_fn(
        native_set_macro_invoke as *const c_void,
        native_set_macro_apply as *const c_void,
        "set-macro",
        &["f"],
        false,
    );

    init_symbol_fn(
        native_listp_invoke as *const c_void,
        native_listp_apply as *const c_void,
        "listp",
        &["x"],
        false,
    );

    init_symbol_fn(
        native_symbolp_invoke as *const c_void,
        native_symbolp_apply as *const c_void,
        "symbolp",
        &["x"],
        false,
    );

    init_symbol_fn(
        native_macroexpand_1_invoke as *const c_void,
        native_macroexpand_1_apply as *const c_void,
        "macroexpand-1",
        &["form"],
        false,
    );

    init_symbol_fn(
        native_error_invoke as *const c_void,
        native_error_apply as *const c_void,
        "error",
        &["msg"],
        false,
    );

    init_symbol_fn(
        native_print_invoke as *const c_void,
        native_print_apply as *const c_void,
        "print",
        &["x"],
        false,
    );

    init_symbol_fn(
        native_println_invoke as *const c_void,
        native_println_apply as *const c_void,
        "println",
        &["x"],
        false,
    );

    init_symbol_fn(
        native_stdout_write_invoke as *const c_void,
        native_stdout_write_apply as *const c_void,
        "stdout-write",
        &["s"],
        false,
    );

    init_symbol_fn(
        native_set_val_invoke as *const c_void,
        native_set_val_apply as *const c_void,
        "set-symbol-value",
        &["sym", "val"],
        false,
    );

    init_symbol_fn(
        native_symbol_value_invoke as *const c_void,
        native_symbol_value_apply as *const c_void,
        "symbol-value",
        &["sym"],
        false,
    );

    init_symbol_fn(
        native_boundp_invoke as *const c_void,
        native_boundp_apply as *const c_void,
        "boundp",
        &["sym"],
        false,
    );

    init_symbol_fn(
        native_fboundp_invoke as *const c_void,
        native_fboundp_apply as *const c_void,
        "fboundp",
        &["sym"],
        false,
    );
}
