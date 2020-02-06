use crate::defs::*;
use crate::error::RuntimeError;
use crate::exceptions;
use crate::symbols;

use unlisp_internal_macros::trivial_apply;

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

unsafe extern "C" fn native_add_apply(_: *const Function, args: ListLike) -> Object {
    let mut sum = 0;
    let mut cur_args = args;

    while !cur_args.is_nil() {
        sum += cur_args.car().unpack_int();
        cur_args = cur_args.cdr();
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

unsafe extern "C" fn native_sub_apply(_: *const Function, args: ListLike) -> Object {
    let mut result = args.car().unpack_int();

    let mut cur_args = args.cdr();

    while !cur_args.is_nil() {
        result -= cur_args.car().unpack_int();
        cur_args = cur_args.cdr();
    }

    Object::from_int(result)
}

#[trivial_apply]
extern "C" fn native_equal_invoke(_: *const Function, x: Object, y: Object) -> Object {
    if x == y {
        x
    } else {
        Object::nil()
    }
}

#[trivial_apply]
extern "C" fn native_set_fn_invoke(_: *const Function, sym: Object, func: Object) -> Object {
    let sym = sym.unpack_symbol();
    let func = func.unpack_function();

    unsafe { (*sym).function = func };

    Object::nil()
}

#[trivial_apply]
extern "C" fn native_cons_invoke(_: *const Function, x: Object, y: Object) -> Object {
    Object::from_cons(to_heap(Cons::new(x, y)))
}

#[trivial_apply]
extern "C" fn native_rest_invoke(_: *const Function, list_like: Object) -> Object {
    list_like.unpack_list_like().cdr_as_object()
}

#[trivial_apply]
unsafe extern "C" fn native_first_invoke(_: *const Function, list_like: Object) -> Object {
    list_like.unpack_list_like().car()
}

unsafe fn apply_to_list_like(f: *const Function, args: ListLike) -> Object {
    let len = args.len();
    if !unlisp_rt_check_arity(f, len) {
        exceptions::unlisp_rt_raise_arity_error((*f).name, (*f).arg_count, len);
    }

    let apply_fn: unsafe extern "C" fn(*const Function, ListLike) -> Object =
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
    let last_arg = (*args_arr.offset((n as isize) - 1)).unpack_list_like();
    let args = obj_array_to_list_like(n - 1, args_arr, last_arg);

    apply_to_list_like(f, args)
}

unsafe extern "C" fn native_apply_apply(_: *const Function, args: ListLike) -> Object {
    let f = args.car().unpack_function();

    let mut to_cons = vec![];
    let mut f_args = args.cdr();

    loop {
        let f_cdr = f_args.cdr();
        if f_cdr.is_nil() {
            break;
        }
        to_cons.push(f_args.car());
        f_args = f_cdr;
    }

    let cons_base = f_args.car().unpack_list_like();

    let reconsed_args = to_cons
        .into_iter()
        .rev()
        .fold(cons_base, |acc, item| acc.cons(item));

    apply_to_list_like(f, reconsed_args)
}

#[trivial_apply]
unsafe extern "C" fn native_symbol_fn_invoke(_: *const Function, sym: Object) -> Object {
    let sym = sym.unpack_symbol();
    let f = unlisp_rt_symbol_function(sym);
    Object::from_function(f)
}

#[trivial_apply]
unsafe extern "C" fn native_set_macro_invoke(_: *const Function, f: Object) -> Object {
    let f = f.unpack_function();

    (*f).is_macro = true;

    Object::nil()
}

#[trivial_apply]
unsafe extern "C" fn native_listp_invoke(_: *const Function, x: Object) -> Object {
    if x.is_nil() || x.ty == ObjType::Cons {
        Object::t()
    } else {
        Object::nil()
    }
}

#[trivial_apply]
unsafe extern "C" fn native_symbolp_invoke(_: *const Function, x: Object) -> Object {
    if x.ty == ObjType::Symbol {
        Object::t()
    } else {
        Object::nil()
    }
}

pub unsafe fn call_macro(f: *mut Function, args: ListLike) -> Result<Object, RuntimeError> {
    assert!((*f).is_macro);

    let apply_fn: unsafe extern "C" fn(*const Function, ListLike) -> Object =
        mem::transmute((*f).apply_to_f_ptr);

    exceptions::run_with_global_ex_handler(|| {
        let len = args.len();
        if !unlisp_rt_check_arity(f, len) {
            exceptions::unlisp_rt_raise_arity_error((*f).name, (*f).arg_count, len);
        }
        apply_fn(f, args)
    })
}

#[trivial_apply]
unsafe extern "C" fn native_macroexpand_1_invoke(_: *const Function, form: Object) -> Object {
    match &form.ty {
        ObjType::Cons => {
            let cons = form.unpack_cons();
            let car = (*cons).car();
            match car.ty {
                ObjType::Symbol => {
                    let sym = car.unpack_symbol();
                    let sym_fn = (*sym).function;

                    if sym_fn.is_null() || !(*sym_fn).is_macro {
                        form
                    } else {
                        match call_macro(sym_fn, (*cons).cdr().unpack_list_like()) {
                            Ok(expanded) => expanded,
                            Err(e) => exceptions::raise_error(format!("{}", e)),
                        }
                    }
                }
                _ => form
            }
        }
        _ => form
    }
}

#[trivial_apply]
unsafe extern "C" fn native_error_invoke(_: *const Function, msg: Object) -> ! {
    let s = msg.unpack_string();
    let rust_str = CStr::from_ptr(s).to_str().unwrap().to_string();
    exceptions::raise_error(rust_str)
}

#[trivial_apply]
unsafe extern "C" fn native_print_invoke(_: *const Function, x: Object) -> Object {
    print!("{}", x);
    x
}

#[trivial_apply]
unsafe extern "C" fn native_println_invoke(_: *const Function, x: Object) -> Object {
    println!("{}", x);
    x
}

#[trivial_apply]
unsafe extern "C" fn native_stdout_write_invoke(_: *const Function, s: Object) -> Object {
    let s = s.unpack_string();
    let rust_str = CStr::from_ptr(s).to_str().unwrap().to_string();
    let _ = write!(std::io::stdout(), "{}", rust_str)
        .map_err(|e| exceptions::raise_error(format!("{}", e)));
    Object::nil()
}

#[trivial_apply]
unsafe extern "C" fn native_set_val_invoke(_: *const Function, sym: Object, val: Object) -> Object {
    let sym = sym.unpack_symbol();
    (*sym).value = Box::into_raw(Box::new(val));

    Object::nil()
}

#[trivial_apply]
unsafe extern "C" fn native_symbol_value_invoke(_: *const Function, sym: Object) -> Object {
    unlisp_rt_symbol_value(sym.unpack_symbol())
}

#[trivial_apply]
unsafe extern "C" fn native_boundp_invoke(_: *const Function, sym: Object) -> Object {
    let sym = sym.unpack_symbol();
    if (*sym).value.is_null() {
        Object::nil()
    } else {
        Object::from_symbol(symbols::get_or_intern_symbol("true".to_string()))
    }
}

#[trivial_apply]
unsafe extern "C" fn native_fboundp_invoke(_: *const Function, sym: Object) -> Object {
    let sym = sym.unpack_symbol();
    if (*sym).function.is_null() {
        Object::nil()
    } else {
        Object::from_symbol(symbols::get_or_intern_symbol("true".to_string()))
    }
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
