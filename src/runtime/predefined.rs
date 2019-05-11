use super::defs::*;
use super::symbols;

use std::mem;
use std::ptr;
use std::ffi::CString;
use libc::{c_void, c_char};

fn arr_to_raw(arr: &[&str]) -> *const *const c_char {
    let vec: Vec<_> = arr.iter().map(|s| CString::new(*s).unwrap().into_raw()).collect();
    let ptr = vec.as_ptr();

    mem::forget(vec);

    ptr as *const *const c_char
}

extern fn native_add(_: *const Function, x: Object, y: Object) -> Object {
    let x_int = x.unpack_int();
    let y_int = y.unpack_int();

    Object::from_int(x_int + y_int)
}

#[used]
static NATIVE_ADD: extern fn(*const Function, Object, Object) -> Object = native_add;

fn init_native_add() {
    let sym = symbols::get_or_intern_symbol("+".to_string());

    let arglist = ["x", "y"];

    let func = Function {
        ty: FunctionType::Function,
        name: CString::new("+").unwrap().into_raw(),
        arglist: arr_to_raw(&arglist),
        arg_count: 2,
        is_macro: false,
        invoke_f_ptr: native_add as *const c_void,
        apply_to_f_ptr: ptr::null()
    };

    let func = Box::into_raw(Box::new(func));

    unsafe { (*sym).function = func };
}

extern fn native_set_fn(_: *const Function, sym: Object, func: Object) -> Object {
    let sym = sym.unpack_symbol();
    let func = func.unpack_function();

    unsafe { (*sym).function = func };

    Object::nil()
}

#[used]
static NATIVE_SET_FN: extern fn(*const Function, Object, Object) -> Object = native_set_fn;

fn init_native_set_fn() {
    let sym = symbols::get_or_intern_symbol("set-fn".to_string());

    let arglist = ["sym", "func"];

    let func = Function {
        ty: FunctionType::Function,
        name: CString::new("set-fn").unwrap().into_raw(),
        arglist: arr_to_raw(&arglist),
        arg_count: 2,
        is_macro: false,
        invoke_f_ptr: native_set_fn as *const c_void,
        apply_to_f_ptr: ptr::null()
    };

    let func = Box::into_raw(Box::new(func));

    unsafe { (*sym).function = func };
}

pub fn init() {
    init_native_add();
    init_native_set_fn();
}
