use super::defs::*;
use super::symbols;

use libc::{c_char, c_void};
use std::ffi::CString;
use std::mem;
use std::ptr;

fn arr_to_raw(arr: &[&str]) -> *const *const c_char {
    let vec: Vec<_> = arr
        .iter()
        .map(|s| CString::new(*s).unwrap().into_raw())
        .collect();
    let ptr = vec.as_ptr();

    mem::forget(vec);

    ptr as *const *const c_char
}

fn init_symbol_fn(f: *const c_void, name: &str, arglist: &[&str]) {
    let sym = symbols::get_or_intern_symbol(name.to_string());

    let func = Function {
        ty: FunctionType::Function,
        name: CString::new(name).unwrap().into_raw(),
        arglist: arr_to_raw(arglist),
        arg_count: (arglist.len() as u64),
        is_macro: false,
        invoke_f_ptr: f,
        apply_to_f_ptr: ptr::null(),
    };

    let func = Box::into_raw(Box::new(func));

    unsafe { (*sym).function = func };
}

extern "C" fn native_add(_: *const Function, x: Object, y: Object) -> Object {
    let x_int = x.unpack_int();
    let y_int = y.unpack_int();

    Object::from_int(x_int + y_int)
}

fn init_native_add() {
    init_symbol_fn(native_add as *const c_void, "+", &["x", "y"]);
}

extern "C" fn native_sub(_: *const Function, x: Object, y: Object) -> Object {
    let x_int = x.unpack_int();
    let y_int = y.unpack_int();

    Object::from_int(x_int - y_int)
}

fn init_native_sub() {
    init_symbol_fn(native_sub as *const c_void, "-", &["x", "y"]);
}

extern "C" fn native_int_eq(_: *const Function, x: Object, y: Object) -> Object {
    let x_int = x.unpack_int();
    let y_int = y.unpack_int();

    if x_int == y_int {
        x
    } else {
        Object::nil()
    }
}

fn init_native_int_eq() {
    init_symbol_fn(native_int_eq as *const c_void, "int-eq", &["x", "y"]);
}

extern "C" fn native_set_fn(_: *const Function, sym: Object, func: Object) -> Object {
    let sym = sym.unpack_symbol();
    let func = func.unpack_function();

    unsafe { (*sym).function = func };

    Object::nil()
}

fn init_native_set_fn() {
    init_symbol_fn(native_set_fn as *const c_void, "set-fn", &["sym", "func"]);
}

extern "C" fn native_cons(_: *const Function, x: Object, list: Object) -> Object {
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

fn init_native_cons() {
    init_symbol_fn(native_cons as *const c_void, "cons", &["x", "list"]);
}

extern "C" fn native_rest(_: *const Function, list: Object) -> Object {
    let list = list.unpack_list();
    let len = unsafe { (*list).len };

    if len == 0 {
        Object::nil()
    } else {
        let rest = unsafe {
            (*(*list).node).next
        };
        Object::from_list(rest)
    }
}

fn init_native_rest() {
    init_symbol_fn(native_rest as *const c_void, "rest", &["list"]);
}

extern "C" fn native_first(_: *const Function, list: Object) -> Object {
    let list = list.unpack_list();
    let len = unsafe { (*list).len };

    if len == 0 {
        panic!("cannot do first on empty list");
    } else {
        unsafe {
            (*(*(*list).node).val).clone()
        }
    }
}

fn init_native_first() {
    init_symbol_fn(native_first as *const c_void, "first", &["list"]);
}

pub fn init() {
    init_native_add();
    init_native_sub();
    init_native_int_eq();

    init_native_set_fn();

    init_native_cons();
    init_native_rest();
    init_native_first();
}
