use libc::c_char;
use libc::c_void;
use std::collections::HashMap;
use std::ffi::CStr;
use std::ptr;

type InternedSymbols = HashMap<String, *mut Symbol>;

static mut interned_symbols: Option<InternedSymbols> = None;

fn interned_symbols_ref() -> &'static InternedSymbols {
    unsafe {
        interned_symbols.as_ref().unwrap()
    }
}

fn interned_symbols_mut() -> &'static mut InternedSymbols {
    unsafe {
        interned_symbols.as_mut().unwrap()
    }
}

#[repr(C)]
pub union UntaggedObject {
    int: i64,
    list: *mut List,
    sym: Symbol
}

#[repr(C)]
pub enum ObjType {
    Int64 = 0,
    List = 1,
    Symbol = 2,
}

#[repr(C)]
pub struct Object {
    ty: ObjType,
    obj: UntaggedObject
}

#[repr(C)]
pub struct List {
    val: *mut Object,
    next: *mut List
}

#[repr(C)]
pub struct Symbol {
    name: *const c_char,
    function: *const Function
}

impl Symbol {
    fn new(name: *const c_char) -> Self {
        Self {
            name: name,
            function: ptr::null()
        }
    }
}

#[repr(C)]
pub struct Function {
    name: *const c_char,
    fpointer: *const c_void,
    is_macro: bool

}

pub fn get_or_intern_symbol(name: *const c_char) -> *mut Symbol {
    let c_str = unsafe { CStr::from_ptr(name) };
    let string = c_str.to_str().unwrap().to_string();

    interned_symbols_ref().get(&string)
        .map_or_else(|| {
            let sym_pointer = Box::into_raw(Box::new(Symbol::new(name)));
            interned_symbols_mut().insert(string.clone(), sym_pointer);
            sym_pointer
        }, |p| *p)
}

pub fn init_runtime() {
    unsafe {
        interned_symbols = Some(HashMap::new());
    }
}

#[no_mangle]
pub extern fn unlisp_rt_intern_sym(name: *const c_char) -> *const Symbol {
    get_or_intern_symbol(name)
}
