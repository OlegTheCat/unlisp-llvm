use crate::defs::Symbol;
use libc::c_char;
use std::collections::HashMap;
use std::ffi::{CStr, CString};

type InternedSymbols = HashMap<String, *mut Symbol>;

static mut INTERNED_SYMBOLS: Option<InternedSymbols> = None;

pub fn interned_symbols_ref() -> &'static InternedSymbols {
    unsafe { INTERNED_SYMBOLS.as_ref().unwrap() }
}

pub fn interned_symbols_mut() -> &'static mut InternedSymbols {
    unsafe { INTERNED_SYMBOLS.as_mut().unwrap() }
}

fn get_or_intern_symbol_impl(name_raw: *const c_char, name: String) -> *mut Symbol {
    interned_symbols_ref().get(&name).map_or_else(
        || {
            let sym_pointer = Box::into_raw(Box::new(Symbol::new(name_raw)));
            interned_symbols_mut().insert(name.clone(), sym_pointer);
            sym_pointer
        },
        |p| *p,
    )
}

pub fn get_or_intern_symbol_by_ptr(name: *const c_char) -> *mut Symbol {
    let c_str = unsafe { CStr::from_ptr(name) };
    let string = c_str.to_str().unwrap().to_string();

    get_or_intern_symbol_impl(name, string)
}

pub fn get_or_intern_symbol(name: String) -> *mut Symbol {
    let c_str = CString::new(name.as_str()).expect("string conversion failed");
    let c_ptr: *const c_char = c_str.into_raw();

    get_or_intern_symbol_impl(c_ptr, name)
}

pub fn init() {
    unsafe {
        INTERNED_SYMBOLS = Some(HashMap::new());
    }
}
