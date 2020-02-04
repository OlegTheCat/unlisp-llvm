use libc::c_char;
use libc::c_void;
use libc::strcmp;
use std::ffi::CStr;
use std::ffi::VaList;
use std::fmt;
use std::ptr;

#[cfg(feature = "llvm_defs")]
use inkwell::context::Context;
#[cfg(feature = "llvm_defs")]
use inkwell::module::Module;
#[cfg(feature = "llvm_defs")]
use inkwell::AddressSpace;

use crate::{exceptions, predefined, symbols};
use unlisp_internal_macros::runtime_fn;

// TODO: use lazy_static here
static mut T: *mut Symbol = ptr::null_mut();
static mut NIL: *mut Symbol = ptr::null_mut();

// TODO: revise usage of Copy here
#[derive(Clone, Copy)]
#[repr(C)]
pub union UntaggedObject {
    int: i64,
    list: *mut List,
    sym: *mut Symbol,
    function: *mut Function,
    string: *const c_char,
}

#[derive(Clone, Eq, PartialEq)]
#[repr(C)]
pub enum ObjType {
    Int64 = 1,
    List = 2,
    Symbol = 3,
    Function = 4,
    String = 5,
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let name = match self {
            ObjType::Int64 => "int",
            ObjType::List => "list",
            ObjType::Function => "function",
            ObjType::Symbol => "symbol",
            ObjType::String => "string",
        };

        write!(f, "{}", name)
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct Object {
    pub ty: ObjType,
    pub obj: UntaggedObject,
}

impl PartialEq for Object {
    fn eq(&self, rhs: &Self) -> bool {
        if self.ty != rhs.ty {
            return false;
        }

        unsafe {
            match self.ty {
                ObjType::Int64 => self.obj.int == rhs.obj.int,
                ObjType::List => *self.obj.list == *rhs.obj.list,
                ObjType::Function => self.obj.function == rhs.obj.function,
                ObjType::Symbol => self.obj.sym == rhs.obj.sym,
                ObjType::String => strcmp(self.obj.string, rhs.obj.string) == 0,
            }
        }
    }
}

impl Object {
    #[cfg(feature = "llvm_defs")]
    pub fn gen_llvm_def(context: &Context, _module: &Module) {
        let int8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let int32_ty = context.i32_type();

        let struct_ty = context.opaque_struct_type("unlisp_rt_object");
        struct_ty.set_body(&[int32_ty.into(), int8_ptr_ty.into()], false);
    }

    fn type_err(&self, target_ty: ObjType) -> ! {
        unsafe { exceptions::raise_cast_error(format!("{}", self.ty), format!("{}", target_ty)) };
    }

    pub fn unpack_int(&self) -> i64 {
        if self.ty == ObjType::Int64 {
            unsafe { self.obj.int }
        } else {
            self.type_err(ObjType::Int64);
        }
    }

    pub fn unpack_list(&self) -> *mut List {
        if self.ty == ObjType::List {
            unsafe { self.obj.list }
        } else {
            self.type_err(ObjType::List);
        }
    }

    pub fn unpack_symbol(&self) -> *mut Symbol {
        if self.ty == ObjType::Symbol {
            unsafe { self.obj.sym }
        } else {
            self.type_err(ObjType::Symbol);
        }
    }

    pub fn unpack_function(&self) -> *mut Function {
        if self.ty == ObjType::Function {
            unsafe { self.obj.function }
        } else {
            self.type_err(ObjType::Function);
        }
    }

    pub fn unpack_string(&self) -> *const c_char {
        if self.ty == ObjType::String {
            unsafe { self.obj.string }
        } else {
            self.type_err(ObjType::String);
        }
    }

    pub fn from_int(i: i64) -> Object {
        Self {
            ty: ObjType::Int64,
            obj: UntaggedObject { int: i },
        }
    }

    pub fn from_list(list: *mut List) -> Object {
        Self {
            ty: ObjType::List,
            obj: UntaggedObject { list: list },
        }
    }

    pub fn from_symbol(sym: *mut Symbol) -> Object {
        Self {
            ty: ObjType::Symbol,
            obj: UntaggedObject { sym: sym },
        }
    }

    pub fn from_function(function: *mut Function) -> Object {
        Self {
            ty: ObjType::Function,
            obj: UntaggedObject { function: function },
        }
    }

    pub fn from_string(string: *const c_char) -> Object {
        Self {
            ty: ObjType::String,
            obj: UntaggedObject { string: string },
        }
    }

    pub fn nil() -> Object {
        let list = List::empty();

        Object::from_list(Box::into_raw(Box::new(list)))
    }
}

unsafe fn display_list(mut list: *const List, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    if (*list).len == 0 {
        write!(f, "nil")?;
    } else {
        write!(f, "(")?;
        let mut is_first = true;

        while (*list).len != 0 {
            let val = &(*(*(*list).node).val);
            if is_first {
                write!(f, "{}", val)?;
                is_first = false;
            } else {
                write!(f, " {}", val)?;
            }

            list = (*(*list).node).next;
        }

        write!(f, ")")?;
    }

    Ok(())
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        unsafe {
            match self.ty {
                ObjType::Int64 => write!(f, "{}", self.obj.int),
                ObjType::List => display_list(self.obj.list, f),
                ObjType::Function => write!(
                    f,
                    "#<FUNCTION{}/{}>",
                    if (*self.obj.function).is_macro {
                        "+MACRO"
                    } else {
                        ""
                    },
                    (*self.obj.function).arg_count
                ),
                ObjType::Symbol => write!(
                    f,
                    "{}",
                    CStr::from_ptr((*self.obj.sym).name).to_str().unwrap()
                ),
                ObjType::String => write!(
                    f,
                    "\"{}\"",
                    CStr::from_ptr(self.obj.string).to_str().unwrap()
                ),
            }
        }
    }
}

#[repr(C)]
pub struct Node {
    pub val: *mut Object,
    pub next: *mut List,
}

#[repr(C)]
#[derive(Clone)]
pub struct List {
    pub node: *mut Node,
    pub len: u64,
}

impl PartialEq for List {
    fn eq(&self, rhs: &Self) -> bool {
        if self.len != rhs.len {
            return false;
        }

        if self.len == 0 {
            return true;
        }

        unsafe {
            *((*self.node).val) == *((*rhs.node).val) && *((*self.node).next) == *((*rhs.node).next)
        }
    }
}

impl List {
    #[cfg(feature = "llvm_defs")]
    pub fn gen_llvm_def(context: &Context, _module: &Module) {
        let i64_ty = context.i64_type();
        let i8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let struct_ty = context.opaque_struct_type("unlisp_rt_list");

        struct_ty.set_body(&[i8_ptr_ty.into(), i64_ty.into()], false);
    }

    pub fn empty() -> List {
        List {
            node: ptr::null_mut(),
            len: 0,
        }
    }

    pub unsafe fn first(&self) -> Object {
        (*(*self.node).val).clone()
    }

    pub unsafe fn rest(&self) -> List {
        (*self.rest_ptr()).clone()
    }

    pub unsafe fn rest_ptr(&self) -> *mut List {
        (*self.node).next
    }

    pub fn cons(&self, obj: Object) -> List {
        List {
            len: self.len + 1,
            node: Box::into_raw(Box::new(Node {
                val: Box::into_raw(Box::new(obj)),
                next: Box::into_raw(Box::new(self.clone())),
            })),
        }
    }
}

#[repr(C)]
pub struct Symbol {
    pub name: *const c_char,
    pub function: *mut Function,
    pub value: *mut Object,
}

impl Symbol {
    #[cfg(feature = "llvm_defs")]
    pub fn gen_llvm_def(context: &Context, module: &Module) {
        let func_struct_ty = module
            .get_type("unlisp_rt_function")
            .unwrap()
            .into_struct_type();

        let obj_struct_ty = module
            .get_type("unlisp_rt_object")
            .unwrap()
            .into_struct_type();

        let name_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let func_ptr_ty = func_struct_ty.ptr_type(AddressSpace::Generic);
        let value_ptr_ty = obj_struct_ty.ptr_type(AddressSpace::Generic);

        let struct_ty = context.opaque_struct_type("unlisp_rt_symbol");

        struct_ty.set_body(
            &[name_ptr_ty.into(), func_ptr_ty.into(), value_ptr_ty.into()],
            false,
        );
    }

    pub fn new(name: *const c_char) -> Self {
        Self {
            name: name,
            function: ptr::null_mut(),
            value: ptr::null_mut(),
        }
    }
}

#[repr(C)]
#[allow(dead_code)]
pub enum FunctionType {
    Function = 0,
    Closure = 1,
}

#[repr(C)]
pub struct Function {
    pub ty: FunctionType,
    pub name: *const c_char,
    pub arglist: *const *const c_char,
    pub arg_count: u64,
    pub is_macro: bool,
    pub invoke_f_ptr: *const c_void,
    pub apply_to_f_ptr: *const c_void,
    pub has_restarg: bool,
}

impl Function {
    pub const FIELDS_COUNT: u32 = 8;

    #[cfg(feature = "llvm_defs")]
    pub fn gen_llvm_def(context: &Context, _module: &Module) {
        let fn_struct_ty = context.opaque_struct_type("unlisp_rt_function");

        let ty_ty = context.i32_type();
        let ty_name = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_arglist = context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .ptr_type(AddressSpace::Generic);
        let ty_arg_count = context.i64_type();
        let ty_is_macro = context.bool_type();
        let ty_invoke_f_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_apply_to_f_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_has_restarg = context.bool_type();

        fn_struct_ty.set_body(
            &[
                ty_ty.into(),
                ty_name.into(),
                ty_arglist.into(),
                ty_arg_count.into(),
                ty_is_macro.into(),
                ty_invoke_f_ptr.into(),
                ty_apply_to_f_ptr.into(),
                ty_has_restarg.into(),
            ],
            false,
        );
    }
}

#[cfg(feature = "llvm_defs")]
pub fn va_gen_llvm_def(ctx: &Context, module: &Module) {
    let i32_ty = ctx.i32_type();
    let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);

    let va_list_ty = ctx.opaque_struct_type("va_list");

    va_list_ty.set_body(
        &[
            i32_ty.into(),
            i32_ty.into(),
            i8_ptr_ty.into(),
            i8_ptr_ty.into(),
        ],
        false,
    );

    let va_start_end_ty = ctx.void_type().fn_type(&[i8_ptr_ty.into()], false);
    module.add_function("llvm.va_start", va_start_end_ty, None);
    module.add_function("llvm.va_end", va_start_end_ty, None);
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_intern_sym(name: *const c_char) -> *mut Symbol {
    symbols::get_or_intern_symbol_by_ptr(name)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_from_int(i: i64) -> Object {
    Object::from_int(i)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_from_string(string: *const c_char) -> Object {
    Object::from_string(string)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_int_from_obj(o: Object) -> i64 {
    o.unpack_int()
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_from_function(f: *mut Function) -> Object {
    Object::from_function(f)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_from_symbol(s: *mut Symbol) -> Object {
    Object::from_symbol(s)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_from_list(list: List) -> Object {
    Object::from_list(Box::into_raw(Box::new(list)))
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_is_nil(o: Object) -> bool {
    o.ty == ObjType::Symbol &&
        unsafe {
            o.obj.sym == NIL
        }
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_nil_object() -> Object {
    unsafe { Object::from_symbol(NIL) }
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_t_object() -> Object {
    unsafe { Object::from_symbol(T) }
}


#[runtime_fn]
pub extern "C" fn unlisp_rt_check_arity(f: *const Function, arg_count: u64) -> bool {
    let has_restarg = unsafe { (*f).has_restarg };
    let params_count = unsafe { (*f).arg_count };

    let is_incorrect = (arg_count < params_count) || (!has_restarg && params_count != arg_count);

    !is_incorrect
}

extern "C" {
    pub fn va_list_to_obj_array(n: u64, list: VaList) -> *mut Object;
}

pub fn obj_array_to_list(n: u64, arr: *mut Object, list: Option<*mut List>) -> *mut List {
    let mut list = list.unwrap_or_else(|| {
        Box::into_raw(Box::new(List {
            node: ptr::null_mut(),
            len: 0,
        }))
    });

    for i in (0..n).rev() {
        list = Box::into_raw(Box::new(List {
            len: unsafe { (*list).len } + 1,
            node: Box::into_raw(Box::new(Node {
                val: unsafe { arr.offset(i as isize) },
                next: list,
            })),
        }))
    }

    list
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_va_list_into_list(n: u64, va_list: VaList) -> Object {
    let obj_array = unsafe { va_list_to_obj_array(n, va_list) };
    let list = obj_array_to_list(n, obj_array, None);

    Object::from_list(list)
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_list_first(list: List) -> Object {
    list.first()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_list_rest(list: List) -> List {
    list.rest()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_list_cons(el: Object, list: List) -> List {
    list.cons(el)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_empty_list() -> List {
    let list = List {
        node: ptr::null_mut(),
        len: 0,
    };

    list
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_init_runtime() {
    symbols::init();
    predefined::init();
    unsafe {
        let t = symbols::get_or_intern_symbol("t".to_string());
        let nil = symbols::get_or_intern_symbol("nil".to_string());

        (*t).value = Box::into_raw(Box::new(Object::from_symbol(t)));
        (*nil).value = Box::into_raw(Box::new(Object::from_symbol(nil)));

        T = t;
        NIL = nil;
    }
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_symbol_value(sym: *mut Symbol) -> Object {
    let val = (*sym).value;

    if val.is_null() {
        let rsym_name = CStr::from_ptr((*sym).name).to_str().unwrap().to_string();
        exceptions::raise_error(format!("unbound symbol: {}", rsym_name))
    }

    (*val).clone()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_symbol_function(sym: *mut Symbol) -> *mut Function {
    let f = (*sym).function;

    if f.is_null() {
        let rsym_name = CStr::from_ptr((*sym).name).to_str().unwrap().to_string();
        exceptions::raise_error(format!("undefined function: {}", rsym_name))
    }

    f
}
