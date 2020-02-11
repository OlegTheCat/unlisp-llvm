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

pub fn to_heap<T>(x: T) -> *mut T {
    Box::into_raw(Box::new(x))
}

// TODO: revise usage of Copy here
#[derive(Clone, Copy)]
#[repr(C)]
pub union UntaggedObject {
    int: i64,
    m_box: *mut MutableBox,
    cons: *mut Cons,
    sym: *mut Symbol,
    function: *mut Function,
    string: *const c_char,
}

#[derive(Clone, Eq, PartialEq)]
#[repr(C)]
pub enum ObjType {
    Int64 = 1,
    Box = 2,
    Symbol = 3,
    Function = 4,
    String = 5,
    Cons = 6
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let name = match self {
            ObjType::Int64 => "int",
            ObjType::Box => "box",
            ObjType::Function => "function",
            ObjType::Symbol => "symbol",
            ObjType::String => "string",
            ObjType::Cons => "cons",
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
                ObjType::Box => *self.obj.m_box == *rhs.obj.m_box,
                ObjType::Function => self.obj.function == rhs.obj.function,
                ObjType::Symbol => self.obj.sym == rhs.obj.sym,
                ObjType::String => strcmp(self.obj.string, rhs.obj.string) == 0,
                ObjType::Cons => *self.obj.cons == *rhs.obj.cons,
            }
        }
    }
}
#[repr(C)]
#[derive(Clone)]
pub struct MutableBox(*mut Object);

impl PartialEq for MutableBox {
    fn eq(&self, rhs: &Self) -> bool {
        unsafe {
            *self.0 == *rhs.0
        }
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct ListLike(*mut c_void);

impl ListLike {
    pub fn from_cons(c: *mut Cons) -> Self {
        Self(c as *mut c_void)
    }

    pub fn from_nil() -> Self {
        unsafe { Self(NIL as *mut c_void) }
    }

    pub fn is_nil(&self) -> bool{
        unsafe { self.0 == NIL as *mut c_void }
    }

    pub fn is_cons(&self) -> bool {
        !self.is_nil()
    }

    pub fn as_nil(&self) -> *mut Symbol {
        self.0 as *mut Symbol
    }

    pub fn as_cons(&self) -> *mut Cons {
        self.0 as *mut Cons
    }

    pub fn len(&self) -> u64 {
        if self.is_nil() {
            0
        } else {
            1 + self.cdr().len()
        }
    }

    pub fn to_object(&self) -> Object {
        if self.is_nil() {
            Object::nil()
        } else {
            Object::from_cons(self.as_cons())
        }
    }

    pub fn cons_ptr(&self, o: *mut Object) -> Self {
        let cons = Cons {
            car: o,
            cdr: to_heap(self.to_object())
        };

        Self::from_cons(to_heap(cons))
    }

    pub fn cons(&self, o: Object) -> Self {
        self.cons_ptr(to_heap(o))
    }

    pub fn car(&self) -> Object {
        if self.is_nil() {
            unsafe {
                exceptions::raise_error("cannot take element from nil".to_string())
            }
        }

        unsafe { (*self.as_cons()).car() }
    }

    pub fn cdr(&self) -> Self {
        if self.is_nil() {
            self.clone()
        } else {
            unsafe { (*self.as_cons()).cdr().unpack_list_like() }
        }
    }

    pub fn cdr_as_object(&self) -> Object {
        if self.is_nil() {
            Object::nil()
        } else {
            unsafe { (*self.as_cons()).cdr() }
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

    pub fn is_nil(&self) -> bool {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().is_nil();
        }

        self.ty == ObjType::Symbol &&
            unsafe {
                self.obj.sym == NIL
            }
    }

    pub fn unpack_int(&self) -> i64 {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().unpack_int();
        }

        if self.ty == ObjType::Int64 {
            unsafe { self.obj.int }
        } else {
            self.type_err(ObjType::Int64);
        }
    }

    pub fn unpack_cons(&self) -> *mut Cons {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().unpack_cons();
        }

        if self.ty == ObjType::Cons {
            unsafe { self.obj.cons }
        } else {
            self.type_err(ObjType::Cons);
        }
    }

    pub fn unpack_list_like(&self) -> ListLike {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().unpack_list_like();
        }

        if self.is_nil() {
            ListLike::from_nil()
        } else {
            ListLike::from_cons(self.unpack_cons())
        }
    }

    pub fn unpack_symbol(&self) -> *mut Symbol {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().unpack_symbol();
        }

        if self.ty == ObjType::Symbol {
            unsafe { self.obj.sym }
        } else {
            self.type_err(ObjType::Symbol);
        }
    }

    pub fn unpack_function(&self) -> *mut Function {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().unpack_function();
        }

        if self.ty == ObjType::Function {
            unsafe { self.obj.function }
        } else {
            self.type_err(ObjType::Function);
        }
    }

    pub fn unpack_string(&self) -> *const c_char {
        if self.ty == ObjType::Box {
            return self.unpack_underlying().unpack_string();
        }

        if self.ty == ObjType::String {
            unsafe { self.obj.string }
        } else {
            self.type_err(ObjType::String);
        }
    }

    pub fn unpack_box(&self) -> *mut MutableBox {
        if self.ty == ObjType::Box {
            // get the most underlying box
            unsafe {
                let b = self.obj.m_box;
                let b_obj = (*b).0;
                if (*b_obj).ty == ObjType::Box {
                    (*b_obj).unpack_box()
                } else {
                    b
                }
            }
        } else {
            self.type_err(ObjType::Box);
        }
    }

    pub fn unpack_underlying(&self) -> Object {
        unsafe { (*(*self.unpack_box()).0).clone() }
    }

    pub fn from_int(i: i64) -> Object {
        Self {
            ty: ObjType::Int64,
            obj: UntaggedObject { int: i },
        }
    }

    pub fn from_cons(cons: *mut Cons) -> Object {
        Self {
            ty: ObjType::Cons,
            obj: UntaggedObject { cons: cons },
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

    pub fn from_box(b: *mut MutableBox) -> Object {
        Self {
            ty: ObjType::Box,
            obj: UntaggedObject { m_box: b},
        }
    }

    pub fn nil() -> Object {
        unsafe { Object::from_symbol(NIL) }
    }

    pub fn t() -> Object {
        unsafe { Object::from_symbol(T) }
    }
}

unsafe fn display_cons(cons: *const Cons, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "(")?;
    write!(f, "{}", *(*cons).car)?;

    let mut obj = (*cons).cdr;
    while (*obj).ty == ObjType::Cons {
        let cons = (*obj).unpack_cons();
        write!(f, " {}", *(*cons).car)?;
        obj = (*cons).cdr;
    }

    if (*obj).is_nil() {
        write!(f, ")")?;
    } else {
        write!(f, " . {})", (*obj))?;
    }

    Ok(())
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        unsafe {
            match self.ty {
                ObjType::Int64 => write!(f, "{}", self.obj.int),
                ObjType::Cons => display_cons(self.obj.cons, f),
                ObjType::Box => write!(f, "{}", (*(*self.obj.m_box).0)),
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
#[derive(Clone)]
pub struct Cons {
    pub car: *mut Object,
    pub cdr: *mut Object
}

impl PartialEq for Cons {
    fn eq(&self, rhs: &Self) -> bool {
        unsafe {
            (self.car == rhs.car ||
             *(self.car) == *(rhs.car))
                && (self.cdr == rhs.cdr ||
                    *(self.car) == *(rhs.cdr))
        }
    }
}

impl Cons {
    #[cfg(feature = "llvm_defs")]
    pub fn gen_llvm_def(context: &Context, _module: &Module) {
        let i8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let struct_ty = context.opaque_struct_type("unlisp_rt_cons");

        struct_ty.set_body(&[i8_ptr_ty.into(), i8_ptr_ty.into()], false);
    }

    pub fn new(x: Object, y: Object) -> Self {
        Self {
            car: to_heap(x),
            cdr: to_heap(y)
        }
    }

    pub unsafe fn car(&self) -> Object {
        (*self.car).clone()
    }

    pub unsafe fn cdr(&self) -> Object {
        (*self.cdr).clone()
    }

    pub fn cons(&self, obj: Object) -> Self {
        Self::new(obj, Object::from_cons(to_heap(self.clone())))
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
pub extern "C" fn unlisp_rt_object_from_cons(cons: Cons) -> Object {
    Object::from_cons(to_heap(cons))
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_object_from_list(list: ListLike) -> Object {
    list.to_object()
}


#[runtime_fn]
pub extern "C" fn unlisp_rt_object_is_nil(o: Object) -> bool {
    o.is_nil()
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_nil_object() -> Object {
    Object::nil()
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_t_object() -> Object {
    Object::t()
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

pub fn obj_array_to_list_like(n: u64, arr: *mut Object, mut list_like: ListLike) -> ListLike {
    for i in (0..n).rev() {
        list_like = list_like.cons_ptr(unsafe { arr.offset(i as isize).clone() })
    }

    list_like
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_va_list_into_list(n: u64, va_list: VaList) -> Object {
    let obj_array = unsafe { va_list_to_obj_array(n, va_list) };
    obj_array_to_list_like(n, obj_array, ListLike::from_nil()).to_object()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_list_car(list: ListLike) -> Object {
    list.car()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_list_cdr(list: ListLike) -> ListLike {
    list.cdr()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_list_cons(el: Object, list: ListLike) -> ListLike {
    list.cons(el)
}

#[runtime_fn]
pub extern "C" fn unlisp_rt_init_runtime() {
    symbols::init();
    predefined::init();
    unsafe {
        let t = symbols::get_or_intern_symbol("t".to_string());
        let nil = symbols::get_or_intern_symbol("nil".to_string());

        (*t).value = to_heap(Object::from_symbol(t));
        (*nil).value = to_heap(Object::from_symbol(nil));

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

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_make_box(o: Object) -> Object {
    Object::from_box(to_heap(MutableBox(to_heap(o))))
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_box_ref(b: Object) -> Object {
    (*(*b.unpack_box()).0).clone()
}

#[runtime_fn]
pub unsafe extern "C" fn unlisp_rt_box_set(b: Object, val: Object) -> Object {
    (*b.unpack_box()).0 = to_heap(val.clone());
    val
}
