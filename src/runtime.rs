use libc::c_char;
use libc::c_void;
use std::collections::HashMap;
use std::ffi::CStr;
use std::ptr;
use std::fmt;

use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::AddressSpace;

type InternedSymbols = HashMap<String, *mut Symbol>;

pub static mut interned_symbols: Option<InternedSymbols> = None;

fn interned_symbols_ref() -> &'static InternedSymbols {
    unsafe { interned_symbols.as_ref().unwrap() }
}

fn interned_symbols_mut() -> &'static mut InternedSymbols {
    unsafe { interned_symbols.as_mut().unwrap() }
}

#[repr(C)]
pub union UntaggedObject {
    int: i64,
    list: *mut List,
    sym: *mut Symbol,
    nil: *const c_void
}

#[derive(Clone, Eq, PartialEq)]
#[repr(C)]
pub enum ObjType {
    Int64 = 0,
    List = 1,
    Symbol = 2,
    Nil = 3
}

impl ObjType {
    fn as_i32(&self) -> i32 {
        (*self).clone() as i32
    }
}

#[repr(C)]
pub struct Object {
    ty: ObjType,
    obj: UntaggedObject,
}

impl Object {
    fn gen_llvm_def(context: &Context) {
        let int8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let int32_ty = context.i32_type();

        let struct_ty = context.opaque_struct_type("unlisp_rt_object");
        struct_ty.set_body(&[int8_ptr_ty.into(), int32_ty.into()], false);
    }

    fn type_err(&self, t_name: &str) -> ! {
        panic!("Cannot unpack Object of type {} as {}", self.ty.as_i32(), t_name);
    }

    fn unpack_int(&self) -> i64 {
        if self.ty == ObjType::Int64 {
            unsafe { self.obj.int }
        } else {
            self.type_err("i64");
        }
    }

    fn unpack_list(&self) -> *mut List {
        if self.ty == ObjType::List {
            unsafe { self.obj.list }
        } else {
            self.type_err("list");
        }
    }

    fn unpack_symbol(&self) -> *mut Symbol {
        if self.ty == ObjType::Symbol {
            unsafe { self.obj.sym }
        } else {
            self.type_err("symbol");
        }
    }

    fn unpack_nil(&self) -> *const c_void {
        if self.ty == ObjType::Nil {
            unsafe { self.obj.nil }
        } else {
            self.type_err("nil");
        }
    }

    fn from_int(i: i64) -> Object {
        Self {
            ty: ObjType::Int64,
            obj: UntaggedObject { int: i }
        }
    }

    fn from_list(list: *mut List) -> Object {
        Self {
            ty: ObjType::List,
            obj: UntaggedObject { list: list }
        }
    }

    fn from_symbol(sym: *mut Symbol) -> Object {
        Self {
            ty: ObjType::Symbol,
            obj: UntaggedObject { sym: sym }
        }
    }

    fn nil() -> Object {
        Self {
            ty: ObjType::Nil,
            obj: UntaggedObject { nil: ptr::null() }
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.ty {
            ObjType::Int64 => write!(f, "Object[int64, {}]", unsafe { self.obj.int }),
            ObjType::List => write!(f, "Object[list, ...]"),
            ObjType::Nil => write!(f, "Object[nil]"),
            _ => panic!("unsupported type")
        }
    }
}

#[repr(C)]
pub struct List {
    val: *mut Object,
    next: *mut List,
}

impl List {
    fn gen_llvm_def(context: &Context) {
        let int8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);

        let struct_ty = context.opaque_struct_type("unlisp_rt_list");
        struct_ty.set_body(&[int8_ptr_ty.into(), int8_ptr_ty.into()], false);
    }
}

#[repr(C)]
pub struct Symbol {
    pub name: *const c_char,
    function: *const Function,
}

impl Symbol {
    fn gen_llvm_def(context: &Context) {
        let func_struct_ty = context.opaque_struct_type("unlisp_rt_function");

        let name_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let func_ptr_ty = func_struct_ty.ptr_type(AddressSpace::Generic);

        let struct_ty = context.opaque_struct_type("unlisp_rt_symbol");

        struct_ty.set_body(&[name_ptr_ty.into(), func_ptr_ty.into()], false);
    }

    pub fn new(name: *const c_char) -> Self {
        Self {
            name: name,
            function: ptr::null(),
        }
    }
}

#[repr(C)]
pub struct Function {
    name: *const c_char,
    f_ptr: *const c_void,
    is_macro: bool,
}

impl Function {
    fn gen_llvm_def(context: &Context) {
        let struct_ty = context.opaque_struct_type("unlisp_rt_function");

        let name_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let func_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let is_macro_ty = context.bool_type();

        struct_ty.set_body(
            &[name_ptr_ty.into(), func_ptr_ty.into(), is_macro_ty.into()],
            false,
        );
    }
}

pub fn get_or_intern_symbol(name: *const c_char) -> *mut Symbol {
    let c_str = unsafe { CStr::from_ptr(name) };
    let string = c_str.to_str().unwrap().to_string();

    interned_symbols_ref().get(&string).map_or_else(
        || {
            let sym_pointer = Box::into_raw(Box::new(Symbol::new(name)));
            interned_symbols_mut().insert(string.clone(), sym_pointer);
            sym_pointer
        },
        |p| *p,
    )
}

pub fn init(ctx: &Context, module: &Module) {
    unsafe {
        interned_symbols = Some(HashMap::new());
    }

    Object::gen_llvm_def(ctx);
    List::gen_llvm_def(ctx);
    Symbol::gen_llvm_def(ctx);
    Function::gen_llvm_def(ctx);

    unlisp_rt_intern_sym_gen_def(ctx, module);
    unlisp_rt_object_from_int_gen_def(ctx, module);
    unlisp_rt_int_from_obj_gen_def(ctx, module);
    unlisp_rt_set_fn_for_sym_gen_def(ctx, module);
    unlisp_rt_f_ptr_from_sym_gen_def(ctx, module);
}

#[no_mangle]
pub extern fn unlisp_rt_intern_sym(name: *const c_char) -> *mut Symbol {
    get_or_intern_symbol(name)
}

#[used]
static __INTERN: extern "C" fn(name: *const c_char) -> *mut Symbol = unlisp_rt_intern_sym;

fn unlisp_rt_intern_sym_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let sym_struct_ty = module.get_type("unlisp_rt_symbol").unwrap();
    let sym_struct_ptr_ty = sym_struct_ty.as_struct_type().ptr_type(AddressSpace::Generic);

    let fn_type = sym_struct_ptr_ty.fn_type(&[arg_ty.into()], false);
    module.add_function("unlisp_rt_intern_sym", fn_type, Some(Linkage::External));
}

#[no_mangle]
pub extern fn unlisp_rt_object_from_int(i: i64) -> Object {
    Object::from_int(i)
}

#[used]
static __OBJ_FROM_INT: extern "C" fn(i: i64) -> Object = unlisp_rt_object_from_int;

fn unlisp_rt_object_from_int_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i64_type();
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function("unlisp_rt_object_from_int", fn_type, Some(Linkage::External));
}

#[no_mangle]
pub extern fn unlisp_rt_int_from_obj(o: Object) -> i64 {
    o.unpack_int()
}

#[used]
static __INT_FROM_OBJ: extern "C" fn(Object) -> i64 = unlisp_rt_int_from_obj;

fn unlisp_rt_int_from_obj_gen_def(ctx: &Context, module: &Module) {
    let i64_ty = ctx.i64_type();
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = i64_ty.fn_type(&[obj_struct_ty.into()], false);
    module.add_function("unlisp_rt_int_from_obj", fn_type, Some(Linkage::External));
}

#[no_mangle]
pub extern fn unlisp_rt_set_fn_for_sym(f_ptr: *const c_void, sym: *mut Symbol) -> Object {
    unsafe {
        (*sym).function = Box::into_raw(Box::new(Function {
            name: ptr::null(),
            f_ptr: f_ptr,
            is_macro: false
        }))
    }

    Object::nil()
}

#[used]
static __SET_FN_FOR_SYM: extern "C" fn(f_ptr: *const c_void, sym: *mut Symbol) -> Object = unlisp_rt_set_fn_for_sym;

fn unlisp_rt_set_fn_for_sym_gen_def(ctx: &Context, module: &Module) {
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let f_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let sym_ptr_ty = module.get_type("unlisp_rt_symbol")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let fn_type = obj_struct_ty.fn_type(&[f_ptr_ty.into(), sym_ptr_ty.into()], false);
    module.add_function("unlisp_rt_set_fn_for_sym", fn_type, Some(Linkage::External));
}

#[no_mangle]
pub extern fn unlisp_rt_f_ptr_from_sym(sym: *mut Symbol) -> *const c_void {
    unsafe {
        (*(*sym).function).f_ptr
    }
}

#[used]
static __F_PTR_from_SYM: extern "C" fn(sym: *mut Symbol) -> *const c_void = unlisp_rt_f_ptr_from_sym;

fn unlisp_rt_f_ptr_from_sym_gen_def(ctx: &Context, module: &Module) {
    let f_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let sym_ptr_ty = module.get_type("unlisp_rt_symbol")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let fn_type = f_ptr_ty.fn_type(&[sym_ptr_ty.into()], false);
    module.add_function("unlisp_rt_f_ptr_from_sym", fn_type, Some(Linkage::External));
}
