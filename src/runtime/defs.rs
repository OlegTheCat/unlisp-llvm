use libc::c_char;
use libc::c_void;
use std::ptr;
use std::fmt;

use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::BasicType;
use inkwell::AddressSpace;

use super::symbols;

#[repr(C)]
pub union UntaggedObject {
    int: i64,
    list: *mut List,
    sym: *mut Symbol,
    function: *mut Function,
    nil: *const c_void
}

#[derive(Clone, Eq, PartialEq)]
#[repr(C)]
pub enum ObjType {
    Nil = 0,
    Int64 = 1,
    List = 2,
    Symbol = 3,
    Function = 4,
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
        struct_ty.set_body(&[int32_ty.into(), int8_ptr_ty.into()], false);
    }

    fn type_err(&self, t_name: &str) -> ! {
        panic!("Cannot unpack Object of type {} as {}", self.ty.as_i32(), t_name);
    }

    pub fn unpack_int(&self) -> i64 {
        if self.ty == ObjType::Int64 {
            unsafe { self.obj.int }
        } else {
            self.type_err("i64");
        }
    }

    pub fn unpack_list(&self) -> *mut List {
        if self.ty == ObjType::List {
            unsafe { self.obj.list }
        } else {
            self.type_err("list");
        }
    }

    pub fn unpack_symbol(&self) -> *mut Symbol {
        if self.ty == ObjType::Symbol {
            unsafe { self.obj.sym }
        } else {
            self.type_err("symbol");
        }
    }

    pub fn unpack_nil(&self) -> *const c_void {
        if self.ty == ObjType::Nil {
            unsafe { self.obj.nil }
        } else {
            self.type_err("nil");
        }
    }

    pub fn unpack_function(&self) -> *const Function {
        if self.ty == ObjType::Function {
            unsafe { self.obj.function }
        } else {
            self.type_err("function");
        }
    }

    pub fn from_int(i: i64) -> Object {
        Self {
            ty: ObjType::Int64,
            obj: UntaggedObject { int: i }
        }
    }

    pub fn from_list(list: *mut List) -> Object {
        Self {
            ty: ObjType::List,
            obj: UntaggedObject { list: list }
        }
    }

    pub fn from_symbol(sym: *mut Symbol) -> Object {
        Self {
            ty: ObjType::Symbol,
            obj: UntaggedObject { sym: sym }
        }
    }

    pub fn from_function(function: *mut Function) -> Object {
        Self {
            ty: ObjType::Function,
            obj: UntaggedObject { function: function }
        }
    }

    pub fn nil() -> Object {
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
    pub function: *const Function,
}

impl Symbol {
    fn gen_llvm_def(context: &Context, module: &Module) {
        let func_struct_ty = module.get_type("unlisp_rt_function").unwrap().into_struct_type();

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
pub enum FunctionType {
    Function = 0,
    Closure = 1,
}

#[repr(C)]
pub struct Function {
    pub ty: FunctionType,
    pub name: *const c_char,
    pub arglist: *const *const c_char,
    pub arg_count: i64,
    pub is_macro: bool,
    pub invoke_f_ptr: *const c_void,
    pub apply_to_f_ptr: *const c_void,
}

impl Function {
    fn gen_llvm_def(context: &Context) {
        let fn_struct_ty = context.opaque_struct_type("unlisp_rt_function");

        let ty_ty = context.i32_type();
        let ty_name = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_arglist = context.i8_type()
            .ptr_type(AddressSpace::Generic)
            .ptr_type(AddressSpace::Generic);
        let ty_arg_count = context.i64_type();
        let ty_is_macro = context.bool_type();
        let ty_invoke_f_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
        let ty_apply_to_f_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

        fn_struct_ty.set_body(
            &[
                ty_ty.into(),
                ty_name.into(),
                ty_arglist.into(),
                ty_arg_count.into(),
                ty_is_macro.into(),
                ty_invoke_f_ptr.into(),
                ty_apply_to_f_ptr.into()
            ],
            false,
        );
    }
}


pub fn gen_defs(ctx: &Context, module: &Module) {
    Object::gen_llvm_def(ctx);
    List::gen_llvm_def(ctx);
    Function::gen_llvm_def(ctx);
    Symbol::gen_llvm_def(ctx, module);

    unlisp_rt_intern_sym_gen_def(ctx, module);
    unlisp_rt_object_from_int_gen_def(ctx, module);
    unlisp_rt_int_from_obj_gen_def(ctx, module);
}


#[no_mangle]
pub extern fn unlisp_rt_intern_sym(name: *const c_char) -> *mut Symbol {
    symbols::get_or_intern_symbol_by_ptr(name)
}

#[used]
static INTERN_SYM: extern "C" fn(name: *const c_char) -> *mut Symbol = unlisp_rt_intern_sym;

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
static OBJ_FROM_INT: extern "C" fn(i: i64) -> Object = unlisp_rt_object_from_int;

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
static INT_FROM_OBJ: extern "C" fn(Object) -> i64 = unlisp_rt_int_from_obj;

fn unlisp_rt_int_from_obj_gen_def(ctx: &Context, module: &Module) {
    let i64_ty = ctx.i64_type();
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = i64_ty.fn_type(&[obj_struct_ty.into()], false);
    module.add_function("unlisp_rt_int_from_obj", fn_type, Some(Linkage::External));
}
