use libc::c_char;
use libc::c_void;
use std::ffi::CStr;
use std::fmt;
use std::ptr;

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
    nil: *const c_void,
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
        panic!(
            "Cannot unpack Object of type {} as {}",
            self.ty.as_i32(),
            t_name
        );
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

    pub fn nil() -> Object {
        Self {
            ty: ObjType::Nil,
            obj: UntaggedObject { nil: ptr::null() },
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.ty {
            ObjType::Int64 => write!(f, "Object[int64, {}]", unsafe { self.obj.int }),
            ObjType::List => write!(f, "Object[list, {}]", unsafe { (*self.obj.list).len }),
            ObjType::Function => write!(f, "Object[fn, {}]", unsafe {
                (*self.obj.function).arg_count
            }),
            ObjType::Symbol => write!(f, "Object[symbol, {}]", unsafe {
                CStr::from_ptr((*self.obj.sym).name).to_str().unwrap()
            }),
            ObjType::Nil => write!(f, "Object[nil]"),
            _ => panic!("unsupported type"),
        }
    }
}

#[repr(C)]
pub struct Node {
    val: *mut Object,
    next: *mut List,
}

#[repr(C)]
pub struct List {
    node: *mut Node,
    len: u64,
}

impl List {
    fn gen_llvm_def(context: &Context, _module: &Module) {
        let i64_ty = context.i64_type();
        let i8_ptr_ty = context.i8_type().ptr_type(AddressSpace::Generic);
        let struct_ty = context.opaque_struct_type("unlisp_rt_list");

        struct_ty.set_body(&[i8_ptr_ty.into(), i64_ty.into()], false);
    }
}

#[repr(C)]
pub struct Symbol {
    pub name: *const c_char,
    pub function: *const Function,
}

impl Symbol {
    fn gen_llvm_def(context: &Context, module: &Module) {
        let func_struct_ty = module
            .get_type("unlisp_rt_function")
            .unwrap()
            .into_struct_type();

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
        let ty_arglist = context
            .i8_type()
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
                ty_apply_to_f_ptr.into(),
            ],
            false,
        );
    }
}

pub fn gen_defs(ctx: &Context, module: &Module) {
    Object::gen_llvm_def(ctx);
    List::gen_llvm_def(ctx, module);
    Function::gen_llvm_def(ctx);
    Symbol::gen_llvm_def(ctx, module);

    unlisp_rt_intern_sym_gen_def(ctx, module);
    unlisp_rt_object_from_int_gen_def(ctx, module);
    unlisp_rt_int_from_obj_gen_def(ctx, module);
    unlisp_rt_object_from_function_gen_def(ctx, module);
    unlisp_rt_object_from_symbol_gen_def(ctx, module);
    unlisp_rt_object_is_nil_gen_def(ctx, module);
    unlisp_rt_nil_object_gen_def(ctx, module);
    malloc_gen_def(ctx, module);
    sjlj_gen_def(ctx, module);
}

fn malloc_gen_def(ctx: &Context, module: &Module) {
    let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let i32_ty = ctx.i32_type();
    let malloc_fn_ty = i8_ptr_ty.fn_type(&[i32_ty.into()], false);
    module.add_function("malloc", malloc_fn_ty, Some(Linkage::External));
}

fn sjlj_gen_def(ctx: &Context, module: &Module) {
    let i32_ty = ctx.i32_type();

    let buf_ty = ctx.opaque_struct_type("setjmp_buf");
    let int32_arr_ty = i32_ty.array_type(40);
    buf_ty.set_body(&[int32_arr_ty.into()], false);

    // has to be looked up through module, to avoid renaming
    let buf_ptr_ty = module
        .get_type("setjmp_buf")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let void_ty = ctx.void_type();
    let sj_fn_ty = i32_ty.fn_type(&[buf_ptr_ty.into()], false);
    let lj_fn_ty = void_ty.fn_type(&[buf_ptr_ty.into(), i32_ty.into()], false);

    module.add_function("setjmp", sj_fn_ty, Some(Linkage::External));
    module.add_function("longjmp", lj_fn_ty, Some(Linkage::External));
}

#[no_mangle]
pub extern "C" fn unlisp_rt_intern_sym(name: *const c_char) -> *mut Symbol {
    symbols::get_or_intern_symbol_by_ptr(name)
}

#[used]
static INTERN_SYM: extern "C" fn(name: *const c_char) -> *mut Symbol = unlisp_rt_intern_sym;

fn unlisp_rt_intern_sym_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i8_type().ptr_type(AddressSpace::Generic);
    let sym_struct_ty = module.get_type("unlisp_rt_symbol").unwrap();
    let sym_struct_ptr_ty = sym_struct_ty
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);

    let fn_type = sym_struct_ptr_ty.fn_type(&[arg_ty.into()], false);
    module.add_function("unlisp_rt_intern_sym", fn_type, Some(Linkage::External));
}

#[no_mangle]
pub extern "C" fn unlisp_rt_object_from_int(i: i64) -> Object {
    Object::from_int(i)
}

#[used]
static OBJ_FROM_INT: extern "C" fn(i: i64) -> Object = unlisp_rt_object_from_int;

fn unlisp_rt_object_from_int_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = ctx.i64_type();
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_int",
        fn_type,
        Some(Linkage::External),
    );
}

#[no_mangle]
pub extern "C" fn unlisp_rt_int_from_obj(o: Object) -> i64 {
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

#[no_mangle]
pub extern "C" fn unlisp_rt_object_from_function(f: *mut Function) -> Object {
    Object::from_function(f)
}

#[used]
static OBJ_FROM_FN: extern "C" fn(f: *mut Function) -> Object = unlisp_rt_object_from_function;

fn unlisp_rt_object_from_function_gen_def(_: &Context, module: &Module) {
    let arg_ty = module
        .get_type("unlisp_rt_function")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_function",
        fn_type,
        Some(Linkage::External),
    );
}

#[no_mangle]
pub extern "C" fn unlisp_rt_object_from_symbol(s: *mut Symbol) -> Object {
    Object::from_symbol(s)
}

#[used]
static OBJ_FROM_SYM: extern "C" fn(f: *mut Symbol) -> Object = unlisp_rt_object_from_symbol;

fn unlisp_rt_object_from_symbol_gen_def(_: &Context, module: &Module) {
    let arg_ty = module
        .get_type("unlisp_rt_symbol")
        .unwrap()
        .as_struct_type()
        .ptr_type(AddressSpace::Generic);
    let obj_struct_ty = module.get_type("unlisp_rt_object").unwrap();
    let fn_type = obj_struct_ty.fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_from_symbol",
        fn_type,
        Some(Linkage::External),
    );
}


#[no_mangle]
pub extern "C" fn unlisp_rt_object_is_nil(o: Object) -> bool {
    o.ty == ObjType::List && {
        let list_ptr = o.unpack_list();
        unsafe { (*list_ptr).len == 0 }
    }
}

#[used]
static IS_NIL: extern "C" fn(Object) -> bool = unlisp_rt_object_is_nil;

fn unlisp_rt_object_is_nil_gen_def(ctx: &Context, module: &Module) {
    let arg_ty = module
        .get_type("unlisp_rt_object")
        .unwrap();

    let fn_type = ctx.bool_type().fn_type(&[arg_ty.into()], false);
    module.add_function(
        "unlisp_rt_object_is_nil",
        fn_type,
        Some(Linkage::External),
    );
}

#[no_mangle]
pub extern "C" fn unlisp_rt_nil_object() -> Object {
    let list = List {
        node: ptr::null_mut(),
        len: 0
    };

    Object::from_list(Box::into_raw(Box::new(list)))
}

#[used]
static NIL_OBJ: extern "C" fn() -> Object = unlisp_rt_nil_object;

fn unlisp_rt_nil_object_gen_def(ctx: &Context, module: &Module) {
    let obj_ty = module
        .get_type("unlisp_rt_object")
        .unwrap();

    let fn_type = obj_ty.fn_type(&[], false);
    module.add_function(
        "unlisp_rt_nil_object",
        fn_type,
        Some(Linkage::External),
    );
}
