use std::ffi::CStr;
use std::mem;
use std::ptr;
use libc::c_char;

use crate::defs::Object;
use crate::error::RuntimeError;

const JMP_BUF_SIZE: usize = mem::size_of::<u32>() * 40;

type JmpBuf = [i8; JMP_BUF_SIZE];

#[export_name = "glob_jmp_buf"]
#[no_mangle]
#[used]
static mut GLOB_JMP_BUF: JmpBuf = [0; JMP_BUF_SIZE];

#[export_name = "err_msg_ptr"]
#[no_mangle]
#[used]
static mut ERR_MSG_PTR: *mut i8 = ptr::null_mut();

unsafe fn jmp_buf_ptr(buf: &mut JmpBuf) -> *mut i8 {
    &mut buf[0] as *mut i8
}

unsafe fn glob_jmp_buf_ptr() -> *mut i8 {
    jmp_buf_ptr(&mut GLOB_JMP_BUF)
}

extern "C" {
    fn setjmp(buf: *mut i8) -> i32;
    fn longjmp(buf: *const i8) -> !;
}

pub unsafe fn run_with_global_ex_handler<F: FnOnce() -> Object>(f: F) -> Result<Object, RuntimeError> {
    let mut prev_handler: JmpBuf = mem::zeroed();

    ptr::copy_nonoverlapping(
        glob_jmp_buf_ptr(),
        jmp_buf_ptr(&mut prev_handler),
        JMP_BUF_SIZE,
    );

    let result = if setjmp(glob_jmp_buf_ptr()) == 0 {
        Ok(f())
    } else {
        Err(RuntimeError::new(
            (*(ERR_MSG_PTR as *mut String)).clone(),
        ))
    };

    ptr::copy_nonoverlapping(
        jmp_buf_ptr(&mut prev_handler),
        glob_jmp_buf_ptr(),
        JMP_BUF_SIZE,
    );

    result
}

pub unsafe fn raise_error(msg: String) -> ! {
    ERR_MSG_PTR = Box::into_raw(Box::new(msg)) as *mut i8;
    longjmp(glob_jmp_buf_ptr())
}

// pub fn gen_defs(ctx: &Context, module: &Module) {
//     // sjlj_gen_def(ctx, module);
//     raise_arity_error_gen_def(ctx, module);
//     raise_undef_fn_error_gen_def(ctx, module);
// }

// fn sjlj_gen_def(ctx: &Context, module: &Module) {
//     let i32_ty = ctx.i32_type();

//     let buf_ty = ctx.opaque_struct_type("setjmp_buf");
//     let int32_arr_ty = i32_ty.array_type(40);
//     buf_ty.set_body(&[int32_arr_ty.into()], false);

//     // has to be looked up through module, to avoid renaming
//     let buf_ptr_ty = module
//         .get_type("setjmp_buf")
//         .unwrap()
//         .as_struct_type()
//         .ptr_type(AddressSpace::Generic);
//     let void_ty = ctx.void_type();
//     let sj_fn_ty = i32_ty.fn_type(&[buf_ptr_ty.into()], false);
//     let lj_fn_ty = void_ty.fn_type(&[buf_ptr_ty.into(), i32_ty.into()], false);

//     module.add_function("setjmp", sj_fn_ty, Some(Linkage::External));
//     module.add_function("longjmp", lj_fn_ty, Some(Linkage::External));
// }

#[no_mangle]
pub unsafe extern "C" fn raise_arity_error(name: *const c_char, _expected: u64, actual: u64) -> ! {
    let name_str = if name != ptr::null() {
        CStr::from_ptr(name).to_str().unwrap()
    } else {
        "lambda"
    };

    let msg = format!(
        "wrong number of arguments ({}) passed to {}",
        actual, name_str
    );

    raise_error(msg);
}

#[used]
static RAISE_ARITY_ERROR: unsafe extern "C" fn(
    name: *const c_char,
    expected: u64,
    actual: u64,
) -> ! = raise_arity_error;

#[no_mangle]
pub unsafe extern "C" fn raise_undef_fn_error(name: *const c_char) -> ! {
    let name_str = CStr::from_ptr(name).to_str().unwrap();

    let msg = format!("undefined function {}", name_str);

    raise_error(msg);
}

#[used]
static RAISE_UNDEF_FN_ERROR: unsafe extern "C" fn(name: *const c_char) -> ! = raise_undef_fn_error;

pub unsafe fn raise_cast_error(from: String, to: String) -> ! {
    let msg = format!("cannot cast {} to {}", from, to);

    raise_error(msg)
}
