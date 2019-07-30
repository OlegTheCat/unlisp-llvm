use crate::error::Error;
use inkwell::values::BasicValueEnum;

pub type CompileResult = Result<BasicValueEnum, Error>;
