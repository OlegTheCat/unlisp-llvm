use inkwell::values::BasicValueEnum;
use std::error::Error;

pub type GenResult<T> = Result<T, Box<dyn Error>>;
pub type CompileResult = GenResult<BasicValueEnum>;
