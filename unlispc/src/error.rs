use unlisp_rt::error::RuntimeError;

use std::error;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorType {
    Reader,
    Compilation,
    Macroexpansion,
    Runtime,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub ty: ErrorType,
}

impl Error {
    pub fn new(ty: ErrorType, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            ty: ty,
        }
    }

    pub fn rt_error(rt_err: RuntimeError) -> Self {
        Self {
            message: format!("{}", rt_err),
            ty: ErrorType::Runtime,
        }
    }

    pub fn convert(self, new_ty: ErrorType) -> Self {
        Self {
            message: self.message,
            ty: new_ty,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}: {}",
            match self.ty {
                ErrorType::Reader => "reader error",
                ErrorType::Compilation => "compilation error",
                ErrorType::Macroexpansion => "macroexpansion error",
                ErrorType::Runtime => "runtime error",
            },
            self.message
        )
    }
}

impl error::Error for Error {}
