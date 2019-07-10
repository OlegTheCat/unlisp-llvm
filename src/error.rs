use std::error;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorType {
    Syntax,
    Runtime,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub ty: ErrorType,
}

impl Error {
    pub fn new(message: impl Into<String>, ty: ErrorType) -> Self {
        Self {
            message: message.into(),
            ty: ty,
        }
    }

    pub fn new_syntax_error(message: impl Into<String>) -> Self {
        Self::new(message, ErrorType::Syntax)
    }

    pub fn new_runtime_error(message: impl Into<String>) -> Self {
        Self::new(message, ErrorType::Runtime)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.message)
    }
}

impl error::Error for Error {}
