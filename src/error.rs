use std::error::Error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct SyntaxError {
    message: String,
}

impl SyntaxError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.message)
    }
}

impl Error for SyntaxError {}

#[derive(Debug, Clone)]
pub struct RuntimeError(String);

impl RuntimeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl Error for RuntimeError {}
