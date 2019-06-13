use inkwell::values::BasicValueEnum;

use std::error::Error;
use std::fmt;

pub type GenResult<T> = Result<T, Box<dyn Error>>;
pub type CompileResult = GenResult<BasicValueEnum>;

#[derive(Debug, Clone)]
pub struct UndefinedSymbol {
    symbol_name: String,
}

impl UndefinedSymbol {
    pub fn new(symbol_name: impl Into<String>) -> Self {
        Self {
            symbol_name: symbol_name.into(),
        }
    }
}

impl fmt::Display for UndefinedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "undefined {} {}", "symbol", self.symbol_name)
    }
}

impl Error for UndefinedSymbol {}
