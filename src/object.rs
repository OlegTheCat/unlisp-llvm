#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LispForm {
    T,
    Symbol(String),
    Integer(i64),
    String(String),
    List(Vec<LispForm>),
}

macro_rules! define_unwrapper {
    ($id:ident ($enum:ident :: $from:ident) -> $to:ty) => {
        #[allow(unused)]
        pub fn $id(arg: &$enum) -> &$to {
            match arg {
                $enum::$from(x) => x,
                x => panic!("cannot cast {:?} to {}", x, stringify!($to)),
            }
        }
    };
}

define_unwrapper!(to_list(LispForm :: List) -> Vec<LispForm>);
define_unwrapper!(to_symbol(LispForm :: Symbol) -> String);
define_unwrapper!(to_i64(LispForm :: Integer) -> i64);
