use crate::cons::List;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LispObject {
    T,
    Symbol(String),
    Integer(i64),
    String(String),
    List(List<LispObject>),
}
