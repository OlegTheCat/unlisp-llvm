#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LispForm {
    T,
    Symbol(String),
    Integer(i64),
    String(String),
    List(Vec<LispForm>),
}
