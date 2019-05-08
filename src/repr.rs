use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Form {
    T,
    Symbol(String),
    Integer(i64),
    String(String),
    List(Vec<Form>),
}

macro_rules! define_unwrapper {
    ($id:ident ($enum:ident :: $from:ident) -> $to:ty) => {
        #[allow(unused)]
        pub fn $id(arg: &$enum) -> Option<&$to> {
            match arg {
                $enum::$from(x) => Some(x),
                x => None,
            }
        }
    };
}

define_unwrapper!(to_list(Form :: List) -> Vec<Form>);
define_unwrapper!(to_symbol(Form :: Symbol) -> String);
define_unwrapper!(to_i64(Form :: Integer) -> i64);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetBlock {
    bindings: Vec<(String, HIR)>,
    body: Vec<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Lambda {
    name: Option<String>,
    arglist: Vec<String>,
    body: Vec<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Quote {
    body: Box<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    cond: Box<HIR>,
    then_hir: Box<HIR>,
    else_hir: Option<Box<HIR>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Defun {
    name: String,
    args: Vec<String>,
    body: Vec<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    captured: Vec<String>,
    lambda: Lambda,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    fn_name: String,
    args: Vec<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    T,
    SymbolLiteral(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    ListLiteral(Vec<Literal>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HIR {
    Literal(Literal),
    Lambda(Lambda),
    Closure(Closure),
    Defun(Defun),
    Call(Call),
    LetBlock(LetBlock),
    Quote(Quote),
    If(If),
}

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

fn forms_to_hirs(forms: &[Form]) -> Result<Vec<HIR>, SyntaxError> {
    forms.iter().map(form_to_hir).collect::<Result<Vec<_>, _>>()
}

fn forms_to_hir(forms: &Vec<Form>) -> Result<HIR, SyntaxError> {
    fn is(s1: &String, s2: &str) -> bool {
        s1.as_str() == s2
    }

    if forms.is_empty() {
        Ok(HIR::Literal(Literal::ListLiteral(vec![])))
    } else {
        match &forms[0] {
            Form::T | Form::Integer(_) | Form::String(_) | Form::List(_) => {
                Err(SyntaxError::new("illegal function call"))
            }

            Form::Symbol(s) if is(s, "if") => {
                let cond = forms
                    .get(1)
                    .ok_or_else(|| SyntaxError::new("no condition in if"))?;

                let cond = form_to_hir(cond)?;

                let then_form = forms
                    .get(2)
                    .ok_or_else(|| SyntaxError::new("no then in if"))?;

                let then_hir = form_to_hir(then_form)?;

                let else_hir = forms
                    .get(3)
                    .map(|form| form_to_hir(form).map(Box::new))
                    .transpose()?;

                let if_hir = If {
                    cond: Box::new(cond),
                    then_hir: Box::new(then_hir),
                    else_hir: else_hir,
                };

                Ok(HIR::If(if_hir))
            }

            Form::Symbol(s) if is(s, "let") => {
                let bindings = forms
                    .get(1)
                    .ok_or_else(|| (SyntaxError::new("no bindings in let")))?;
                let bindings = to_list(bindings)
                    .ok_or_else(|| SyntaxError::new("let bindings are not a list"))?;

                let mut collected_bindings = vec![];

                for binding in bindings.iter() {
                    let binding = to_list(binding)
                        .ok_or_else(|| SyntaxError::new("let binding is not a list"))?;
                    let sym = binding
                        .get(0)
                        .ok_or_else(|| SyntaxError::new("empty binding clause"))?;
                    let sym = to_symbol(sym)
                        .ok_or_else(|| SyntaxError::new("not a symbol in binding clause"))?;

                    let val_form = binding
                        .get(1)
                        .ok_or_else(|| SyntaxError::new("no value in binding clause"))?;

                    let val_form = form_to_hir(val_form)?;

                    collected_bindings.push((sym.clone(), val_form));
                }

                let body = forms_to_hirs(&forms[2..])?;

                let let_block = LetBlock {
                    bindings: collected_bindings,
                    body: body,
                };

                Ok(HIR::LetBlock(let_block))
            }

            Form::Symbol(s) if is(s, "lambda") => {
                let mut name = None;
                let mut parsed_arglist;
                let mut body;

                fn parse_arglist(arglist: &Form) -> Result<Vec<String>, SyntaxError> {
                    let arglist = to_list(arglist)
                        .ok_or_else(|| SyntaxError::new("not a list in lambda arglist"))?;

                    arglist
                        .iter()
                        .map(|arg| {
                            to_symbol(arg)
                                .cloned()
                                .ok_or_else(|| SyntaxError::new("not a symbol in arglist"))
                        })
                        .collect::<Result<Vec<_>, _>>()
                }

                let name_or_arglist = forms
                    .get(1)
                    .ok_or_else(|| SyntaxError::new("no arglist in lambda"))?;

                match name_or_arglist {
                    Form::Symbol(l_name) => {
                        name = Some(l_name.clone());
                        let arglist = forms
                            .get(2)
                            .ok_or_else(|| SyntaxError::new("no arglist in lambda"))?;
                        parsed_arglist = parse_arglist(arglist)?;
                        body = forms_to_hirs(&forms[2..])?;
                    }

                    Form::List(_) => {
                        parsed_arglist = parse_arglist(name_or_arglist)?;
                        body = forms_to_hirs(&forms[1..])?;
                    }

                    _ => return Err(SyntaxError::new("not a list in lambda arglist")),
                };

                let lambda = Lambda {
                    name: name,
                    arglist: parsed_arglist,
                    body: body,
                };

                Ok(HIR::Lambda(lambda))
            }
            Form::Symbol(s) => {
                let call = Call {
                    fn_name: s.clone(),
                    args: forms[1..]
                        .iter()
                        .map(form_to_hir)
                        .collect::<Result<Vec<_>, _>>()?,
                };
                Ok(HIR::Call(call))
            }
        }
    }
}

pub fn form_to_hir(form: &Form) -> Result<HIR, SyntaxError> {
    match form {
        Form::T => Ok(HIR::Literal(Literal::T)),
        Form::Symbol(s) => Ok(HIR::Literal(Literal::SymbolLiteral(s.to_owned()))),
        Form::Integer(i) => Ok(HIR::Literal(Literal::IntegerLiteral(*i))),
        Form::String(s) => Ok(HIR::Literal(Literal::StringLiteral(s.to_owned()))),
        Form::List(list) => forms_to_hir(list),
    }
}

fn literal_to_form(literal: &Literal) -> Form {
    match literal {
        Literal::T => Form::T,
        Literal::SymbolLiteral(s) => Form::Symbol(s.clone()),
        Literal::IntegerLiteral(i) => Form::Integer(*i),
        Literal::StringLiteral(s) => Form::String(s.clone()),
        Literal::ListLiteral(list) => Form::List(list.iter().map(literal_to_form).collect())
    }
}

pub fn hir_to_form(hir: &HIR) -> Form {
    match hir {
        HIR::Literal(lit) => literal_to_form(lit),
        HIR::Call(call) => unimplemented!(),
        _ => unimplemented!()
    }
}
