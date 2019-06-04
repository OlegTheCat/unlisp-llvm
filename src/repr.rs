use crate::error::SyntaxError;

use std::collections::HashSet;
use std::iter::FromIterator;
use std::ops::Deref;

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
    pub name: Option<String>,
    pub arglist: Vec<String>,
    pub restarg: Option<String>,
    pub body: Vec<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Quote {
    pub body: Literal,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub cond: Box<HIR>,
    pub then_hir: Box<HIR>,
    pub else_hir: Option<Box<HIR>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub free_vars: Vec<String>,
    pub lambda: Lambda,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub fn_name: String,
    pub args: Vec<HIR>,
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
    Call(Call),
    LetBlock(LetBlock),
    Quote(Quote),
    If(If),
}

fn form_to_literal(form: &Form) -> Literal {
    match form {
        Form::Symbol(s) => Literal::SymbolLiteral(s.clone()),
        _ => unimplemented!(),
    }
}

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

            Form::Symbol(s) if is(s, "quote") => {
                let quote = Quote {
                    body: form_to_literal(
                        forms
                            .get(1)
                            .ok_or_else(|| SyntaxError::new("no arg to quote"))?,
                    ),
                };

                Ok(HIR::Quote(quote))
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

                fn parse_arglist(
                    arglist: &Form,
                ) -> Result<(Vec<String>, Option<String>), SyntaxError> {
                    let arglist = to_list(arglist)
                        .ok_or_else(|| SyntaxError::new("not a list in lambda arglist"))?;

                    let arglist = arglist
                        .into_iter()
                        .map(|arg| {
                            to_symbol(arg)
                                .cloned()
                                .ok_or_else(|| SyntaxError::new("not a symbol in arglist"))
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut iter = arglist.into_iter();

                    let simple_args = iter
                        .by_ref()
                        .take_while(|s| *s != "&".to_string())
                        .collect();

                    let restargs = iter.collect::<Vec<_>>();
                    let restarg = if restargs.is_empty() {
                        None
                    } else {
                        if restargs.len() != 1 {
                            return Err(SyntaxError::new("wrong syntax near '&' in lambda"));
                        } else {
                            restargs.into_iter().next()
                        }
                    };

                    Ok((simple_args, restarg))
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
                        body = forms_to_hirs(&forms[3..])?;
                    }

                    Form::List(_) => {
                        parsed_arglist = parse_arglist(name_or_arglist)?;
                        body = forms_to_hirs(&forms[2..])?;
                    }

                    _ => return Err(SyntaxError::new("not a list in lambda arglist")),
                };

                let (simple_args, restarg) = parsed_arglist;

                let lambda = Lambda {
                    name: name,
                    arglist: simple_args,
                    restarg: restarg,
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

pub fn form_to_hir_with_transforms(form: &Form) -> Result<HIR, SyntaxError> {
    Ok(convert_into_closures(&form_to_hir(form)?))
}

fn literal_to_form(literal: &Literal) -> Form {
    match literal {
        Literal::T => Form::T,
        Literal::SymbolLiteral(s) => Form::Symbol(s.clone()),
        Literal::IntegerLiteral(i) => Form::Integer(*i),
        Literal::StringLiteral(s) => Form::String(s.clone()),
        Literal::ListLiteral(list) => Form::List(list.iter().map(literal_to_form).collect()),
    }
}

#[allow(unused)]
pub fn hir_to_form(hir: &HIR) -> Form {
    match hir {
        HIR::Literal(lit) => literal_to_form(lit),
        HIR::Call(_) => unimplemented!(),
        _ => unimplemented!(),
    }
}

fn convert_lambda_body_item(
    bound_vars: &mut Vec<HashSet<String>>,
    free_vars: &mut HashSet<String>,
    body_item: &HIR,
) -> HIR {
    let is_bound = |s| {
        for frame in bound_vars.iter().rev() {
            if frame.get(s).is_some() {
                return true;
            }
        }

        false
    };

    match body_item {
        HIR::Literal(Literal::SymbolLiteral(s)) => {
            if !is_bound(s) {
                free_vars.insert(s.clone());
            }

            HIR::Literal(Literal::SymbolLiteral(s.clone()))
        }
        HIR::Literal(literal) => HIR::Literal(literal.clone()),
        HIR::Lambda(lambda) => {
            let closure = convert_lambda(lambda);

            for var in closure.free_vars.iter() {
                if !is_bound(var) {
                    free_vars.insert(var.clone());
                }
            }

            HIR::Closure(closure)
        }
        HIR::Closure(_) => panic!("unexpected closure"),
        HIR::Call(call) => {
            let call = Call {
                fn_name: call.fn_name.clone(),
                args: call
                    .args
                    .iter()
                    .map(|hir| convert_lambda_body_item(bound_vars, free_vars, hir))
                    .collect(),
            };

            HIR::Call(call)
        }
        HIR::LetBlock(let_block) => {
            let mut new_bindings = vec![];

            for (name, val) in let_block.bindings.iter() {
                let new_val = convert_lambda_body_item(bound_vars, free_vars, val);

                let mut frame = HashSet::new();
                frame.insert(name.clone());

                bound_vars.push(frame);
                new_bindings.push((name.clone(), new_val));
            }

            let new_body = let_block
                .body
                .iter()
                .map(|hir| convert_lambda_body_item(bound_vars, free_vars, hir))
                .collect();

            for _ in let_block.bindings.iter() {
                bound_vars.pop();
            }

            let new_let_block = LetBlock {
                bindings: new_bindings,
                body: new_body,
            };

            HIR::LetBlock(new_let_block)
        }
        HIR::Quote(quote) => HIR::Quote(quote.clone()),
        HIR::If(if_hir) => {
            let converted = If {
                cond: Box::new(convert_lambda_body_item(
                    bound_vars,
                    free_vars,
                    if_hir.cond.deref(),
                )),
                then_hir: Box::new(convert_lambda_body_item(
                    bound_vars,
                    free_vars,
                    if_hir.then_hir.deref(),
                )),
                else_hir: if_hir.else_hir.as_ref().map(|box_hir| {
                    Box::new(convert_lambda_body_item(
                        bound_vars,
                        free_vars,
                        box_hir.deref(),
                    ))
                }),
            };

            HIR::If(converted)
        }
    }
}

fn convert_lambda(lambda: &Lambda) -> Closure {
    let mut lambda_frame: HashSet<_> = lambda.arglist.iter().map(|n| n.clone()).collect();
    lambda
        .restarg
        .as_ref()
        .map(|arg| lambda_frame.insert(arg.clone()));

    let mut bound_vars = vec![lambda_frame];
    let mut free_vars = HashSet::new();

    let body = lambda
        .body
        .iter()
        .map(|item| convert_lambda_body_item(&mut bound_vars, &mut free_vars, item))
        .collect();

    Closure {
        free_vars: Vec::from_iter(free_vars),
        lambda: Lambda {
            name: lambda.name.clone(),
            arglist: lambda.arglist.clone(),
            restarg: lambda.restarg.clone(),
            body: body,
        },
    }
}

pub fn convert_into_closures(hir: &HIR) -> HIR {
    match hir {
        HIR::Literal(literal) => HIR::Literal(literal.clone()),
        HIR::Lambda(lambda) => HIR::Closure(convert_lambda(lambda)),
        HIR::Closure(_) => panic!("unexpected closure"),
        HIR::Call(call) => {
            let converted = Call {
                fn_name: call.fn_name.clone(),
                args: call.args.iter().map(convert_into_closures).collect(),
            };

            HIR::Call(converted)
        }
        HIR::LetBlock(let_block) => {
            let converted = LetBlock {
                bindings: let_block
                    .bindings
                    .iter()
                    .map(|(s, hir)| (s.clone(), convert_into_closures(hir)))
                    .collect(),
                body: let_block.body.iter().map(convert_into_closures).collect(),
            };

            HIR::LetBlock(converted)
        }
        HIR::Quote(quote) => HIR::Quote(quote.clone()),
        HIR::If(if_hir) => {
            let converted = If {
                cond: Box::new(convert_into_closures(if_hir.cond.deref())),
                then_hir: Box::new(convert_into_closures(if_hir.then_hir.deref())),
                else_hir: if_hir
                    .else_hir
                    .as_ref()
                    .map(|box_hir| Box::new(convert_into_closures(box_hir.deref()))),
            };

            HIR::If(converted)
        }
    }
}
