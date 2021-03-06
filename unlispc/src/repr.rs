use crate::error::{Error, ErrorType};
use unlisp_rt::*;

use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::iter::FromIterator;

use libc::c_char;

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
    pub bindings: Vec<(String, HIR)>,
    pub body: Vec<HIR>,
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
pub struct DeclareVar {
    pub var_name: String,
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
pub struct SetExpr {
    pub name: String,
    pub val: Box<HIR>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HIR {
    DeclareVar(DeclareVar),
    Literal(Literal),
    Lambda(Lambda),
    Closure(Closure),
    Call(Call),
    LetBlock(LetBlock),
    Quote(Quote),
    If(If),
    SetExpr(SetExpr),
}

fn form_to_literal(form: &Form) -> Literal {
    match form {
        Form::Symbol(s) => Literal::SymbolLiteral(s.clone()),
        Form::Integer(i) => Literal::IntegerLiteral(*i),
        Form::String(s) => Literal::StringLiteral(s.clone()),
        Form::T => Literal::T,
        Form::List(list) => {
            let literals = list.iter().map(form_to_literal).collect();
            Literal::ListLiteral(literals)
        }
    }
}

fn forms_to_hirs(forms: &[Form]) -> Result<Vec<HIR>, Error> {
    forms.iter().map(form_to_hir).collect::<Result<Vec<_>, _>>()
}

fn forms_to_hir(forms: &Vec<Form>) -> Result<HIR, Error> {
    fn is(s1: &String, s2: &str) -> bool {
        s1.as_str() == s2
    }

    if forms.is_empty() {
        Ok(HIR::Literal(Literal::ListLiteral(vec![])))
    } else {
        match &forms[0] {
            Form::T | Form::Integer(_) | Form::String(_) | Form::List(_) => {
                Ok(Err(Error::new(ErrorType::Reader, "illegal function call"))?)
            }

            Form::Symbol(s) if is(s, "quote") => {
                let quote = Quote {
                    body: form_to_literal(
                        forms
                            .get(1)
                            .ok_or_else(|| Error::new(ErrorType::Reader, "no arg to quote"))?,
                    ),
                };

                forms
                    .get(2)
                    .map(|_| {
                        Err(Error::new(
                            ErrorType::Reader,
                            format!(
                                "wrong number of arguments ({}) passed to quote",
                                forms.len() - 1
                            ),
                        )) as Result<(), _>
                    })
                    .transpose()?;

                Ok(HIR::Quote(quote))
            }

            Form::Symbol(s) if is(s, "if") => {
                let cond = forms
                    .get(1)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "no condition in if"))?;

                let cond = form_to_hir(cond)?;

                let then_form = forms
                    .get(2)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "no then in if"))?;

                let then_hir = form_to_hir(then_form)?;

                let else_hir = forms
                    .get(3)
                    .map(|form| form_to_hir(form).map(Box::new))
                    .transpose()?;

                forms
                    .get(4)
                    .map(|_| {
                        Err(Error::new(ErrorType::Reader, "too many clauses in if"))
                            as Result<(), _>
                    })
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
                    .ok_or_else(|| (Error::new(ErrorType::Reader, "no bindings in let")))?;
                let bindings = to_list(bindings)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "let bindings are not a list"))?;

                let mut collected_bindings = vec![];

                for binding in bindings.iter() {
                    let binding = to_list(binding).ok_or_else(|| {
                        Error::new(ErrorType::Reader, "let binding is not a list")
                    })?;
                    let sym = binding
                        .get(0)
                        .ok_or_else(|| Error::new(ErrorType::Reader, "empty binding clause"))?;
                    let sym = to_symbol(sym).ok_or_else(|| {
                        Error::new(ErrorType::Reader, "not a symbol in binding clause")
                    })?;

                    let val_form = binding.get(1).ok_or_else(|| {
                        Error::new(ErrorType::Reader, "no value in binding clause")
                    })?;

                    binding
                        .get(3)
                        .map(|_| {
                            Err(Error::new(ErrorType::Reader, "malformed let binding"))
                                as Result<(), _>
                        })
                        .transpose()?;

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
                let parsed_arglist;
                let body;

                fn parse_arglist(arglist: &Form) -> Result<(Vec<String>, Option<String>), Error> {
                    let arglist = to_list(arglist).ok_or_else(|| {
                        Error::new(ErrorType::Reader, "not a list in lambda arglist")
                    })?;

                    let arglist = arglist
                        .into_iter()
                        .map(|arg| {
                            to_symbol(arg).cloned().ok_or_else(|| {
                                Error::new(ErrorType::Reader, "not a symbol in arglist")
                            })
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
                            return Err(Error::new(
                                ErrorType::Reader,
                                "wrong syntax near '&' in lambda",
                            ));
                        } else {
                            restargs.into_iter().next()
                        }
                    };

                    Ok((simple_args, restarg))
                }

                let name_or_arglist = forms
                    .get(1)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "no arglist in lambda"))?;

                match name_or_arglist {
                    Form::Symbol(l_name) => {
                        name = Some(l_name.clone());
                        let arglist = forms
                            .get(2)
                            .ok_or_else(|| Error::new(ErrorType::Reader, "no arglist in lambda"))?;
                        parsed_arglist = parse_arglist(arglist)?;
                        body = forms_to_hirs(&forms[3..])?;
                    }

                    Form::List(_) => {
                        parsed_arglist = parse_arglist(name_or_arglist)?;
                        body = forms_to_hirs(&forms[2..])?;
                    }

                    _ => {
                        return Err(Error::new(
                            ErrorType::Reader,
                            "not a list in lambda arglist",
                        ))?
                    }
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
            Form::Symbol(s) if is(s, "set!") => {
                let sym = forms
                    .get(1)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "no symbol in set"))?;
                let sym = to_symbol(sym)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "not a symbol in set"))?;

                let val = forms
                    .get(2)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "no value in set"))?;

                if forms.get(3).is_some() {
                    Err(Error::new(
                        ErrorType::Reader,
                        format!(
                            "wrong number of arguments ({}) passed to set",
                            forms.len() - 1
                        ),
                    ))?
                }

                let expr = SetExpr {
                    name: sym.clone(),
                    val: Box::new(form_to_hir(val)?),
                };

                Ok(HIR::SetExpr(expr))
            }
            Form::Symbol(s) if is(s, "declare-var") => {
                let sym = forms
                    .get(1)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "no symbol in declare-var"))?;
                let sym = to_symbol(sym)
                    .ok_or_else(|| Error::new(ErrorType::Reader, "not a symbol in declare-var"))?;

                if forms.get(2).is_some() {
                    Err(Error::new(
                        ErrorType::Reader,
                        format!(
                            "wrong number of arguments ({}) passed to declare-var",
                            forms.len() - 1
                        ),
                    ))?
                }

                let decl_var = DeclareVar {
                    var_name: sym.clone(),
                };

                Ok(HIR::DeclareVar(decl_var))
            }
            Form::Symbol(s) => unsafe {
                let call_sym = symbols::get_or_intern_symbol(s.clone());
                let sym_fn = (*call_sym).function;

                let call_hir;

                if sym_fn.is_null() || !(*sym_fn).is_macro {
                    let call = Call {
                        fn_name: s.clone(),
                        args: forms[1..]
                            .iter()
                            .map(form_to_hir)
                            .collect::<Result<Vec<_>, _>>()?,
                    };

                    call_hir = HIR::Call(call);
                } else {
                    let arg_objs_list = forms[1..]
                        .iter()
                        .map(form_to_runtime_object)
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .rev()
                        .fold(defs::ListLike::from_nil(), |acc, obj| acc.cons(obj));
                    let expanded = predefined::call_macro(sym_fn, arg_objs_list)
                        .map_err(|e| Error::rt_error(e).convert(ErrorType::Macroexpansion))?;
                    let form = runtime_object_to_form(expanded)
                        .map_err(|e| e.convert(ErrorType::Macroexpansion))?;
                    call_hir =
                        form_to_hir(&form).map_err(|e| e.convert(ErrorType::Macroexpansion))?;
                }

                Ok(call_hir)
            },
        }
    }
}

pub fn form_to_hir(form: &Form) -> Result<HIR, Error> {
    match form {
        literal @ Form::T
        | literal @ Form::Symbol(_)
        | literal @ Form::Integer(_)
        | literal @ Form::String(_) => Ok(HIR::Literal(form_to_literal(literal))),

        Form::List(list) => forms_to_hir(list),
    }
}

pub fn form_to_hir_with_transforms(form: &Form) -> Result<HIR, Error> {
    let hir = form_to_hir(form)?;
    Ok(convert_into_closures(&hir))
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
        HIR::SetExpr(e) => {
            if !is_bound(&e.name) {
                free_vars.insert(e.name.clone());
            }

            HIR::SetExpr(SetExpr {
                name: e.name.clone(),
                val: e.val.clone(),
            })
        }
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
                    &if_hir.cond,
                )),
                then_hir: Box::new(convert_lambda_body_item(
                    bound_vars,
                    free_vars,
                    &if_hir.then_hir,
                )),
                else_hir: if_hir.else_hir.as_ref().map(|box_hir| {
                    Box::new(convert_lambda_body_item(bound_vars, free_vars, &box_hir))
                }),
            };

            HIR::If(converted)
        }
        HIR::DeclareVar(decl_var) => HIR::DeclareVar(decl_var.clone()),
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
        HIR::SetExpr(e) => HIR::SetExpr(e.clone()),
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
                cond: Box::new(convert_into_closures(&if_hir.cond)),
                then_hir: Box::new(convert_into_closures(&if_hir.then_hir)),
                else_hir: if_hir
                    .else_hir
                    .as_ref()
                    .map(|box_hir| Box::new(convert_into_closures(&box_hir))),
            };

            HIR::If(converted)
        }

        HIR::DeclareVar(decl_var) => HIR::DeclareVar(decl_var.clone()),
    }
}

pub fn form_to_runtime_object(form: &Form) -> Result<defs::Object, Error> {
    let obj = match form {
        Form::Symbol(s) => defs::Object::from_symbol(symbols::get_or_intern_symbol(s.clone())),
        Form::Integer(i) => defs::Object::from_int(*i),
        Form::String(s) => {
            let c_str = CString::new(s.as_str()).expect("string conversion failed");
            let c_ptr: *const c_char = c_str.into_raw();
            defs::Object::from_string(c_ptr)
        }
        Form::List(list) => list
            .iter()
            .map(form_to_runtime_object)
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .rev()
            .fold(defs::ListLike::from_nil(), |acc, obj| acc.cons(obj))
            .to_object(),
        Form::T => defs::Object::t(),
    };

    Ok(obj)
}

pub unsafe fn runtime_object_to_form(t_obj: defs::Object) -> Result<Form, Error> {
    let form = match t_obj.ty {
        defs::ObjType::Int64 => Form::Integer(t_obj.unpack_int()),
        defs::ObjType::Box => runtime_object_to_form(t_obj.unpack_underlying())?,
        defs::ObjType::Cons => {
            let mut converted = vec![];
            let mut cons = t_obj.unpack_cons();

            while (*cons).cdr().ty == defs::ObjType::Cons {
                converted.push(runtime_object_to_form((*cons).car())?);
                cons = (*cons).cdr().unpack_cons();
            }

            if !(*cons).cdr().is_nil() {
                Err(Error::new(
                    ErrorType::Macroexpansion,
                    "embedding pairs in code is not supported yet",
                ))?;
            } else {
                converted.push(runtime_object_to_form((*cons).car())?);
            }

            Form::List(converted)
        }
        defs::ObjType::Function => Err(Error::new(
            ErrorType::Macroexpansion,
            "embedding functions in code is not supported yet",
        ))?,
        defs::ObjType::Symbol => {
            if t_obj.is_nil() {
                Form::List(vec![])
            } else {
                Form::Symbol(
                    CStr::from_ptr((*t_obj.unpack_symbol()).name)
                        .to_str()
                        .expect("string conversion failed")
                        .to_string(),
                )
            }
        }

        defs::ObjType::String => Form::String(
            CStr::from_ptr(t_obj.unpack_string())
                .to_str()
                .expect("string conversion failed")
                .to_string(),
        ),
    };

    Ok(form)
}
