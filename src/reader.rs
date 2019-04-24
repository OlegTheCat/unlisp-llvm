use crate::cons::List;
use crate::error::SyntaxError;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::object::LispObject;
use std::error::Error;
use std::io::Read;
use std::io;

pub struct Reader<'a, T: Read + 'a> {
    lexer: Lexer<'a, T>,
}

impl<'a, T: Read + 'a> Reader<'a, T> {
    pub fn create(r: &'a mut T) -> Reader<'a, T> {
        Reader {
            lexer: Lexer::create(r),
        }
    }

    fn next_tok_or_eof(&mut self) -> Result<Token, Box<Error>> {
        let tok = self.lexer.next_token()?;
        tok.ok_or(Box::new(io::Error::from(io::ErrorKind::UnexpectedEof)))
    }

    fn tok_to_trivial_form(&self, tok: &Token) -> Option<LispObject> {
        match tok {
            Token::Symbol(s) if s == "nil" => Some(LispObject::List(List::empty())),
            Token::Symbol(s) if s == "t" => Some(LispObject::T),
            Token::Symbol(s) => Some(LispObject::Symbol(s.clone())),
            Token::IntegerLiteral(i) => Some(LispObject::Integer(*i)),
            Token::StringLiteral(s) => Some(LispObject::String(s.to_string())),
            _ => None,
        }
    }

    fn read_list_form(&mut self) -> Result<LispObject, Box<Error>> {
        let mut vec = Vec::new();

        let mut tok = self.next_tok_or_eof()?;

        while tok != Token::RightPar {
            let form;

            if let Some(t_form) = self.tok_to_trivial_form(&tok) {
                form = t_form;
            } else {
                form = match tok {
                    Token::LeftPar => self.read_list_form()?,
                    Token::RightPar => break,
                    tok => panic!("unexpected token {:?}", tok),
                }
            }

            vec.push(form);
            tok = self.next_tok_or_eof()?;
        }

        Ok(LispObject::List(List::from_rev_iter(vec)))
    }

    pub fn read_form(&mut self) -> Result<Option<LispObject>, Box<Error>> {
        let tok = self.lexer.next_token()?;

        if tok.is_none() {
            return Ok(None);
        }

        let tok = tok.unwrap();

        let trivial_form = self.tok_to_trivial_form(&tok);
        let form = match trivial_form {
            Some(form) => form,
            None => match tok {
                Token::LeftPar => self.read_list_form()?,
                Token::RightPar => Err(SyntaxError::new("unbalanced parens"))?,
                tok => panic!("unexpected token {:?}", tok),
            },
        };

        Ok(Some(form))
    }
}
