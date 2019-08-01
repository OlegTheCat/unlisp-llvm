use crate::error;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::repr::Form;
use std::error::Error;
use std::io;
use std::io::Read;

pub struct Reader<'a, T: Read + 'a> {
    lexer: Lexer<'a, T>,
}

impl<'a, T: Read + 'a> Reader<'a, T> {
    pub fn create(r: &'a mut T) -> Reader<'a, T> {
        Reader {
            lexer: Lexer::create(r),
        }
    }

    fn next_tok_or_eof(&mut self) -> Result<Token, Box<dyn Error>> {
        let tok = self.lexer.next_token()?;
        tok.ok_or(Box::new(io::Error::from(io::ErrorKind::UnexpectedEof)))
    }

    fn tok_to_trivial_form(&self, tok: &Token) -> Option<Form> {
        match tok {
            Token::Symbol(s) if s == "nil" => Some(Form::List(vec![])),
            Token::Symbol(s) if s == "t" => Some(Form::T),
            Token::Symbol(s) => Some(Form::Symbol(s.clone())),
            Token::IntegerLiteral(i) => Some(Form::Integer(*i)),
            Token::StringLiteral(s) => Some(Form::String(s.to_string())),
            _ => None,
        }
    }

    fn read_list_form(&mut self) -> Result<Form, Box<dyn Error>> {
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

        Ok(Form::List(vec))
    }

    pub fn read_form(&mut self) -> Result<Option<Form>, Box<dyn Error>> {
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
                Token::RightPar => Err(error::Error::new(
                    error::ErrorType::Reader,
                    "unbalanced parens",
                ))?,
                tok => panic!("unexpected token {:?}", tok),
            },
        };

        Ok(Some(form))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_gen_eof<T>(result: &Result<T, Box<dyn Error>>) -> bool {
        match result {
            Err(e) => match e.downcast_ref::<io::Error>() {
                Some(io_err) => io_err.kind() == io::ErrorKind::UnexpectedEof,
                None => false,
            },
            _ => false,
        }
    }

    #[test]
    fn test_empty() {
        let mut input = "".as_bytes();
        let mut reader = Reader::create(&mut input);

        assert_eq!(reader.read_form().unwrap(), None);

        let mut input = "foo".as_bytes();
        let mut reader = Reader::create(&mut input);

        let _ = reader.read_form().unwrap().unwrap();
        assert_eq!(reader.read_form().unwrap(), None);
    }

    #[test]
    fn test_integer_literal() {
        let mut input = "1 12 1000 2019".as_bytes();
        let mut reader = Reader::create(&mut input);

        assert_eq!(reader.read_form().unwrap().unwrap(), Form::Integer(1));
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::Integer(12)
        );
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::Integer(1000)
        );
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::Integer(2019)
        );
    }

    #[test]
    fn test_string_literal() {
        let mut input = "\"\" \"foo\" \"bar\"".as_bytes();
        let mut reader = Reader::create(&mut input);

        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::String("".to_string())
        );
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::String("foo".to_string())
        );
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::String("bar".to_string())
        );
    }

    #[test]
    fn test_symbol() {
        let mut input = "x foo bar*".as_bytes();
        let mut reader = Reader::create(&mut input);

        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::Symbol("x".to_string())
        );
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::Symbol("foo".to_string())
        );
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::Symbol("bar*".to_string())
        );
    }

    #[test]
    fn test_list() {
        let mut input = "() (foo bar) (foo (bar baz) quux)".as_bytes();
        let mut reader = Reader::create(&mut input);

        let sym = |x: &str| Form::Symbol(x.to_string());

        assert_eq!(reader.read_form().unwrap().unwrap(), Form::List(vec![]));
        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::List(vec![sym("foo"), sym("bar")])
        );

        assert_eq!(
            reader.read_form().unwrap().unwrap(),
            Form::List(vec![
                sym("foo"),
                Form::List(vec![sym("bar"), sym("baz")]),
                sym("quux")
            ])
        );
    }

    #[test]
    fn test_nil_t() {
        let mut input = "nil t".as_bytes();
        let mut reader = Reader::create(&mut input);

        assert_eq!(reader.read_form().unwrap().unwrap(), Form::List(vec![]));
        assert_eq!(reader.read_form().unwrap().unwrap(), Form::T);
    }

    #[test]
    fn test_incomplete_list() {
        let mut input = "(foo".as_bytes();
        let mut reader = Reader::create(&mut input);
        assert!(is_gen_eof(&reader.read_form()));
    }

    //TODO: tests on unbalanced pars
}
