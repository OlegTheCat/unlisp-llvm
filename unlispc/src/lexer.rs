use crate::error;
use crate::pushback_reader::PushbackReader;
use std::error::Error;
use std::io;
use std::io::Read;

type GenResult<T> = Result<T, Box<dyn Error>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    LeftPar,
    RightPar,
    IntegerLiteral(i64),
    StringLiteral(String),
    Symbol(String),
}

pub fn is_eof<T>(result: &io::Result<T>) -> bool {
    match result {
        Err(e) => e.kind() == io::ErrorKind::UnexpectedEof,
        _ => false,
    }
}

pub struct Lexer<'a, T: Read + 'a> {
    pbr: PushbackReader<'a, T>,
}

impl<'a, T: Read> Lexer<'a, T> {
    pub fn create(r: &'a mut T) -> Lexer<'a, T> {
        Lexer {
            pbr: PushbackReader::create(r),
        }
    }

    fn valid_symbol_char(c: char) -> bool {
        c.is_alphanumeric()
            || vec!['&', '*', '-', '?', '+', '<', '>', '_', '!']
                .into_iter()
                .find(|x| c == *x)
                .is_some()
    }

    fn next_char(&mut self) -> io::Result<char> {
        let mut one_byte: [u8; 1] = [0];
        self.pbr.read_exact(&mut one_byte).map_err(|e| {
            // replace not informative 'failed to fill buffer' message
            if e.kind() == io::ErrorKind::UnexpectedEof {
                io::Error::from(io::ErrorKind::UnexpectedEof)
            } else {
                e
            }
        })?;
        Ok(one_byte[0] as char)
    }

    fn unread_char(&mut self, c: char) {
        self.pbr.unread_byte(c as u8);
    }

    fn read_string_literal(&mut self) -> io::Result<String> {
        let mut buf = Vec::new();
        loop {
            let c = self.next_char()?;

            // TODO: handle escaping
            if c == '"' {
                break;
            }
            buf.push(c);
        }

        Ok(buf.into_iter().collect())
    }

    fn read_integer_literal(&mut self) -> io::Result<i64> {
        let mut buf = Vec::new();
        loop {
            let c = self.next_char();

            if is_eof(&c) {
                break;
            }

            let c = c?;

            if c.is_numeric() {
                buf.push(c);
            } else {
                self.unread_char(c);
                break;
            }
        }

        let s: String = buf.into_iter().collect();
        Ok(s.parse::<i64>().unwrap())
    }

    fn read_symbol(&mut self) -> io::Result<String> {
        let mut buf = Vec::new();
        loop {
            let c = self.next_char();

            if is_eof(&c) {
                break;
            }

            let c = c?;

            if Self::valid_symbol_char(c) {
                buf.push(c);
            } else {
                self.unread_char(c);
                break;
            }
        }

        Ok(buf.into_iter().collect())
    }

    fn skip_line(&mut self) -> io::Result<()> {
        let mut next_char = self.next_char()?;
        while next_char != '\n' {
            next_char = self.next_char()?;
        }

        Ok(())
    }

    pub fn next_token(&mut self) -> GenResult<Option<Token>> {
        let c = self.next_char();

        if is_eof(&c) {
            return Ok(None);
        }

        let c = c?;

        if c.is_whitespace() {
            return self.next_token();
        }

        let tok = match c {
            ';' => {
                self.skip_line()?;
                return self.next_token();
            }
            '(' => Token::LeftPar,
            ')' => Token::RightPar,

            c if c.is_numeric() => {
                self.unread_char(c);
                Token::IntegerLiteral(self.read_integer_literal()?)
            }

            c if Self::valid_symbol_char(c) => {
                self.unread_char(c);
                Token::Symbol(self.read_symbol()?)
            }

            '"' => Token::StringLiteral(self.read_string_literal()?),
            _ => Err(error::Error::new(
                error::ErrorType::Reader,
                format!("unexpexted char {}", c),
            ))?,
        };

        Ok(Some(tok))
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
        let mut lexer = Lexer::create(&mut input);

        assert_eq!(lexer.next_token().unwrap(), None);

        let mut input = "foo".as_bytes();
        let mut lexer = Lexer::create(&mut input);
        let _ = lexer.next_token().unwrap().unwrap();

        assert_eq!(lexer.next_token().unwrap(), None);
    }

    #[test]
    fn test_integer_literal() {
        let mut input = "1 12 1000 2019".as_bytes();
        let mut lexer = Lexer::create(&mut input);

        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::IntegerLiteral(1)
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::IntegerLiteral(12)
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::IntegerLiteral(1000)
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::IntegerLiteral(2019)
        );
    }

    #[test]
    fn test_string_literal() {
        let mut input = "\"\" \"foo\" \"bar\"".as_bytes();
        let mut lexer = Lexer::create(&mut input);

        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::StringLiteral("".to_string())
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::StringLiteral("foo".to_string())
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::StringLiteral("bar".to_string())
        );
    }

    #[test]
    fn test_incomplete_string() {
        let mut input = "\"foo".as_bytes();
        let mut lexer = Lexer::create(&mut input);

        assert!(is_gen_eof(&lexer.next_token()));
    }

    #[test]
    fn test_symbol() {
        let mut input = "x foo bar*".as_bytes();
        let mut lexer = Lexer::create(&mut input);

        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::Symbol("x".to_string())
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::Symbol("foo".to_string())
        );
        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::Symbol("bar*".to_string())
        );
    }

    #[test]
    fn test_parens() {
        let mut input = "( ) (".as_bytes();
        let mut lexer = Lexer::create(&mut input);

        assert_eq!(lexer.next_token().unwrap().unwrap(), Token::LeftPar);
        assert_eq!(lexer.next_token().unwrap().unwrap(), Token::RightPar);
        assert_eq!(lexer.next_token().unwrap().unwrap(), Token::LeftPar);
    }

    #[test]
    fn test_comments() {
        let mut input = ";; this is comment \n foo".as_bytes();
        let mut lexer = Lexer::create(&mut input);

        assert_eq!(
            lexer.next_token().unwrap().unwrap(),
            Token::Symbol("foo".to_string())
        );
    }
}
