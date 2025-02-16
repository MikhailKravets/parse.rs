use crate::scanner::Scanner;

pub struct Token<'source> {
    lexeme: &'source str,
}

// TODO: lexer is a concrete implementation of the end user
//       while Grammar, Parser, and generator must work with
//       generic lexer, token, etc
pub struct Lexer<'source> {
    scanner: Scanner<'source>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            scanner: Scanner::new(source),
        }
    }

    pub fn next(&mut self) -> Token<'source> {
        unimplemented!()
    }
}
