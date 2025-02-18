use std::marker::PhantomData;

/* TODO: LexicalToken is different from Token from lexer. They coincide in terminal symbols,
       for example, terminal symbol "(" can be used as Token in lexer
       (Token { lexeme: "(", kind: TokenKind::LeftParen} type is returned from lexer)
       so also as lexical terminal token used in Grammar.

       At the same time, non-terminal will never be returned from lexer but should
       be present in grammar.

       How to ensure this duality of terminals and how to ensure a presence of both terminal and non-terminal?
*/
pub trait LexicalToken {
    type TokenKind;

    fn lexeme(&self) -> &str;
    fn is_terminal(&self) -> bool;
}

#[derive(Debug)]
pub struct Rule<T, U, F> {
    lhs: T,
    rhs: Vec<T>,
    handle: F,
    _marker: PhantomData<U>,
}

impl<T, U, F> Rule<T, U, F>
where
    F: Fn(U, T) -> U,
{
    pub fn new(lhs: T, rhs: Vec<T>, handle: F) -> Self {
        Self {
            lhs,
            rhs,
            handle,
            _marker: PhantomData,
        }
    }
}

// #[derive(Debug)]
// pub struct Grammar<T: LexicalToken + Hasher + Eq> {
//     rules: Vec<Rule<T>>,
// }

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Token, TokenKind},
        span::Span,
    };

    use super::*;

    #[derive(Debug)]
    enum Expr {
        String(String),
        Int(i32),
        Binary(Box<Expr>, String, Box<Expr>),
    }

    impl LexicalToken for Token<'_> {
        type TokenKind = TokenKind;

        fn lexeme(&self) -> &str {
            self.lexeme()
        }

        fn is_terminal(&self) -> bool {
            true
        }
    }

    #[test]
    fn create_rule() {
        let rule = Rule::new(
            Token::simple(TokenKind::LeftBrace, Span::new(0, 2, 0)),
            vec![],
            |ast: Expr, _: Token| ast,
        );
    }
}
