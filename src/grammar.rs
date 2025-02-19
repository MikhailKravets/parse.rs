use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

/* TODO: LexicalToken is different from Token from lexer. They coincide in terminal symbols,
       for example, terminal symbol "(" can be used as Token in lexer
       (Token { lexeme: "(", kind: TokenKind::LeftParen} type is returned from lexer)
       so also as lexical terminal token used in Grammar.

       At the same time, non-terminal will never be returned from lexer but should
       be present in grammar.

       How to ensure this duality of terminals and how to ensure a presence of both terminal and non-terminal?
*/
pub trait Terminal {
    type TokenKind;

    fn lexeme(&self) -> &str;
    fn kind(&self) -> Self::TokenKind;
}

#[derive(Debug)]
pub struct NonTermToken {
    lexeme: &'static str,
}

impl NonTermToken {
    pub fn new(lexeme: &'static str) -> Self {
        Self { lexeme }
    }
}

#[derive(Debug)]
pub enum LexicalToken<T> {
    Term(T),
    NonTerm(NonTermToken),
}

#[derive(Debug)]
pub struct Rule<T, U, F> {
    lhs: LexicalToken<T>,
    rhs: Vec<LexicalToken<T>>,
    handle: F,
    _marker: PhantomData<U>,
}

impl<T, U, F> Rule<T, U, F>
where
    T: Terminal,
    F: Fn(U, LexicalToken<T>) -> U,
{
    pub fn new(lhs: LexicalToken<T>, rhs: Vec<LexicalToken<T>>, handle: F) -> Self {
        Self {
            lhs,
            rhs,
            handle,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct Grammar<T, U, F> {
    rules: Vec<Rule<T, U, F>>,
}

impl<T, U, F> Grammar<T, U, F>
where
    T: Terminal,
{
    pub fn new(rules: Vec<Rule<T, U, F>>) -> Self {
        Self { rules }
    }

    pub fn first(&self) -> HashMap<Rule<T, U, F>, HashSet<Rule<T, U, F>>> {
        todo!("Write algorithm to build a first set for the grammar")
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Token, TokenKind},
        span::Span,
    };

    use super::*;

    impl Terminal for Token<'_> {
        type TokenKind = TokenKind;

        fn lexeme(&self) -> &str {
            self.lexeme()
        }

        fn kind(&self) -> Self::TokenKind {
            self.kind()
        }
    }

    #[derive(Debug)]
    enum Expr {
        String(String),
        Int(i32),
        Binary(Box<Expr>, String, Box<Expr>),
    }

    #[test]
    fn create_rule() {
        let rule = Rule::new(
            LexicalToken::NonTerm(NonTermToken::new("goal")),
            vec![LexicalToken::NonTerm(NonTermToken::new("list"))],
            |_, _: LexicalToken<Token>| Expr::Int(42),
        );
    }
}
