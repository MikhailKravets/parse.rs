use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
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

impl<T: Terminal> LexicalToken<T> {
    pub fn lexeme(&self) -> &str {
        match self {
            Self::Term(t) => t.lexeme(),
            Self::NonTerm(t) => t.lexeme,
        }
    }
}

#[derive(Debug)]
struct Production<T> {
    lhs: LexicalToken<T>,
    rhs: Vec<LexicalToken<T>>,
}

impl<T> Production<T> {
    fn new(lhs: LexicalToken<T>, rhs: Vec<LexicalToken<T>>) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(Debug)]
pub struct Rule<T, U, F> {
    production: Production<T>,
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
            production: Production::new(lhs, rhs),
            handle,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct Grammar<T, U, F> {
    rules: Vec<Rule<T, U, F>>,
    eof_token: LexicalToken<T>,
    empty_token: LexicalToken<T>,
}

impl<T, U, F> Grammar<T, U, F>
where
    T: Terminal,
{
    pub fn new(rules: Vec<Rule<T, U, F>>, eof_token: T, empty_token: T) -> Self {
        Self {
            rules,
            eof_token: LexicalToken::Term(eof_token),
            empty_token: LexicalToken::Term(empty_token),
        }
    }
}

pub fn first<'grammar, T, U, F>(
    grammar: &'grammar Grammar<T, U, F>,
) -> HashMap<&'grammar str, HashSet<&'grammar str>>
where
    T: Terminal,
{
    let mut map = HashMap::new();
    let mut stack = Vec::<&Production<T>>::with_capacity(grammar.rules.len());

    // TODO: retrieve terminal symbols from grammar and insert them into map
    for rule in grammar.rules.iter() {
        map.insert(rule.production.lhs.lexeme(), HashSet::new());
    }

    // key is lexeme, value is a vec of dependent productions
    let mut rev_dependencies = HashMap::<&str, Vec<&Production<T>>>::new();
    for rule in grammar.rules.iter() {
        for token in rule.production.rhs.iter() {
            rev_dependencies
                .entry(token.lexeme())
                .or_default()
                .push(&rule.production);
        }
    }

    // println!("{:#?}", rev_dependencies);

    // TODO: build first map
    while !stack.is_empty() {
        // SAFETY: non-emptiness of stack is ensured in the expression under while
        let p = stack.pop().unwrap();
    }

    map
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

    #[test]
    fn build_first() {
        fn handle(_: (), t: LexicalToken<Token>) {
            println!("{:#?}", t)
        }
        let grammar = Grammar::new(
            vec![
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("goal")),
                    vec![LexicalToken::NonTerm(NonTermToken::new("list"))],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("list")),
                    vec![
                        LexicalToken::NonTerm(NonTermToken::new("list")),
                        LexicalToken::NonTerm(NonTermToken::new("pair")),
                    ],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("list")),
                    vec![LexicalToken::NonTerm(NonTermToken::new("pair"))],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("pair")),
                    vec![
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(", Span::default())),
                        LexicalToken::NonTerm(NonTermToken::new("list")),
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(", Span::default())),
                    ],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("pair")),
                    vec![
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(", Span::default())),
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(", Span::default())),
                    ],
                    handle,
                ),
            ],
            Token::simple(TokenKind::EOF, Span::default()),
            Token::simple(TokenKind::Init, Span::default()),
        );
        let first_set = first(&grammar);
    }
}
