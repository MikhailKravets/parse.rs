use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    marker::PhantomData,
};

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

struct Production<T> {
    lhs: LexicalToken<T>,
    rhs: Vec<LexicalToken<T>>,
}

impl<T> Production<T> {
    fn new(lhs: LexicalToken<T>, rhs: Vec<LexicalToken<T>>) -> Self {
        Self { lhs, rhs }
    }
}

impl<T: Debug> fmt::Debug for Production<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} → {:?}", self.lhs, self.rhs)
    }
}

impl<T: Terminal> fmt::Display for Production<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rhs: Vec<&str> = self.rhs.iter().map(|v| v.lexeme()).collect();
        write!(f, "{} → {}", self.lhs.lexeme(), rhs.join(" "))
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
pub struct Grammar<T: Terminal, U, F> {
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

    /// Builds a `HashSet` of terminal token lexemes for the grammar.
    fn terminals(&self) -> HashSet<&str> {
        let mut terminals = HashSet::new();

        terminals.insert(self.empty_token.lexeme());
        terminals.insert(self.eof_token.lexeme());

        for rule in self.rules.iter() {
            for v in rule.production.rhs.iter() {
                match v {
                    LexicalToken::Term(t) => {
                        terminals.insert(t.lexeme());
                    }
                    _ => (),
                }
            }
        }

        terminals
    }

    /// Builds a `HashMap` containing reverse dependencies for all non-terminals
    /// of the grammar. Key of the hash map is the lexeme of non-terminal token
    /// and Value is a reference to dependant production
    /// 
    /// # Examples
    /// 
    /// For example, let's take a brackets grammar
    /// 
    /// ```ignore
    /// goal -> list
    /// list -> list pair
    /// list -> pair
    /// pair -> ( list )
    /// pair -> ()
    /// ```
    /// 
    /// Then the corresponding reverse dependencies map will look like
    /// 
    /// ```ignore
    /// {
    ///     "goal": [],
    ///     "list": [goal -> list, list -> list pair, pair -> ( list )],
    ///     "pair": [list -> list pair, list -> pair]
    /// }
    /// ```
    fn reverse_dependencies(&self) -> HashMap<&str, Vec<&Production<T>>> {
        let mut rev_dependencies = HashMap::<&str, Vec<&Production<T>>>::new();

        for rule in self.rules.iter() {
            rev_dependencies
                .entry(rule.production.lhs.lexeme())
                .or_default();
            for token in rule.production.rhs.iter() {
                match token {
                    LexicalToken::NonTerm(t) => {
                        rev_dependencies
                            .entry(t.lexeme)
                            .or_default()
                            .push(&rule.production);
                    }
                    _ => (),
                }
            }
        }

        rev_dependencies
    }
}

pub fn first<'grammar, T, U, F>(
    grammar: &'grammar Grammar<T, U, F>,
) -> HashMap<&'grammar str, HashSet<&'grammar str>>
where
    T: Terminal,
{
    let empty_lexeme = grammar.empty_token.lexeme();
    let mut map = HashMap::new();

    // Stack of productions to be processed by the algorithm
    let mut stack = Vec::<&Production<T>>::with_capacity(grammar.rules.len());
    let terminals = grammar.terminals();

    // Key is lexeme, value is a vec of dependent productions
    let mut rev_dependencies = grammar.reverse_dependencies();

    // Add terminals to first map
    for term in terminals {
        let mut set = HashSet::new();
        set.insert(term);
        map.insert(term, set);
    }

    // Add non-terminals to first map.
    // Add productions to the stack of jobs
    for rule in grammar.rules.iter() {
        map.insert(rule.production.lhs.lexeme(), HashSet::new());
        stack.push(&rule.production);
    }

    while !stack.is_empty() {
        // SAFETY: non-emptiness of stack is ensured in the expression under while
        let p = stack.pop().unwrap();

        // TODO: what if p.rhs.len() == 0?
        let mut rhs_set = if let Some(s) = map.get(p.rhs[0].lexeme()) {
            s.clone()
        } else {
            HashSet::new()
        };
        let mut trailing = false;

        rhs_set.remove(empty_lexeme);
        for t in p.rhs.iter().rev().skip(1).rev() {
            if let Some(s) = map.get(t.lexeme()) {
                if s.contains(empty_lexeme) {
                    rhs_set.extend(s);
                    rhs_set.remove(empty_lexeme);
                } else {
                    trailing = false;
                    break;
                }
            }
        }

        if trailing
            && map
                .entry(p.rhs.last().unwrap().lexeme())
                .or_insert(HashSet::new())
                .contains(empty_lexeme)
        {
            rhs_set.insert(empty_lexeme);
        }

        let first_lhs = map.entry(p.lhs.lexeme()).or_insert(HashSet::new());
        if rhs_set.difference(&first_lhs).count() > 0 {
            // TODO: if we use `.or_default()` method anyway. Perhaps, there is no need to add lhs to rev_dependencies?
            stack.extend_from_slice(rev_dependencies.entry(p.lhs.lexeme()).or_default());
        }

        first_lhs.extend(rhs_set);
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
    fn build_first_for_brackets() {
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
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, ")", Span::default())),
                    ],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("pair")),
                    vec![
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(", Span::default())),
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, ")", Span::default())),
                    ],
                    handle,
                ),
            ],
            Token::new(TokenKind::EOF, "EOF", Span::default()),
            Token::new(TokenKind::Init, "EPS", Span::default()),
        );
        let first_set = first(&grammar);

        assert!(first_set.get("goal").unwrap().contains("("));
        assert!(first_set.get("list").unwrap().contains("("));
        assert!(first_set.get("pair").unwrap().contains("("));
    }

    #[test]
    fn build_first_with_empty() {
        fn handle(_: (), t: LexicalToken<Token>) {
            println!("{:#?}", t)
        }

        let grammar = Grammar::new(
            vec![
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("goal")),
                    vec![LexicalToken::NonTerm(NonTermToken::new("A"))],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("A")),
                    vec![
                        LexicalToken::NonTerm(NonTermToken::new("A")),
                        LexicalToken::Term(Token::new(TokenKind::And, "a", Span::default())),
                    ],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("A")),
                    vec![LexicalToken::Term(Token::new(
                        TokenKind::And,
                        "a",
                        Span::default(),
                    ))],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("A")),
                    vec![LexicalToken::Term(Token::new(
                        TokenKind::Init,
                        "EPS",
                        Span::default(),
                    ))],
                    handle,
                ),
            ],
            Token::new(TokenKind::EOF, "EOF", Span::default()),
            Token::new(TokenKind::Init, "EPS", Span::default()),
        );
        let first_set = first(&grammar);
        println!("{:#?}", first_set);
    }
}
