use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    marker::PhantomData,
};

const BULLET: &str = "•";

pub trait Terminal {
    type TokenKind;

    fn lexeme(&self) -> &str;
    fn kind(&self) -> Self::TokenKind;
}

#[derive(Debug, Clone)]
pub struct NonTermToken {
    lexeme: &'static str,
}

impl NonTermToken {
    pub fn new(lexeme: &'static str) -> Self {
        Self { lexeme }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Clone)]
pub struct Production<T> {
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

// TODO: make Item hashable
#[derive(Debug)]
pub struct Item<T> {
    production: Production<T>,
    dot_at: usize,
    lookahead: T,
}

impl<T> Item<T> {
    pub fn new(production: Production<T>, lookahead: T) -> Self {
        Self {
            production,
            dot_at: 0,
            lookahead,
        }
    }
}

impl<T: Clone> Item<T> {
    pub fn moving(item: &Self) -> Self {
        if item.dot_at >= item.production.rhs.len() {
            // TODO: use recoverable errors instead of panic
            panic!("Dot cannot exceed the number of tokens in right side of production")
        }

        Self {
            production: item.production.clone(),
            dot_at: item.dot_at + 1,
            lookahead: item.lookahead.clone(),
        }
    }
}

impl<T: Terminal> fmt::Display for Item<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rhs = Vec::with_capacity(self.production.rhs.len() + 1);
        for (i, v) in self.production.rhs.iter().enumerate() {
            if i == self.dot_at {
                rhs.push(BULLET);
            }
            rhs.push(v.lexeme());
        }

        if self.dot_at == self.production.rhs.len() {
            rhs.push(BULLET);
        }

        write!(
            f,
            "{} → [{}, {}]",
            self.production.lhs.lexeme(),
            rhs.join(" "),
            self.lookahead.lexeme()
        )
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

#[derive(Debug)]
pub struct FirstSet<'grammar> {
    inner: HashMap<&'grammar str, HashSet<&'grammar str>>,
}

impl<'grammar, T: Terminal, U, F> From<&'grammar Grammar<T, U, F>> for FirstSet<'grammar> {
    fn from(grammar: &'grammar Grammar<T, U, F>) -> Self {
        let empty_lexeme = grammar.empty_token.lexeme();
        let mut inner = HashMap::new();

        // Stack of productions to be processed by the algorithm
        let mut stack = Vec::<&Production<T>>::with_capacity(grammar.rules.len());
        let terminals = grammar.terminals();

        // Key is lexeme, value is a vec of dependent productions
        let mut rev_dependencies = grammar.reverse_dependencies();

        // Add terminals to first map
        for term in terminals {
            let mut set = HashSet::new();
            set.insert(term);
            inner.insert(term, set);
        }

        // Add non-terminals to first map.
        // Add productions to the stack of jobs
        for rule in grammar.rules.iter() {
            inner.insert(rule.production.lhs.lexeme(), HashSet::new());
            stack.push(&rule.production);
        }

        while !stack.is_empty() {
            // SAFETY: non-emptiness of stack is ensured in the expression under while
            let p = stack.pop().unwrap();

            // TODO: what if p.rhs.len() == 0?
            let mut rhs_set = if let Some(s) = inner.get(p.rhs[0].lexeme()) {
                s.clone()
            } else {
                HashSet::new()
            };
            let mut trailing = false;

            rhs_set.remove(empty_lexeme);
            for t in p.rhs.iter().rev().skip(1).rev() {
                if let Some(s) = inner.get(t.lexeme()) {
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
                && inner
                    .entry(p.rhs.last().unwrap().lexeme())
                    .or_insert(HashSet::new())
                    .contains(empty_lexeme)
            {
                rhs_set.insert(empty_lexeme);
            }

            let first_lhs = inner.entry(p.lhs.lexeme()).or_insert(HashSet::new());
            if rhs_set.difference(&first_lhs).count() > 0 {
                // TODO: if we use `.or_default()` method anyway. Perhaps, there is no need to add lhs to rev_dependencies?
                stack.extend_from_slice(rev_dependencies.entry(p.lhs.lexeme()).or_default());
            }

            first_lhs.extend(rhs_set);
        }

        Self { inner }
    }
}

impl<'grammar> FirstSet<'grammar> {
    pub fn get<T: Terminal>(&self, items: &[Production<T>]) -> HashSet<&'grammar str> {
        // TODO: what exactly should this method accept?
        todo!()
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

    #[test]
    fn create_item_moving() {
        let item = Item::new(
            Production::new(
                LexicalToken::NonTerm(NonTermToken::new("list")),
                vec![
                    LexicalToken::NonTerm(NonTermToken::new("list")),
                    LexicalToken::NonTerm(NonTermToken::new("pair")),
                ],
            ),
            Token::new(TokenKind::LeftParen, "(", Span::default()),
        );
        let next_item = Item::moving(&item);
        let next_item2 = Item::moving(&next_item);

        assert_eq!(item.dot_at, 0);
        assert_eq!(next_item.dot_at, 1);
        assert_eq!(next_item2.dot_at, 2);

        println!("{}", item);
        println!("{}", next_item);
        println!("{}", next_item2);
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
        let first_set = FirstSet::from(&grammar);

        assert!(first_set.inner.get("goal").unwrap().contains("("));
        assert!(first_set.inner.get("list").unwrap().contains("("));
        assert!(first_set.inner.get("pair").unwrap().contains("("));
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
        let first_set = FirstSet::from(&grammar);
        println!("{:#?}", first_set);
    }
}
