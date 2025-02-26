use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    hash::Hash,
    marker::PhantomData,
};

const BULLET: &str = "•";

pub trait Terminal {
    fn lexeme(&self) -> &str;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonTermToken {
    lexeme: &'static str,
}

impl NonTermToken {
    pub fn new(lexeme: &'static str) -> Self {
        Self { lexeme }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Production<T: Terminal> {
    lhs: LexicalToken<T>,
    rhs: Vec<LexicalToken<T>>,
}

impl<T: Terminal> Production<T> {
    pub fn new(lhs: LexicalToken<T>, rhs: Vec<LexicalToken<T>>) -> Self {
        Self { lhs, rhs }
    }

    #[inline]
    pub fn lhs(&self) -> &LexicalToken<T> {
        &self.lhs
    }

    #[inline]
    pub fn rhs(&self) -> &[LexicalToken<T>] {
        &self.rhs
    }

    /// Returns `true` if right hand side of the production is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.rhs.is_empty()
    }
}

impl<T: Debug + Terminal> fmt::Debug for Production<T> {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Item<T: Terminal> {
    production: Production<T>,
    dot_at: usize,
    lookahead: LexicalToken<T>,
}

impl<T: Terminal> Item<T> {
    pub fn new(production: Production<T>, lookahead: LexicalToken<T>) -> Self {
        Self {
            production,
            dot_at: 0,
            lookahead,
        }
    }

    pub fn next(&self, skip: usize) -> impl Iterator<Item = &LexicalToken<T>> {
        self.production
            .rhs
            .iter()
            .skip(self.dot_at + skip)
            .map(|v| v)
            .chain(std::iter::once(&self.lookahead))
    }

    pub fn next_symbol(&self) -> Option<&LexicalToken<T>> {
        self.next(0).next()
    }

    #[inline]
    pub fn production(&self) -> &Production<T> {
        &self.production
    }
}

impl<T: Clone + Terminal> Item<T> {
    pub fn moving(item: &Self) -> Option<Self> {
        if item.dot_at >= item.production.rhs.len() {
            // TODO: use recoverable errors instead of Option
            return None;
            // panic!("Dot cannot exceed the number of tokens in right side of production")
        }

        Some(Self {
            production: item.production.clone(),
            dot_at: item.dot_at + 1,
            lookahead: item.lookahead.clone(),
        })
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
pub struct Rule<T: Terminal, U, F> {
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

impl<T: Terminal, U, F> Rule<T, U, F> {
    #[inline]
    pub fn production(&self) -> &Production<T> {
        &self.production
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

    #[inline]
    pub fn eof(&self) -> &LexicalToken<T> {
        &self.eof_token
    }

    #[inline]
    pub fn empty(&self) -> &LexicalToken<T> {
        &self.empty_token
    }

    #[inline]
    pub fn rules(&self) -> &[Rule<T, U, F>] {
        &self.rules
    }
}

impl<T: Terminal + Clone + Eq + Hash, U, F> Grammar<T, U, F> {
    /// Builds a `HashSet` of terminal tokens for the grammar.
    fn terminals(&self) -> Vec<&LexicalToken<T>> {
        let mut terminals = vec![self.empty(), self.eof()];

        for rule in self.rules.iter() {
            for v in rule.production.rhs.iter() {
                match v {
                    LexicalToken::Term(_) => {
                        terminals.push(v);
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
    fn reverse_dependencies(&self) -> HashMap<&LexicalToken<T>, Vec<&Production<T>>> {
        let mut rev_dependencies = HashMap::<_, Vec<&Production<T>>>::new();

        for rule in self.rules.iter() {
            rev_dependencies.entry(rule.production.lhs()).or_default();
            for token in rule.production.rhs.iter() {
                match token {
                    LexicalToken::NonTerm(_) => {
                        rev_dependencies
                            .entry(token)
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
pub struct FirstSet<T> {
    inner: HashMap<LexicalToken<T>, HashSet<LexicalToken<T>>>,

    // TODO: this field is also stored in grammar. Should it be copied here or store it somewhere else?
    empty_token: LexicalToken<T>,
}

impl<T: Terminal + Clone + Eq + Hash, U, F> From<&Grammar<T, U, F>> for FirstSet<T> {
    fn from(grammar: &Grammar<T, U, F>) -> Self {
        let empty_token = grammar.empty().clone();
        let mut inner = HashMap::new();

        // Stack of productions to be processed by the algorithm
        let mut stack = Vec::<&Production<T>>::with_capacity(grammar.rules.len());
        let terminals = grammar.terminals();

        // Key is lexeme, value is a vec of dependent productions
        let mut rev_dependencies = grammar.reverse_dependencies();

        // Add terminals to first map
        for term in terminals {
            let mut set = HashSet::new();
            set.insert(term.clone());
            inner.insert(term.clone(), set);
        }

        // Add non-terminals to first map.
        // Add productions to the stack of jobs
        for rule in grammar.rules.iter() {
            inner.insert(rule.production.lhs.clone(), HashSet::new());
            stack.push(&rule.production);
        }

        while let Some(p) = stack.pop() {
            // TODO: what if p.rhs.len() == 0?
            let mut rhs_set = if let Some(s) = inner.get(&p.rhs[0]) {
                s.clone()
            } else {
                HashSet::new()
            };
            let mut trailing = false;

            rhs_set.remove(&empty_token);
            for t in p.rhs.iter().rev().skip(1).rev() {
                if let Some(s) = inner.get(t) {
                    if s.contains(&empty_token) {
                        rhs_set.extend(s.iter().cloned());
                        rhs_set.remove(&empty_token);
                    } else {
                        trailing = false;
                        break;
                    }
                }
            }

            if trailing
                && inner
                    .entry(p.rhs().last().unwrap().clone()) // TODO: can we .unwrap() safely?
                    .or_insert(HashSet::new())
                    .contains(&empty_token)
            {
                rhs_set.insert(empty_token.clone());
            }

            let first_lhs = inner.entry(p.lhs.clone()).or_insert(HashSet::new());
            if rhs_set.difference(&first_lhs).count() > 0 {
                // TODO: if we use `.or_default()` method anyway. Perhaps, there is no need to add lhs to rev_dependencies?
                stack.extend_from_slice(rev_dependencies.entry(&p.lhs).or_default());
            }

            first_lhs.extend(rhs_set);
        }

        Self {
            inner,
            empty_token: grammar.empty().clone(),
        }
    }
}

impl<T: Terminal + Hash + Eq> FirstSet<T> {
    pub fn get<'a>(
        &'a self,
        lexemes: impl Iterator<Item = &'a LexicalToken<T>>,
    ) -> HashSet<&'a LexicalToken<T>> {
        let mut set = HashSet::new();

        for lexeme in lexemes {
            if let Some(first) = self.inner.get(lexeme) {
                set.extend(first);
                set.remove(&self.empty_token);

                if !first.contains(&self.empty_token) {
                    break;
                }
            }
        }

        set
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
        fn lexeme(&self) -> &str {
            self.lexeme()
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
            LexicalToken::Term(Token::new(TokenKind::LeftParen, "(", Span::default())),
        );
        let next_item = Item::moving(&item).unwrap();
        let next_item2 = Item::moving(&next_item).unwrap();

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
        println!("{:#?}", first_set);

        // assert!(first_set.inner.get(&LexicalToken::NonTerm(NonTermToken::new("goal"))).unwrap().contains("("));
        // assert!(first_set.inner.get("list").unwrap().contains("("));
        // assert!(first_set.inner.get("pair").unwrap().contains("("));
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
