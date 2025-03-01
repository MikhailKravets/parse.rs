use std::{
    collections::{BTreeSet, HashMap}, fmt::Display, hash::{BuildHasher, Hash}
};

use crate::grammar::{FirstSet, Grammar, Item, LexicalToken, Terminal};

/// Type alias for a set of grammar items
type ItemSet<T, F> = BTreeSet<Item<T, F>>;

/// Type alias for canonical collection
type Canonical<T, F> = HashMap<ItemSet<T, F>, usize>;

/// Type alias for canonical trace table
type Trace<T> = HashMap<usize, HashMap<LexicalToken<T>, usize>>;

/// Type alias for actions table
type ActionTable<T, F> = HashMap<usize, HashMap<LexicalToken<T>, Action<T, F>>>;

/// Type alias for goto table
type GotoTable<T> = HashMap<usize, HashMap<LexicalToken<T>, usize>>;

#[derive(Debug)]
pub enum Action<T: Terminal, F> {
    Shift(usize),
    Reduce(Item<T, F>),
    Accept(Item<T, F>),
}

impl<T: Terminal, F> Display for Action<T, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shift(id) => write!(f, "Shift({})", id),
            Self::Reduce(item) => write!(f, "Reduce({})", item),
            Self::Accept(_) => write!(f, "Accept")
        }
    }
}

#[derive(Debug)]
/// A simple container for parses tables
pub struct Parser<T: Terminal, F> {
    actions: ActionTable<T, F>,
    goto: GotoTable<T>,
}

impl<T: Terminal, F> Parser<T, F> {
    pub fn new(actions: ActionTable<T, F>, goto: GotoTable<T>) -> Self {
        Self { actions, goto }
    }
}

pub struct ParserBuilder<T: Terminal, U, F> {
    grammar: Grammar<T, U, F>,
    first: FirstSet<T>,
}

impl<T, U, F> From<Grammar<T, U, F>> for ParserBuilder<T, U, F>
where
    T: Terminal + Clone + Eq + Ord + Hash,
    F: Fn(U, LexicalToken<T>) -> U,
{
    fn from(value: Grammar<T, U, F>) -> Self {
        Self::new(value)
    }
}

impl<T, U, F> ParserBuilder<T, U, F>
where
    T: Terminal + Clone + Eq + Ord + Hash,
    F: Fn(U, LexicalToken<T>) -> U,
{
    pub fn new(grammar: Grammar<T, U, F>) -> Self {
        Self {
            first: FirstSet::from(&grammar),
            grammar,
        }
    }

    /// Builds closure set for the initial items
    fn closure(&self, initial: impl Iterator<Item = Item<T, F>>) -> ItemSet<T, F> {
        let mut stack: Vec<Item<T, F>> = initial.collect();
        let mut set = BTreeSet::new();

        while let Some(item) = stack.pop() {
            if let Some(c) = item.next_symbol() {
                for r in self
                    .grammar
                    .rules()
                    .iter()
                    .filter(|r| r.production().lhs() == c)
                {
                    if r.production().is_empty() {
                        continue;
                    }

                    for l in self.first.get(item.next(1)) {
                        let item =
                            Item::new(r.production().clone(), l.clone(), item.handle().clone());
                        if !set.contains(&item) {
                            stack.push(item);
                        }
                    }
                }
            }
            set.insert(item);
        }

        set
    }

    /// Builds goto set for the state when the next token
    /// of the item equals `next_token`
    fn goto<'a>(
        &'a self,
        next_token: &LexicalToken<T>,
        state: impl Iterator<Item = &'a Item<T, F>>,
    ) -> ItemSet<T, F> {
        let mut res = BTreeSet::new();

        for item in state {
            let next = match item.next_symbol() {
                Some(n) => n,
                None => continue,
            };

            if next == next_token {
                if let Some(item) = Item::moving(&item) {
                    res.insert(item);
                }
            }
        }

        self.closure(res.into_iter())
    }

    /// Build canonical collection and trace of items
    fn build_canonical(&self) -> (Canonical<T, F>, Trace<T>) {
        let mut canonical = HashMap::new();
        let mut trace: Trace<T> = HashMap::new();
        let mut stack = Vec::<(ItemSet<T, F>, usize)>::new();

        // 0 state is an ∅ state in the Parser tables
        let mut state_name = 1usize;

        /*
        This is an indirection set used to check is the built BTreeSet
        was already processed or added to processing queue.
        The value of the hash map is goto state name and is used
        to build trace table.

        The set stores a hash of BTreeSet<Item<T>> such as
        calculation of hash is potentially faster than using
        HashSet<BTreeSet<Item<T>>> as it was implemented in prototype.
        Also, it uses less amount of memory.
        However, this requires calculation of two hashes:
        - one for BTreeSet<Item<T>>
        - second one for u64
        */
        let mut indirect_map = HashMap::<u64, usize>::new();

        let mut initial = BTreeSet::new();
        for rule in self.grammar.rules() {
            if rule.production().lhs().lexeme() == "goal" {
                // TODO: make "goal" lexeme settable?
                initial.insert(Item::new(
                    rule.production().clone(),
                    self.grammar.eof().clone(),
                    rule.handle().clone(),
                ));
            }
        }
        stack.push((self.closure(initial.into_iter()), state_name));

        while let Some((state, name)) = stack.pop() {
            for item in state.iter() {
                if let Some(next_token) = item.next_symbol() {
                    let goto = self.goto(next_token, state.iter());
                    let goto_hash = canonical.hasher().hash_one(&goto);
                    if let Some(goto_name) = indirect_map.get(&goto_hash) {
                        trace
                            .entry(name)
                            .or_default()
                            .insert(next_token.clone(), *goto_name);
                    } else {
                        state_name += 1;
                        indirect_map.insert(goto_hash, state_name);
                        stack.push((goto, state_name));
                        trace
                            .entry(name)
                            .or_default()
                            .insert(next_token.clone(), state_name);
                    }
                }
            }
            canonical.insert(state, name);
        }

        (canonical, trace)
    }

    pub fn build(&self) -> Parser<T, F> {
        let (canonical, trace) = self.build_canonical();
        let terminals = self.grammar.terminals();
        let nonterminals = self.grammar.nonterminals();

        let mut actions: ActionTable<T, F> = HashMap::with_capacity(canonical.len());
        let mut goto: GotoTable<T> = HashMap::with_capacity(canonical.len());

        for (set, i) in canonical {
            for item in set {
                match item.next_symbol() {
                    Some(n) => {
                        if terminals.contains(n) {
                            let entry = actions.entry(i).or_default();
                            if entry.contains_key(n) {
                                println!("Shift Error"); // TODO: is it?
                            } else {
                                if let Some(t) = trace.get(&i) {
                                    entry
                                        .insert(n.clone(), Action::Shift(*t.get(&n).unwrap_or(&0)));
                                }
                                // entry.insert(
                                //     n.clone(),
                                //     Action::Shift(*trace.get(&i).unwrap().get(n).unwrap_or(&0)),
                                // );
                            }
                        }
                    }
                    None => {
                        if item.production().lhs().lexeme() == "goal"
                            && item.lookahead() == self.grammar.eof()
                        {
                            let entry = actions.entry(i).or_default();
                            entry.insert(self.grammar.eof().clone(), Action::Accept(item));
                        } else {
                            let entry = actions.entry(i).or_default();
                            entry.insert(item.lookahead().clone(), Action::Reduce(item));
                        }
                    }
                }
            }

            for token in nonterminals.iter() {
                if let Some(t) = trace.get(&i) {
                    goto.entry(i).or_default().insert(
                        (*token).clone(),
                        *t.get(token).unwrap_or(&0),
                    );
                }
            }
        }

        Parser::new(actions, goto)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        grammar::{Grammar, Item, LexicalToken, NonTermToken, Rule, Terminal},
        parser::ParserBuilder,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum TokenKind {
        LeftParen,
        RightParen,
        EOF,
        EPS,
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Token<'source> {
        lexeme: &'source str,
        kind: TokenKind,
    }

    impl<'source> Token<'source> {
        fn new(kind: TokenKind, lexeme: &'source str) -> Self {
            Self { kind, lexeme }
        }
    }

    impl<'source> Terminal for Token<'source> {
        fn lexeme(&self) -> &str {
            &self.lexeme
        }
    }

    #[derive(Debug)]
    enum Expr {
        String(String),
        Int(i32),
        Binary(Box<Expr>, String, Box<Expr>),
    }

    fn handle(_: Expr, t: LexicalToken<Token>) -> Expr {
        println!("{:#?}", t);
        unimplemented!()
    }

    fn brackets_grammar() -> Grammar<Token<'static>, Expr, fn(Expr, LexicalToken<Token<'_>>) -> Expr>
    {
        Grammar::new(
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
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(")),
                        LexicalToken::NonTerm(NonTermToken::new("list")),
                        LexicalToken::Term(Token::new(TokenKind::RightParen, ")")),
                    ],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("pair")),
                    vec![
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(")),
                        LexicalToken::Term(Token::new(TokenKind::RightParen, ")")),
                    ],
                    handle,
                ),
            ],
            Token::new(TokenKind::EOF, "EOF"),
            Token::new(TokenKind::EPS, "EPS"),
        )
    }

    #[test]
    fn build_collection() {
        let grammar = brackets_grammar();
        let builder = ParserBuilder::new(grammar);
        let c = builder.build_canonical();

        let c_vec: Vec<(_, _)> = {
            let mut v: Vec<(
                std::collections::BTreeSet<
                    Item<Token<'_>, fn(Expr, LexicalToken<Token<'_>>) -> Expr>,
                >,
                usize,
            )> = c.0.into_iter().collect();
            v.sort_by_key(|&(_, v)| v);
            v
        };

        for (k, v) in c_vec {
            println!("Collection {v}");
            for item in k {
                println!("{item}");
            }
            println!();
        }

        println!();
        let c_vec: Vec<(_, _)> = {
            let mut v: Vec<(usize, _)> = c.1.into_iter().collect();
            v.sort_by_key(|&(k, _)| k);
            v
        };
        for (k, v) in c_vec {
            let data: Vec<String> = v.into_iter().map(|(k, v)| format!("({k}, {v})")).collect();
            println!("{k}: [{}]", data.join(", "));
        }
    }

    #[test]
    fn build_parser() {
        let grammar = brackets_grammar();
        let builder = ParserBuilder::new(grammar);

        let parser = builder.build();
        let mut actions: Vec<(_, _)> = parser.actions.into_iter().collect();
        actions.sort_by_key(|&(k, _)| k);

        println!("Actions table");
        for (k, v) in actions {
            let data: Vec<String> = v.into_iter().map(|(k, v)| format!("({k}, {v})")).collect();
            println!("{k}: [{}]", data.join(", "));
        }
        println!("\nGoto table");

        let mut goto: Vec<(_, _)> = parser.goto.into_iter().collect();
        goto.sort_by_key(|&(k, _)| k);
        for (k, v) in goto {
            let data: Vec<String> = v.into_iter().map(|(k, v)| format!("({k}, {v})")).collect();
            println!("{k}: [{}]", data.join(", "));
        }
    }
}
