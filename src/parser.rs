use std::{
    collections::{BTreeSet, HashMap},
    hash::{BuildHasher, Hash},
    ops::Deref,
};

use crate::grammar::{FirstSet, Grammar, Item, LexicalToken, Terminal};

/// Type alias for a set of grammar items
type ItemSet<T> = BTreeSet<Item<T>>;

/// Type alias for canonical collection
type Canonical<T> = HashMap<ItemSet<T>, usize>;

/// Type alias for canonical trace table
type Trace<T> = HashMap<usize, HashMap<LexicalToken<T>, usize>>;

/// Type alias for actions table
type ActionTable<T> = HashMap<usize, HashMap<LexicalToken<T>, Action<T>>>;

/// Type alias for goto table
type GotoTable<T> = HashMap<usize, HashMap<LexicalToken<T>, usize>>;

#[derive(Debug)]
pub enum Action<T: Terminal> {
    Shift(usize),
    Reduce(Item<T>),
    Accept(Item<T>),
}

#[derive(Debug)]
/// A simple container for parses tables
pub struct Parser<T: Terminal> {
    actions: ActionTable<T>,
    goto: GotoTable<T>,
}

impl<T: Terminal> Parser<T> {
    pub fn new(actions: ActionTable<T>, goto: GotoTable<T>) -> Self {
        Self { actions, goto }
    }
}

pub struct ParserBuilder<T: Terminal, U, F> {
    grammar: Grammar<T, U, F>,
    first: FirstSet<T>,
}

impl<T: Terminal + Clone + Eq + Ord + Hash, U, F> From<Grammar<T, U, F>>
    for ParserBuilder<T, U, F>
{
    fn from(value: Grammar<T, U, F>) -> Self {
        Self::new(value)
    }
}

impl<T: Terminal + Clone + Eq + Ord + Hash, U, F> ParserBuilder<T, U, F> {
    pub fn new(grammar: Grammar<T, U, F>) -> Self {
        Self {
            first: FirstSet::from(&grammar),
            grammar,
        }
    }

    /// Builds closure set for the initial items
    fn closure(&self, initial: impl Iterator<Item = Item<T>>) -> ItemSet<T> {
        let mut stack: Vec<Item<T>> = initial.collect();
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
                        let item = Item::new(r.production().clone(), l.clone());
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
        state: impl Iterator<Item = &'a Item<T>>,
    ) -> ItemSet<T> {
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
    fn build_canonical(&self) -> (Canonical<T>, Trace<T>) {
        let mut canonical = HashMap::new();
        let mut trace: Trace<T> = HashMap::new();
        let mut stack = Vec::<(ItemSet<T>, usize)>::new();
        let mut state_name = 0usize;

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

    pub fn build(&self) -> Parser<T> {
        // TODO: decide how to store rules and handles
        let (canonical, trace) = self.build_canonical();
        let terminals = self.grammar.terminals();
        let nonterminals = self.grammar.nonterminals();

        let mut actions: ActionTable<T> = HashMap::with_capacity(canonical.len());
        let mut goto: GotoTable<T> = HashMap::with_capacity(canonical.len());

        for (set, i) in canonical {
            for item in set {
                match item.next_symbol() {
                    Some(n) => {
                        if terminals.contains(n) {
                            let entry = actions.entry(i).or_default();
                            if entry.contains_key(n) {
                                println!("Shift Error");
                            } else {
                                entry.insert(
                                    n.clone(),
                                    Action::Shift(*trace.get(&i).unwrap().get(n).unwrap()), // TODO: what if trace doesn't have the entry?
                                );
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
                goto.entry(i).or_default().insert(
                    (*token).clone(),
                    *trace.get(&i).unwrap().get(token).unwrap(),
                );
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

    fn handle(_: (), t: LexicalToken<Token>) {
        println!("{:#?}", t)
    }

    fn brackets_grammar() -> Grammar<Token<'static>, (), fn((), LexicalToken<Token<'static>>)> {
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
            let mut v: Vec<(std::collections::BTreeSet<Item<Token<'_>>>, usize)> =
                c.0.into_iter().collect();
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
        println!("{:#?}", parser)
    }
}
