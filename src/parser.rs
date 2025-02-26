use std::{
    collections::{BTreeSet, HashMap, HashSet},
    hash::{BuildHasher, Hash, Hasher},
};

use crate::grammar::{FirstSet, Grammar, Item, LexicalToken, Terminal};

// TODO: to be able to unite Item sets with similar productions but
//       different lookahead. How to make a merging algorithm efficient?

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

    fn closure(&self, initial: impl Iterator<Item = Item<T>>) -> BTreeSet<Item<T>> {
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

    fn goto<'a>(
        &'a self,
        next_symbol: &LexicalToken<T>,
        state: impl Iterator<Item = &'a Item<T>>,
    ) -> BTreeSet<Item<T>> {
        let mut res = BTreeSet::new();

        for item in state {
            let next = match item.next_symbol() {
                Some(n) => n,
                None => continue,
            };

            if next == next_symbol {
                if let Some(item) = Item::moving(&item) {
                    res.insert(item);
                }
            }
        }

        self.closure(res.into_iter())
    }

    /// Build canonical collection of items
    pub fn build(&self) -> HashMap<BTreeSet<Item<T>>, usize> {
        /*
        TODO: this approach uses BTReeSet. Is there a need to use B-Tree?
              Each state will have at most dozens of items, perhaps, it's
              better to just use sorted Vec?
         */
        let mut canonical = HashMap::<BTreeSet<Item<T>>, usize>::new();
        let mut stack = Vec::<(BTreeSet<Item<T>>, usize)>::new();
        let mut state_name = 0usize;

        // This is an indirection set used to check is the built BTreeSet
        // was already processed or added to processing queue.
        //
        // The set stores a hash of BTreeSet<Item<T>> such as
        // calculation of hash is potentially faster than using
        // HashSet<BTreeSet<Item<T>>> as it was implemented in prototype.
        // Also, it uses less amount of memory.
        // However, this requires calculation of two hashes:
        // - one for BTreeSet<Item<T>>
        // - second one for u64
        let mut indirect_set = HashSet::<u64>::new();

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

        // TODO: build goto trace table as well!
        while let Some((state, name)) = stack.pop() {
            for item in state.iter() {
                if let Some(token) = item.next_symbol() {
                    let goto = self.goto(token, state.iter());
                    let goto_hash = canonical.hasher().hash_one(&goto);
                    if !indirect_set.contains(&goto_hash) {
                        state_name += 1;
                        indirect_set.insert(goto_hash);
                        stack.push((goto, state_name));
                    }
                }
            }
            canonical.insert(state, name);
        }

        canonical
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

    #[test]
    fn build_brackets_grammar() {
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
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(")),
                        LexicalToken::NonTerm(NonTermToken::new("list")),
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, ")")),
                    ],
                    handle,
                ),
                Rule::new(
                    LexicalToken::NonTerm(NonTermToken::new("pair")),
                    vec![
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, "(")),
                        LexicalToken::Term(Token::new(TokenKind::LeftParen, ")")),
                    ],
                    handle,
                ),
            ],
            Token::new(TokenKind::EOF, "EOF"),
            Token::new(TokenKind::EPS, "EPS"),
        );
        let builder = ParserBuilder::new(grammar);
        let c = builder.build();
        
        let c_vec: Vec<(_, _)> = {
            let mut v: Vec<(std::collections::BTreeSet<Item<Token<'_>>>, usize)> = c.into_iter().collect();
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
    }
}
