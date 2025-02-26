use std::{collections::HashSet, hash::Hash};

use crate::grammar::{FirstSet, Grammar, Item, Terminal};

// TODO: to be able to unite Item sets with similar productions but
//       different lookahead. How to make a merging algorithm efficient?

pub struct ParserBuilder<T: Terminal, U, F> {
    grammar: Grammar<T, U, F>,
    first: FirstSet<T>,
}

impl<T: Terminal + Clone + Eq + Hash, U, F> From<Grammar<T, U, F>> for ParserBuilder<T, U, F> {
    fn from(value: Grammar<T, U, F>) -> Self {
        Self::new(value)
    }
}

impl<T: Terminal + Clone + Eq + Hash, U, F> ParserBuilder<T, U, F> {
    pub fn new(grammar: Grammar<T, U, F>) -> Self {
        Self {
            first: FirstSet::from(&grammar),
            grammar,
        }
    }

    pub fn closure(&self, initial: HashSet<Item<T>>) -> HashSet<Item<T>> {
        let mut stack: Vec<Item<T>> = initial.into_iter().collect();
        let mut set = HashSet::new();

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

    /// Build canonical collection of items
    pub fn build(&self) {
        // let first = FirstSet::from(&self.grammar);
        todo!()
    }
}
