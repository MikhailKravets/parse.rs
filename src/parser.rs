use std::collections::HashSet;

use crate::grammar::{FirstSet, Grammar, Item, Terminal};

pub fn closure<'grammar, T: Terminal, U, F>(
    grammar: &'grammar Grammar<T, U, F>,
    first: FirstSet<'grammar>,
    initial: HashSet<&'grammar Item<T>>,
) -> HashSet<&'grammar Item<T>> {
    todo!()
}

// TODO: to be able to unite Item sets with similar productions but
//       different lookahead. How to make a merging algorithm efficient?