use std::collections::HashSet;

use crate::grammar::{FirstSet, Grammar, Terminal};

pub fn closure<'grammar, T: Terminal, U, F>(
    grammar: &'grammar Grammar<T, U, F>,
    first: FirstSet<'grammar>,
    initial: HashSet<&'grammar str>,
) -> HashSet<&'grammar str> {
    todo!()
}
