pub trait TokenKind {
    fn human(&self) -> &str;
}

pub trait Token {
    fn lexeme(&self) -> &str;
    fn kind<K: TokenKind>(&self) -> K;
}

#[derive(Debug)]
pub struct Rule<T: Token> {
    lhs: T,
    rhs: Vec<T>,
}

#[derive(Debug)]
pub struct Grammar<T: Token> {
    rules: Vec<Rule<T>>
}
