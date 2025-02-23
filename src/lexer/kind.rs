#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Or,
    Class,
    Super,
    This,

    Fun,
    Return,

    For,
    While,
    Break,
    Continue,

    If,
    Else,
    Nil,
    Print,

    True,
    False,
    Var,

    // EOF
    #[allow(clippy::upper_case_acronyms)]
    EOF,

    // Below variants are special case. They don't
    // play any role in parsing

    /// Special Error Token.
    /// The only error may happen during lexing is `Unexpected Token` error.
    /// This can be delayed till the parsing, making the lexing process easier.
    Error,

    /// Special token which designates that parsing process is not yet
    /// started. This is easier than using Option<T> all the time.
    Init,
}

impl TokenKind {

    /// Returns an amount of enum variants to be used in parsing
    pub fn len() -> usize {
        TokenKind::EOF as usize + 1
    }

    /// Convert enum variant to usize
    pub fn to_usize(self) -> usize {
        self as usize
    }
}