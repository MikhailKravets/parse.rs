use crate::span::Span;
use crate::lexer::kind::TokenKind;

#[derive(Debug, Copy, Clone)]
pub struct Token<'source> {
    kind: TokenKind,
    lexeme: &'source str,
    span: Span,
}

impl<'source> Token<'source> {
    pub fn new(kind: TokenKind, lexeme: &'source str, span: Span) -> Self {
        Self { kind, lexeme, span }
    }

    pub fn simple(kind: TokenKind, position: Span) -> Self {
        Self { kind, lexeme: "", span: position }
    }

    pub fn init() -> Self {
        Self { kind: TokenKind::Init, lexeme: "", span: Span::default() }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl<'source> Token<'source> {
    /// Returns human-readable string for the token
    pub fn human(&self) -> String {
        match self.kind {
            TokenKind::LeftParen => "(".into(),
            TokenKind::RightParen => ")".into(),
            TokenKind::LeftBrace => "{".into(),
            TokenKind::RightBrace => "}".into(),

            TokenKind::Comma => ",".into(),
            TokenKind::Dot => ".".into(),

            TokenKind::Minus => "-".into(),
            TokenKind::Plus => "+".into(),
            TokenKind::Slash => "/".into(),
            TokenKind::Star => "*".into(),

            TokenKind::Semicolon => ";".into(),

            TokenKind::Bang => "!".into(),
            TokenKind::BangEqual => "!=".into(),
            TokenKind::Equal => "=".into(),
            TokenKind::EqualEqual => "==".into(),
            TokenKind::Greater => ">".into(),
            TokenKind::GreaterEqual => ">=".into(),
            TokenKind::Less => "<".into(),
            TokenKind::LessEqual => "<=".into(),

            TokenKind::Identifier => self.lexeme().into(),
            TokenKind::String => self.lexeme().into(),
            TokenKind::Number => self.lexeme().into(),

            TokenKind::And => "and".into(),
            TokenKind::Or => "or".into(),

            TokenKind::Class => "class".into(),
            TokenKind::Super => "super".into(),
            TokenKind::This => "this".into(),

            TokenKind::Fun => "fun".into(),
            TokenKind::Return => "return".into(),
            TokenKind::For => "for".into(),
            TokenKind::While => "while".into(),

            TokenKind::Break => "break".into(),
            TokenKind::Continue => "continue".into(),

            TokenKind::If => "if".into(),
            TokenKind::Else => "else".into(),

            TokenKind::Nil => "nil".into(),
            TokenKind::Print => "print".into(),

            TokenKind::True => "true".into(),
            TokenKind::False => "false".into(),

            TokenKind::Var => "var".into(),

            TokenKind::EOF => "EOF".into(),
            TokenKind::Error => self.lexeme().into(),

            TokenKind::Init => "special init token".into(),
        }
    }
}