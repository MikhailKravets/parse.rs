mod kind;
mod token;

use crate::scanner::Scanner;
pub use kind::TokenKind;
pub use token::Token;

pub(crate) struct Lexer<'source> {
    scanner: Scanner<'source>,
    line: usize
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            scanner: Scanner::new(source),
            line: 1
        }
    }

    pub fn next(&mut self) -> Token<'source> {
        loop {
            let mut pos = self.scanner.position();
            let c = self.scanner.next();

            pos.set_line(self.line);

            match c {
                Some('(') => return Token::simple(TokenKind::LeftParen, pos),
                Some(')') => return Token::simple(TokenKind::RightParen, pos),
                Some('{') => return Token::simple(TokenKind::LeftBrace, pos),
                Some('}') => return Token::simple(TokenKind::RightBrace, pos),

                Some(',') => return Token::simple(TokenKind::Comma, pos),
                Some('.') => return Token::simple(TokenKind::Dot, pos),
                Some('+') => return Token::simple(TokenKind::Plus, pos),
                Some('-') => return Token::simple(TokenKind::Minus, pos),
                Some('*') => return Token::simple(TokenKind::Star, pos),
                Some(';') => return Token::simple(TokenKind::Semicolon, pos),

                Some('!') => {
                    if self.scanner.next_if(|c| c == '=').is_some() {
                        return Token::simple(TokenKind::BangEqual, pos);
                    } else {
                        return Token::simple(TokenKind::Bang, pos);
                    }
                }
                Some('=') => {
                    if self.scanner.next_if(|c| c == '=').is_some() {
                        return Token::simple(TokenKind::EqualEqual, pos);
                    } else {
                        return Token::simple(TokenKind::Equal, pos);
                    }
                }
                Some('<') => {
                    if self.scanner.next_if(|c| c == '=').is_some() {
                        return Token::simple(TokenKind::LessEqual, pos);
                    } else {
                        return Token::simple(TokenKind::Less, pos);
                    }
                }
                Some('>') => {
                    if self.scanner.next_if(|c| c == '=').is_some() {
                        return Token::simple(TokenKind::GreaterEqual, pos);
                    } else {
                        return Token::simple(TokenKind::Greater, pos);
                    }
                }

                Some('/') => {
                    if self.scanner.next_if(|c| c == '/').is_some() {
                        self.scanner.next_while(|c| c != '\n');
                    } else {
                        return Token::simple(TokenKind::Slash, pos);
                    }
                }

                Some('"') => {
                    let s = self.string();
                    pos.set_length(s.len());
                    return Token::new(TokenKind::String, s, pos);
                }
                Some(c) if c.is_numeric() => {
                    let num = self.number();
                    pos.set_length(num.len());
                    return Token::new(TokenKind::Number, num, pos);
                }
                Some(c) if c.is_alphabetic() => {
                    self.scanner.back(c);
                    let s = self.scanner.next_while(|c| c.is_alphanumeric() || c == '_');
                    pos.set_length(s.len());

                    if let Some(kind) = self.keywords(s) {
                        return Token::new(kind, s, pos);
                    } else {
                        return Token::new(TokenKind::Identifier, s, pos);
                    }
                }

                Some(' ') | Some('\r') | Some('\t') => {
                    self.scanner
                        .next_while(|c| c == ' ' || c == '\r' || c == '\t');
                }
                Some('\n') => {
                    self.line += 1;
                }
                Some(_) => {
                    return Token::new(
                        TokenKind::Error,
                        self.scanner.at(pos.cursor(), pos.cursor() + 1),
                        pos,
                    );
                }
                None => return Token::simple(TokenKind::EOF, pos),
            }
        }
    }

    fn string(&mut self) -> &'source str {
        let s = self.scanner.next_while(|c| c != '"');
        // Skip last " character
        self.scanner.next();

        s
    }

    fn number(&mut self) -> &'source str {
        // -1 because the first char from the number
        // was already consumed by self.next()
        let start = self.scanner.cursor() - 1;

        self.scanner.next_while(|c| c.is_numeric());
        self.scanner.next_if(|c| c == '.');
        self.scanner.next_while(|c| c.is_numeric());

        self.scanner.at(start, self.scanner.cursor())
    }

    fn keywords(&self, s: &str) -> Option<TokenKind> {
        match s {
            "and" => Some(TokenKind::And),
            "class" => Some(TokenKind::Class),
            "else" => Some(TokenKind::Else),
            "false" => Some(TokenKind::False),
            "for" => Some(TokenKind::For),
            "fun" => Some(TokenKind::Fun),
            "if" => Some(TokenKind::If),
            "nil" => Some(TokenKind::Nil),
            "or" => Some(TokenKind::Or),
            "print" => Some(TokenKind::Print),
            "return" => Some(TokenKind::Return),
            "super" => Some(TokenKind::Super),
            "this" => Some(TokenKind::This),
            "true" => Some(TokenKind::True),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            "break" => Some(TokenKind::Break),
            "continue" => Some(TokenKind::Continue),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexer_for(s: &str) -> Lexer {
        Lexer::new(s)
    }

    fn assert_token(token: Token, kind: TokenKind, lexeme: &str) {
        assert_eq!(token.kind(), kind);
        assert_eq!(token.lexeme(), lexeme);
    }

    #[test]
    fn brackets() {
        let mut lex = lexer_for("({})");

        assert_token(lex.next(), TokenKind::LeftParen, "");
        assert_token(lex.next(), TokenKind::LeftBrace, "");
        assert_token(lex.next(), TokenKind::RightBrace, "");
        assert_token(lex.next(), TokenKind::RightParen, "");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn math() {
        let mut lex = lexer_for("+ - \n * /");

        assert_token(lex.next(), TokenKind::Plus, "");
        assert_token(lex.next(), TokenKind::Minus, "");
        assert_token(lex.next(), TokenKind::Star, "");
        assert_token(lex.next(), TokenKind::Slash, "");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn double() {
        let mut lex = lexer_for("!= <= >= ==");

        assert_token(lex.next(), TokenKind::BangEqual, "");
        assert_token(lex.next(), TokenKind::LessEqual, "");
        assert_token(lex.next(), TokenKind::GreaterEqual, "");
        assert_token(lex.next(), TokenKind::EqualEqual, "");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn double_single() {
        let mut lex = lexer_for("! < > =");

        assert_token(lex.next(), TokenKind::Bang, "");
        assert_token(lex.next(), TokenKind::Less, "");
        assert_token(lex.next(), TokenKind::Greater, "");
        assert_token(lex.next(), TokenKind::Equal, "");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn strings() {
        let first = "Hello World!";
        let second = "tadaa";
        let s = format!("\"{first}\" \n \"{second}\"");
        let mut lex = lexer_for(&s);

        assert_token(lex.next(), TokenKind::String, first);
        assert_token(lex.next(), TokenKind::String, second);

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn numbers() {
        let mut lex = lexer_for("0.35 \n 10.4 \n 12 \n 12.45");

        assert_token(lex.next(), TokenKind::Number, "0.35");
        assert_token(lex.next(), TokenKind::Number, "10.4");
        assert_token(lex.next(), TokenKind::Number, "12");
        assert_token(lex.next(), TokenKind::Number, "12.45");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn variable_assignment() {
        let mut lex = lexer_for("var is_true = true;");

        assert_token(lex.next(), TokenKind::Var, "var");
        assert_token(lex.next(), TokenKind::Identifier, "is_true");
        assert_token(lex.next(), TokenKind::Equal, "");
        assert_token(lex.next(), TokenKind::True, "true");
        assert_token(lex.next(), TokenKind::Semicolon, "");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    /// This test case taken from code crafters
    #[test]
    fn codecrafters() {
        let mut lex = lexer_for("({*.,+*})");

        assert_token(lex.next(), TokenKind::LeftParen, "");
        assert_token(lex.next(), TokenKind::LeftBrace, "");
        assert_token(lex.next(), TokenKind::Star, "");
        assert_token(lex.next(), TokenKind::Dot, "");
        assert_token(lex.next(), TokenKind::Comma, "");
        assert_token(lex.next(), TokenKind::Plus, "");
        assert_token(lex.next(), TokenKind::Star, "");
        assert_token(lex.next(), TokenKind::RightBrace, "");
        assert_token(lex.next(), TokenKind::RightParen, "");

        assert_eq!(lex.next().kind(), TokenKind::EOF)
    }

    #[test]
    fn unexpected_token() {
        let mut lex = lexer_for("$m 42");
        assert_token(lex.next(), TokenKind::Error, "$");
    }
}