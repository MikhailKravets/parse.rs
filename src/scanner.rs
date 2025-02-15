use std::{iter::Peekable, str::Chars};
use crate::span::Span;

pub struct Scanner<'source> {
    s: &'source str,
    it: Peekable<Chars<'source>>,

    pos: Span,
}

impl<'s> Scanner<'s> {
    #[inline]
    pub fn cursor(&self) -> usize {
        self.pos.cursor()
    }

    #[inline]
    pub fn position(&self) -> Span {
        self.pos
    }
}

impl<'source> Scanner<'source> {
    pub fn new(s: &'source str) -> Self {
        Self {
            s,
            it: s.chars().peekable(),
            pos: Span::default(),
        }
    }

    pub fn next(&mut self) -> Option<char> {
        match self.it.next() {
            Some(c) => {
                self.proceed(c);
                Some(c)
            }
            None => None,
        }
    }

    /// Move one character back.
    /// The method updates the cursor and resets
    /// an iterator.
    ///
    /// # Notes
    ///
    /// - `cursor` is saturated near lower `usize` bound;
    ///   the min value cursor takes is 0
    pub fn back(&mut self, c: char) {
        self.pos.set_cursor(self.pos.cursor().saturating_sub(c.len_utf8()));
        self.it = self.s[self.pos.cursor()..].chars().peekable();
    }

    pub fn at(&self, l: usize, r: usize) -> &'source str {
        let start = if l >= self.s.len() {
            self.s.len() - 1
        } else {
            l
        };

        let end = if r > self.s.len() { self.s.len() } else { r };

        &self.s[start..end]
    }

    pub fn next_if(&mut self, condition: impl Fn(char) -> bool) -> Option<char> {
        if let Some(c) = self.it.peek().cloned() {
            if condition(c) {
                self.proceed(c);
                self.it.next();
                return Some(c);
            }
        }

        None
    }

    // TODO: next_while doesn't consume the first char for which the
    //  condition doesn't hold. Should it?
    pub fn next_while(&mut self, condition: impl Fn(char) -> bool) -> &'source str {
        let start = self.pos.cursor();

        while let Some(c) = self.it.peek().copied() {
            if !condition(c) {
                // Safety! the range (start..self.cursor) is always in bounds
                // of a source string
                return unsafe { self.s.get_unchecked(start..self.pos.cursor()) };
            }

            self.proceed(c);
            self.it.next();
        }

        unsafe { self.s.get_unchecked(start..self.pos.cursor()) }
    }

    // TODO: benchmark with #[inline]
    fn proceed(&mut self, c: char) {
        self.pos.set_cursor(self.pos.cursor() + c.len_utf8());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_ascii() {
        let mut s = Scanner::new("ch\na");

        assert_eq!(s.next(), Some('c'));
        assert_eq!(s.next(), Some('h'));
        assert_eq!(s.next(), Some('\n'));
        assert_eq!(s.next(), Some('a'));
        assert_eq!(s.next(), None);
    }

    #[test]
    fn next_utf8() {
        let mut s = Scanner::new("Зд🙃");

        assert_eq!(s.next(), Some('З'));
        assert_eq!(s.next(), Some('д'));
        assert_eq!(s.next(), Some('🙃'));
        assert_eq!(s.next(), None);
    }

    #[test]
    fn next_if() {
        let mut s = Scanner::new("ch");

        assert_eq!(s.next_if(|c| c == 'c'), Some('c'));
        assert_eq!(s.next_if(|c| c == 'c'), None);
    }

    #[test]
    fn next_while() {
        let mut s = Scanner::new("line\"");
        assert_eq!(s.next_while(|c| c != '"'), "line");
    }

    #[test]
    fn next_while_to_the_end() {
        let mut s = Scanner::new("line");
        assert_eq!(s.next_while(|c| c != '"'), "line");
    }
}