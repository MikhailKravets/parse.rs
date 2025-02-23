#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Span {
    cursor: usize,
    length: usize,

    line: usize
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize) -> Self {
        Self {
            cursor: start,
            length: end - start,
            line
        }
    }

    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    #[inline]
    pub fn length(&self) -> usize {
        self.length
    }

    #[inline]
    pub fn set_cursor(&mut self, new_cursor: usize) {
        self.cursor = new_cursor;
    }

    #[inline]
    pub fn set_length(&mut self, length: usize) {
        self.length = length;
    }

    #[inline]
    pub fn line(&self) -> usize {
        self.line
    }

    #[inline]
    pub fn set_line(&mut self, line: usize) {
        self.line = line;
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            cursor: 0,
            length: 1,
            line: 1,
        }
    }
}