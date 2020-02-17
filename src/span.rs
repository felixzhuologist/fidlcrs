// use std::cmp::Ordering;
use std::fmt;

// TODO: make Line, Col, ByteOffset types?
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct Location {
    pub line: usize,
    pub col: usize,
    pub absolute: usize,
}

impl Location {
    pub fn shift(mut self, ch: char) -> Location {
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        self.absolute += ch.len_utf8();
        self
    }
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct FileId(pub usize);

// template on Pos type?
#[derive(Copy, Clone, Eq, Debug)]
pub struct Span<Pos> {
    pub file: FileId,
    pub start: Pos,
    pub end: Pos,
}

impl<Pos> Span<Pos> {
    pub fn wrap<T>(self, value: T) -> Spanned<T, Pos> {
        Spanned { value, span: self }
    }
}

impl<Pos> PartialEq for Span<Pos>
where
    Pos: PartialEq,
{
    fn eq(&self, other: &Span<Pos>) -> bool {
        self.start == other.start && self.end == other.end && self.file == other.file
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Spanned<T, Pos> {
    pub span: Span<Pos>,
    pub value: T,
}

impl<T, Pos> Spanned<T, Pos> {
    pub fn map<U, F>(self, mut f: F) -> Spanned<U, Pos>
    where
        F: FnMut(T) -> U,
    {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }

    pub fn try_map<U, E, F>(self, mut f: F) -> Result<Spanned<U, Pos>, E>
    where
        F: FnMut(T) -> Result<U, E>,
    {
        let result = f(self.value)?;
        Ok(Spanned {
            span: self.span,
            value: result,
        })
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Line: {}, Column: {}", self.line, self.col)
    }
}

pub fn spanned<T, Pos>(file: FileId, start: Pos, end: Pos, value: T) -> Spanned<T, Pos> {
    Spanned {
        span: Span { file, start, end },
        value: value,
    }
}
