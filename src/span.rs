use std::cmp::Ordering;
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

// template on Pos type?
#[derive(Copy, Clone, Default, Eq, Debug)]
pub struct Span<Pos> {
    pub start: Pos,
    pub end: Pos,
}

impl<Pos> PartialEq for Span<Pos>
where
    Pos: PartialEq,
{
    fn eq(&self, other: &Span<Pos>) -> bool {
        self.start == other.start && self.end == other.end
    }
}

impl<Pos> PartialOrd for Span<Pos>
where
    Pos: PartialOrd,
{
    fn partial_cmp(&self, other: &Span<Pos>) -> Option<Ordering> {
        self.start.partial_cmp(&other.start).and_then(|ord| {
            if ord == Ordering::Equal {
                self.end.partial_cmp(&self.end)
            } else {
                Some(ord)
            }
        })
    }
}

impl<Pos> Ord for Span<Pos>
where
    Pos: Ord,
{
    fn cmp(&self, other: &Span<Pos>) -> Ordering {
        let ord = self.start.cmp(&other.start);
        if ord == Ordering::Equal {
            self.end.cmp(&self.end)
        } else {
            ord
        }
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

pub fn spanned<T, Pos>(start: Pos, end: Pos, value: T) -> Spanned<T, Pos> {
    Spanned {
        span: Span { start, end },
        value: value,
    }
}
