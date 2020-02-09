use crate::lexer::Span;

pub enum Error {
    UnresolvedLocal(Span),
}
