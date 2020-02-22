use super::*;
use crate::lexer::Span;
use crate::source_file::FileMap;
use annotate_snippets::snippet::Snippet;

#[derive(Debug, Clone)]
pub enum Error {
    SortError { expected: Sort, actual: Sort },

    // these kind errors can essentially be merged into a single
    // UnexpectedKind { expected, actual } error, though mentioning kinds to users
    // can be kind of cryptic, so they're separated out further
    InvalidTypeParam {
        func_call: Span,
        param: ParamType,
        func_def: Span,
    },
    NonConcreteType {
        span: Span,
        missing: Vec<ParamType>,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum ParamType {
    Constraint,
    Layout,
}

impl Error {
    pub fn into_snippet(self, _srcs: &FileMap) -> Snippet {
        unimplemented!()
    }
}
