use super::*;
use crate::lexer::Span;
use crate::source_file::FileMap;
use annotate_snippets::snippet::Snippet;

#[derive(Debug, Clone)]
pub enum Error {
    SortError {
        /// Span of the occurence of this error
        span: Span,
        expected: Sort,
        actual: Sort,
    },

    // these kind errors can essentially be merged into a single
    // UnexpectedKind { expected, actual } error, though mentioning kinds to users
    // can be kind of cryptic, so they're separated out further
    InvalidTypeParam {
        func_call: Span,
        param: ParamType,
        // this Span is optional since it only makes sense to provide it if `func`
        // is a user defined type. If it's a builtin (or, eventually, if it's an
        // inline/anonymous type), the func call span will include the definition
        func_def: Option<Span>,
    },
    NonConcreteType {
        span: Span,
        missing: Vec<ParamType>,
    },

    VarCycle(Vec<(Name, Span)>),

    TypeError {
        actual: Spanned<Type>,
        expected: Type,
    },
    IntCoercionError {
        span: Span,
        ty: Type,
    },
    StringBoundsError {
        span: Span,
        length: usize,
        bounds: Spanned<u64>,
    },

    InvalidBitsType(Spanned<Type>),

    NullableTableMember(Span),
    NullableUnionMember(Span),
}

#[derive(Debug, Copy, Clone)]
pub enum ParamType {
    Constraint,
    Layout,
}

impl Error {
    pub fn into_snippet(self, _srcs: &FileMap) -> Snippet {
        use Error::*;
        match self {
            VarCycle(_cycle) => {
                // note: the top level error name will be the last name, since
                // the first element in the vector is the rhs of the first type
                // that caused this error
                // so the title should be: detected cycle for name N, followed
                // by snippet_i = span_i, which requires processing name_i...
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }
}
