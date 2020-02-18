use super::Name;
use crate::lexer::Span;
use crate::raw::Spanned;
use crate::source_file::FileMap;
use annotate_snippets::snippet::Snippet;
// use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

#[derive(Debug, Clone)]
pub enum Error {
    // import errors
    /// The same library was imported multiple times
    DuplicateImport {
        import: String,
        orig: Span,
        dupe: Span,
    },
    /// Two different libraries are imported under the same name
    ImportNameConflict {
        name: String,
        orig: Span,
        dupe: Span,
    },
    /// A library specified in a using statement was not found in this library's dependencies
    /// (i.e. was not included in a set of --files args)
    DependencyNotFound(Span),
    /// A library specified in a using statement was not used anywhere in the file
    UnusedImport(Span),

    // resolution errors
    /// No definition was found for the variable referenced at Span
    Undefined(Span),
    /// The reference found at `span` is ambiguous, and can be interpreted as
    /// either `interp1` or `interp2`. Note that the Spans for the two interpretations
    /// correpond to where the referee is defined, not where the reference is located
    AmbiguousReference {
        span: Span,
        interp1: Spanned<Name>,
        interp2: Spanned<Name>,
    },

    /// Two libraries were provided that have the same name
    DuplicateLibrary,
}

impl Error {
    pub fn into_snippet(self, _srcs: &FileMap) -> Snippet {
        unimplemented!()
    }
}
