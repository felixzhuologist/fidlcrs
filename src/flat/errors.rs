use super::Name;
use crate::lexer::Span;
use crate::raw::Spanned;
use crate::source_file::FileMap;
use annotate_snippets::snippet::Snippet;
// use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

#[derive(Debug, Clone)]
pub enum Error {
    Undefined(Span),
    DuplicateImport {
        import: String,
        orig: Span,
        dupe: Span,
    },
    ImportNameConflict {
        name: String,
        orig: Span,
        dupe: Span,
    },
    /// The reference found at `span` is ambiguous, and can be interpreted as
    /// either `interp1` or `interp2`. Note that the Spans for the two interpretations
    /// correpond to where the referee is defined, not where the reference is located
    AmbiguousReference {
        span: Span,
        interp1: Spanned<Name>,
        interp2: Spanned<Name>,
    },
    DependencyNotFound(Span),
    DuplicateLibrary,
}

impl Error {
    pub fn into_snippet(self, _srcs: &FileMap) -> Snippet {
        unimplemented!()
    }
}
