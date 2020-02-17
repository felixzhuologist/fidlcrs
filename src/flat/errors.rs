use crate::lexer::Span;
use crate::source_file::FileMap;
use annotate_snippets::snippet::Snippet;
// use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

#[derive(Debug, Clone)]
pub enum Error {
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
}

impl Error {
    pub fn into_snippet(self, _srcs: &FileMap) -> Snippet {
        unimplemented!()
    }
}
