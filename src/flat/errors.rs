use super::Name;
use crate::errors::{span_to_snippet, two_spans_to_snippet, ErrText};
use crate::lexer::Span;
use crate::raw::Spanned;
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

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
    UndefinedLocal(Span),
    Undefined(Span, Name, Name),
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
    pub fn into_snippet(self, srcs: &FileMap) -> Snippet {
        use Error::*;
        match self {
            DuplicateImport { import, orig, dupe } => two_spans_to_snippet(
                orig,
                dupe,
                srcs,
                ErrText {
                    text: "duplicate library import".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("library {} imported here", import),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "also imported here".to_string(),
                    ty: AnnotationType::Info,
                },
                Some(Annotation {
                    label: Some("remove one of the imports".to_string()),
                    id: None,
                    annotation_type: AnnotationType::Help,
                }),
            ),
            ImportNameConflict { name, orig, dupe } => two_spans_to_snippet(
                orig,
                dupe,
                srcs,
                ErrText {
                    text: "name conflict in library imports".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("library imported under name {}", name),
                    ty: AnnotationType::Info,
                },
                ErrText {
                    text: "clashes with import here".to_string(),
                    ty: AnnotationType::Info,
                },
                None,
            ),
            DependencyNotFound(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "imported library not found".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "library was not found in specified dependencies".to_string(),
                    ty: AnnotationType::Error,
                },
                Some(Annotation {
                    label: Some("did you include it with --files?".to_string()),
                    id: None,
                    annotation_type: AnnotationType::Help,
                }),
            ),
            UnusedImport(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "unused import".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "import here is not referenced in the rest of the file".to_string(),
                    ty: AnnotationType::Info,
                },
                Some(Annotation {
                    label: Some("consider removing this import".to_string()),
                    id: None,
                    annotation_type: AnnotationType::Help,
                }),
            ),
            UndefinedLocal(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "undefined name".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "could not find definition for this variable".to_string(),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            _ => unimplemented!(),
        }
    }
}
