use crate::errors::{span_to_snippet, spans_to_snippet, two_spans_to_snippet, ErrText};
use crate::lexer::Span;
use crate::raw::Spanned;
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Snippet};
use std::fmt;

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
    Undefined(Span, RawName, RawName),
    /// The reference found at `span` is ambiguous, and can be interpreted as
    /// either `interp1` or `interp2`. Note that the Spans for the two interpretations
    /// correpond to where the referee is defined, not where the reference is located
    AmbiguousReference {
        span: Span,
        interp1: Spanned<RawName>,
        interp2: Spanned<RawName>,
    },

    ConstrainedHandle(Span),
    InvalidHandleSubtype(Span),

    MissingEndArg(Span),
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
            Undefined(span, name1, name2) => span_to_snippet(
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
                Some(Annotation {
                    label: Some(format!("both {} and {} are undefined", name1, name2)),
                    id: None,
                    annotation_type: AnnotationType::Info,
                }),
            ),
            AmbiguousReference {
                span,
                interp1,
                interp2,
            } => spans_to_snippet(
                ErrText {
                    text: "ambiguous reference".to_string(),
                    ty: AnnotationType::Error,
                },
                vec![
                    (
                        span,
                        ErrText {
                            text: "multiple ways to resolve this reference".to_string(),
                            ty: AnnotationType::Error,
                        },
                    ),
                    (
                        interp1.span,
                        ErrText {
                            text: format!("reference could be interpreted as {}", interp1.value),
                            ty: AnnotationType::Info,
                        },
                    ),
                    (
                        interp2.span,
                        ErrText {
                            text: format!("or as {}", interp2.value),
                            ty: AnnotationType::Info,
                        },
                    ),
                ],
                srcs,
                None,
            ),
            ConstrainedHandle(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "constrained handle".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "handles do not allow constraints, please remove".to_string(),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            InvalidHandleSubtype(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "invalid handle subtype".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "this is not a valid handle subtype".to_string(),
                    ty: AnnotationType::Error,
                },
                Some(Annotation {
                    label: Some(
                        "handle subtypes must be one of `vmo`, `channel`, todo: list the rest..."
                            .to_string(),
                    ),
                    id: None,
                    annotation_type: AnnotationType::Info,
                }),
            ),
            MissingEndArg(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "missing endpoint arg".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "`client_end` and `server_end` must have a layout arg specified"
                        .to_string(),
                    ty: AnnotationType::Error,
                },
                None,
            ),
        }
    }
}

// This is a Name, but where the library field is a raw string instead of a resolved
// library ID. This is used as an intermediate state between when we interpret a
// raw::CompoundIdentifier and fully resolve it to a Name. Names are also converted
// back to a RawName when we want to display it
#[derive(Debug, Clone)]
pub struct RawName {
    pub library: Option<String>,
    pub name: String,
    pub member: Option<String>,
}

impl fmt::Display for RawName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lib_str = match &self.library {
            Some(lib) => format!("library {}", lib),
            None => "the current library".to_string(),
        };
        let var_str = match &self.member {
            Some(member) => format!("{}.{}", self.name, member),
            None => format!("{}", self.name),
        };
        write!(f, "{} in {}", var_str, lib_str)
    }
}
