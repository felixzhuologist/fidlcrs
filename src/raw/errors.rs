use crate::errors::{span_to_snippet, two_spans_to_snippet, ErrText};
use crate::lexer::Span;
use crate::raw::{FidlType, Spanned};
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

pub enum Error {
    DuplicateDefinition {
        original: Span,
        duplicate: Span,
        decl_type: FidlType,
        decl_name: String,
    },
    InvalidAttributePlacement {
        name: Spanned<String>,
        placement: FidlType,
    },
    InvalidAttributeValue {
        name: Spanned<String>,
        value: Spanned<String>,
    },
    InvalidLibraryName(Span),
    EmptyTableOrUnion(Span),
    OobOrdinals {
        decl_span: Span,
        ordinal_spans: Vec<Span>,
    },
    NonDenseOrdinals {
        decl_span: Span,
        name_span: Span,
        missing_ranges: Vec<(u32, u32)>,
    },
}

impl Error {
    pub fn into_snippet(self, srcs: &FileMap) -> Snippet {
        use Error::*;
        match self {
            // TODO: the span may be invalid for "Doc" attribute keys.

            // TODO: provide help for duplicate attributes (in particular doc comments)
            // if original.value.name.value == "Doc" || duplicate.value.name.value == "Doc" {
            //     footer.push(Annotation {
            //         label: Some(
            //             "doc comments are added as attributes with a key of \"Doc\""
            //                 .to_string(),
            //         ),
            //         id: None,
            //         annotation_type: AnnotationType::Help,
            //     });
            // }
            DuplicateDefinition {
                original,
                duplicate,
                decl_type,
                decl_name,
            } => two_spans_to_snippet(
                duplicate,
                original,
                srcs,
                ErrText {
                    text: "duplicate definition".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "duplicate definition".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("{} {} originally defined here", decl_type, decl_name),
                    ty: AnnotationType::Info,
                },
                None,
            ),
            InvalidAttributePlacement { name, placement } => span_to_snippet(
                name.span,
                srcs,
                ErrText {
                    text: "attribute not allowed".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!(
                        "attribute {} cannot be placed on a {}",
                        name.value, placement
                    ),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            InvalidAttributeValue { name, value } => span_to_snippet(
                value.span,
                srcs,
                ErrText {
                    text: "attribute value not allowed".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!(
                        "attribute {} cannot have value \"{}\"",
                        name.value, value.value
                    ),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            InvalidLibraryName(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "invalid library name".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "library name components must match ^[a-z][a-z0-9]*$".to_string(),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            EmptyTableOrUnion(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "empty table or union".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "tables and unions must have at least one non reserved member"
                        .to_string(),
                    ty: AnnotationType::Error,
                },
                Some(Annotation {
                    label: Some(
                        "try using an empty struct to define a placeholder member".to_string(),
                    ),
                    id: None,
                    annotation_type: AnnotationType::Help,
                }),
            ),
            OobOrdinals {
                decl_span,
                ordinal_spans,
            } => {
                let src = srcs.get_file(decl_span.file);
                let (line_start, source) = src.surrounding_lines(decl_span.start, decl_span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                let annotations: Vec<SourceAnnotation> = ordinal_spans
                    .into_iter()
                    .map(|span| SourceAnnotation {
                        label: "ordinal out of bounds".to_string(),
                        annotation_type: AnnotationType::Error,
                        range: (span.start - source_start, span.end - source_start),
                    })
                    .collect();

                Snippet {
                    title: Some(Annotation {
                        label: Some("invalid ordinal values".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![Annotation {
                        label: Some("ordinals be in [1, 0xffff_ffff]".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Help,
                    }],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations,
                    }],
                }
            }
            NonDenseOrdinals {
                decl_span,
                name_span,
                missing_ranges,
            } => {
                let src = srcs.get_file(decl_span.file);
                let (line_start, source) = src.surrounding_lines(decl_span.start, decl_span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                let missing_ranges = missing_ranges
                    .into_iter()
                    .map(|(start, end)| {
                        if start == end {
                            format!("{}", start)
                        } else {
                            format!("{} - {}", start, end)
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                Snippet {
                    title: Some(Annotation {
                        label: Some("non dense ordinals".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![Annotation {
                        label: Some(format!("missing ordinals: {}", missing_ranges)),
                        id: None,
                        annotation_type: AnnotationType::Help,
                    }],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: "declaration does not contain dense ordinals".to_string(),
                            annotation_type: AnnotationType::Error,
                            range: (name_span.start - source_start, name_span.end - source_start),
                        }],
                    }],
                }
            }
        }
    }
}
