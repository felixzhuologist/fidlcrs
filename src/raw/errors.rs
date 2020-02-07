use crate::raw::{FidlType, Spanned};
use crate::source_file::SourceFile;
use crate::span;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use std::cmp;
use std::fmt;

type Span = span::Span<usize>;

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
    EmptyTableOrUnion(Span, FidlType),
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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl Error {
    pub fn into_snippet(self, src: &SourceFile) -> Snippet {
        use Error::*;
        match self {
            DuplicateDefinition {
                original,
                duplicate,
                decl_type,
                decl_name,
            } => {
                let orig_span = original;
                let dupe_span = duplicate;
                let src_start = cmp::min(orig_span.start, dupe_span.start);
                let src_end = cmp::min(orig_span.end, dupe_span.end);
                let (line_start, source) = src.surrounding_lines(src_start, src_end);
                let source_start = src.lines.offset_at_line_number(line_start);

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

                Snippet {
                    title: Some(Annotation {
                        label: Some("duplicate attributes".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![
                            SourceAnnotation {
                                label: format!(
                                    "{} {} originally defined here",
                                    decl_type, decl_name
                                ),
                                annotation_type: AnnotationType::Info,
                                range: (
                                    orig_span.start - source_start,
                                    orig_span.end - source_start,
                                ),
                            },
                            SourceAnnotation {
                                label: format!("duplicate definition"),
                                annotation_type: AnnotationType::Error,
                                range: (
                                    dupe_span.start - source_start,
                                    dupe_span.end - source_start,
                                ),
                            },
                        ],
                    }],
                }
            }
            InvalidAttributePlacement { name, placement } => {
                let span = name.span;
                let (line_start, source) = src.surrounding_lines(span.start, span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                Snippet {
                    title: Some(Annotation {
                        label: Some("attribute not allowed".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: format!(
                                "attribute {} cannot be placed on a {}",
                                name.value, placement
                            ),
                            annotation_type: AnnotationType::Error,
                            range: (span.start - source_start, span.end - source_start),
                        }],
                    }],
                }
            }
            InvalidAttributeValue { name, value } => {
                let span = value.span;
                let (line_start, source) = src.surrounding_lines(span.start, span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                Snippet {
                    title: Some(Annotation {
                        label: Some("attribute value not allowed".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: format!(
                                "attribute {} cannot have value \"{}\"",
                                name.value, value.value
                            ),
                            annotation_type: AnnotationType::Error,
                            range: (span.start - source_start, span.end - source_start),
                        }],
                    }],
                }
            }
            InvalidLibraryName(span) => {
                let (line_start, source) = src.surrounding_lines(span.start, span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                Snippet {
                    title: Some(Annotation {
                        label: Some("invalid library name".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: "library name components must match ^[a-z][a-z0-9]*$"
                                .to_string(),
                            annotation_type: AnnotationType::Error,
                            range: (span.start - source_start, span.end - source_start),
                        }],
                    }],
                }
            }
            EmptyTableOrUnion(span, table_or_union) => {
                let (line_start, source) = src.surrounding_lines(span.start, span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                Snippet {
                    title: Some(Annotation {
                        label: Some(format!("empty {}", table_or_union)),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![Annotation {
                        label: Some(
                            "try using an empty struct to define a placeholder variant".to_string(),
                        ),
                        id: None,
                        annotation_type: AnnotationType::Help,
                    }],
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: format!("{} must have at least one member", table_or_union),
                            annotation_type: AnnotationType::Error,
                            range: (span.start - source_start, span.end - source_start),
                        }],
                    }],
                }
            }
            OobOrdinals {
                decl_span,
                ordinal_spans,
            } => {
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
