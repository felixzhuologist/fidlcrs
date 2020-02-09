use crate::lexer::Span;
use crate::raw::{FidlType, Spanned};
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use std::cmp;

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
            DuplicateDefinition {
                original,
                duplicate,
                decl_type,
                decl_name,
            } => {
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

                // If the two declarations are in the same file, we output a single slice
                // of the file with two annotations. If they're in different files, we
                // need to output to separate slices of the two different files each with
                // an annotation. There should probably be a way to simplify this logic
                let slices = if original.file == duplicate.file {
                    let src = srcs.get_file(original.file);
                    let src_start = cmp::min(original.start, duplicate.start);
                    let src_end = cmp::min(original.end, duplicate.end);
                    let (line_start, source) = src.surrounding_lines(src_start, src_end);
                    let source_start = src.lines.offset_at_line_number(line_start);
                    vec![Slice {
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
                                range: (original.start - source_start, original.end - source_start),
                            },
                            SourceAnnotation {
                                label: "duplicate definition".to_string(),
                                annotation_type: AnnotationType::Error,
                                range: (
                                    duplicate.start - source_start,
                                    duplicate.end - source_start,
                                ),
                            },
                        ],
                    }]
                } else {
                    let orig_slice = {
                        let src = srcs.get_file(original.file);
                        let (line_start, source) =
                            src.surrounding_lines(original.start, original.end);
                        let source_start = src.lines.offset_at_line_number(line_start);
                        Slice {
                            source,
                            line_start,
                            origin: Some(src.path.clone()),
                            fold: false,
                            annotations: vec![SourceAnnotation {
                                label: format!(
                                    "{} {} originally defined here",
                                    decl_type, decl_name
                                ),
                                annotation_type: AnnotationType::Info,
                                range: (original.start - source_start, original.end - source_start),
                            }],
                        }
                    };
                    let dupe_slice = {
                        let src = srcs.get_file(duplicate.file);
                        let (line_start, source) =
                            src.surrounding_lines(duplicate.start, duplicate.end);
                        let source_start = src.lines.offset_at_line_number(line_start);
                        Slice {
                            source,
                            line_start,
                            origin: Some(src.path.clone()),
                            fold: false,
                            annotations: vec![SourceAnnotation {
                                label: "duplicate definition".to_string(),
                                annotation_type: AnnotationType::Info,
                                range: (
                                    duplicate.start - source_start,
                                    duplicate.end - source_start,
                                ),
                            }],
                        }
                    };
                    vec![dupe_slice, orig_slice]
                };

                Snippet {
                    title: Some(Annotation {
                        label: Some("duplicate definition".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices,
                }
            }
            InvalidAttributePlacement { name, placement } => {
                let span = name.span;
                let src = srcs.get_file(span.file);
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
                let src = srcs.get_file(span.file);
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
                let src = srcs.get_file(span.file);
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
            EmptyTableOrUnion(span) => {
                let src = srcs.get_file(span.file);
                let (line_start, source) = src.surrounding_lines(span.start, span.end);
                let source_start = src.lines.offset_at_line_number(line_start);

                Snippet {
                    title: Some(Annotation {
                        label: Some("empty table or union".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![Annotation {
                        label: Some(
                            "try using an empty struct to define a placeholder member".to_string(),
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
                            label: "tables and unions must have at least one non reserved member"
                                .to_string(),
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
