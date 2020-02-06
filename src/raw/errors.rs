use super::attributes::Placement;
use crate::raw::{Attribute, Spanned};
use crate::source_file::SourceFile;
use crate::span::Span;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use std::cmp;
use std::fmt;

pub enum Error {
    DuplicateAttributes {
        original: Spanned<Attribute>,
        duplicate: Spanned<Attribute>,
    },
    InvalidAttributePlacement {
        name: Spanned<String>,
        placement: Placement,
    },
    InvalidAttributeValue {
        name: Spanned<String>,
        value: Spanned<String>,
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
            DuplicateAttributes {
                original,
                duplicate,
            } => {
                let orig_span = original.span;
                let dupe_span = duplicate.span;
                let src_start = cmp::min(orig_span.start, dupe_span.start);
                let src_end = cmp::min(orig_span.end, dupe_span.end);
                let (line_start, source) = src.surrounding_lines(src_start, src_end);
                let source_start = src.lines.offset_at_line_number(line_start);

                let mut footer = Vec::new();
                if original.value.name.value == "Doc" || duplicate.value.name.value == "Doc" {
                    footer.push(Annotation {
                        label: Some(
                            "doc comments are added as attributes with a key of \"Doc\""
                                .to_string(),
                        ),
                        id: None,
                        annotation_type: AnnotationType::Help,
                    });
                }

                Snippet {
                    title: Some(Annotation {
                        label: Some("duplicate attributes".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: footer,
                    slices: vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![
                            SourceAnnotation {
                                label: format!(
                                    "attribute {} originally defined here",
                                    original.value.name.value
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
        }
    }
}
