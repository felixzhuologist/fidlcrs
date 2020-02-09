//! This module's role is to accumulate multiple Files and construct whatever
//! state is needed for the next phase of compilation (resolution, see flat::).
//! Unlike flat:: it operates on unresolved Files, and unlike raw:: it
//! processes multiple Files and accumulates internal state.

use crate::lexer::Span;
use crate::raw;
use crate::raw::{Attribute, FidlType, File, LibraryName, Spanned};
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use std::cmp;
use std::collections::HashMap;

#[derive(Default)]
pub struct Flattener {
    // each file's attributes are moved out and accumulated here
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Option<(String, Span)>,
    // TODO: create a type that wraps a (Span, FileId) pair. Or, maybe Span should
    // be refactored to contain a FileId.
    pub defined_names: HashMap<String, Span>,
    pub errors: Vec<Error>,
    pub files: Vec<File>,
}

pub struct ResolverContext {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: String,
    pub defined_names: HashMap<String, Span>,
    pub files: Vec<File>,
}

impl Flattener {
    pub fn add_file(&mut self, mut file: File) {
        self.attributes.extend(file.attributes);
        file.attributes = Vec::new();

        match &self.name {
            Some((existing_name, span)) => {
                let name = library_name_as_string(&file.name);
                if existing_name != &name {
                    self.errors.push(Error::LibraryNameInconsistent {
                        existing_name: existing_name.clone(),
                        existing_span: *span,
                        conflicting_name: name,
                        conflicting_span: get_library_name_span(&file.name),
                    });
                }
            }
            None => {
                let span = get_library_name_span(&file.name);
                let name = library_name_as_string(&file.name);
                self.name = Some((name, span))
            }
        };

        for decl in &file.decls {
            if let Some(dupe) = self.defined_names.insert(decl.value.name(), decl.span) {
                self.errors.push(Error::DupeDecl {
                    name: decl.value.name(),
                    existing_span: dupe,
                    conflicting_span: decl.span,
                })
            }
        }

        self.files.push(file);
    }

    pub fn finish(mut self) -> (ResolverContext, Vec<Error>) {
        let errors = raw::validate::validate_attributes(&self.attributes, FidlType::Library);
        self.errors.extend(
            errors
                .into_iter()
                .map(|e| Error::ValidationError(e))
                .collect::<Vec<_>>(),
        );

        let result = ResolverContext {
            attributes: self.attributes,
            name: self.name.unwrap().0,
            defined_names: self.defined_names,
            files: self.files,
        };
        (result, self.errors)
    }
}

fn library_name_as_string(name: &LibraryName) -> String {
    name.iter()
        .map(|s| s.value.clone())
        .collect::<Vec<_>>()
        .join(".")
}

// TODO: this code is repeated in the grammar
// TODO: these two functions can be merged into a single one that takes in a
// Vec<Spanned<T>> and returns as Spanned<Vec<T>>
fn get_library_name_span(name: &LibraryName) -> Span {
    let start = name.first().unwrap().span.start;
    let end = name.last().unwrap().span.start;
    let file = name.first().unwrap().span.file;
    Span { file, start, end }
}

pub enum Error {
    LibraryNameInconsistent {
        existing_name: String,
        existing_span: Span,
        conflicting_name: String,
        conflicting_span: Span,
    },
    DupeDecl {
        name: String,
        existing_span: Span,
        conflicting_span: Span,
    },
    // Really the only errors that can occur here are the attribute related
    // ones.
    ValidationError(raw::errors::Error),
}

impl Error {
    pub fn into_snippet(self, srcs: &FileMap) -> Snippet {
        use Error::*;
        match self {
            LibraryNameInconsistent {
                existing_name,
                existing_span,
                conflicting_name,
                conflicting_span,
            } => {
                let existing_slice = {
                    let span = existing_span;
                    let src = srcs.get_file(span.file);
                    let (line_start, source) = src.surrounding_lines(span.start, span.end);
                    let source_start = src.lines.offset_at_line_number(line_start);
                    Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: "conflicts with name specified here".to_string(),
                            annotation_type: AnnotationType::Info,
                            range: (span.start - source_start, span.end - source_start),
                        }],
                    }
                };
                let conflicting_slice = {
                    let span = conflicting_span;
                    let src = srcs.get_file(span.file);
                    let (line_start, source) = src.surrounding_lines(span.start, span.end);
                    let source_start = src.lines.offset_at_line_number(line_start);
                    Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![SourceAnnotation {
                            label: "library name specified here".to_string(),
                            annotation_type: AnnotationType::Info,
                            range: (span.start - source_start, span.end - source_start),
                        }],
                    }
                };
                Snippet {
                    title: Some(Annotation {
                        label: Some("files passed to --files disagree on library name".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![Annotation {
                        label: Some(format!(
                            "library names are different: {} vs {}",
                            existing_name, conflicting_name
                        )),
                        id: None,
                        annotation_type: AnnotationType::Info,
                    }],
                    slices: vec![conflicting_slice, existing_slice],
                }
            }
            DupeDecl {
                name,
                existing_span,
                conflicting_span,
            } => {
                let slices = if existing_span.file == conflicting_span.file {
                    let src = srcs.get_file(existing_span.file);
                    let src_start = cmp::min(existing_span.start, conflicting_span.start);
                    let src_end = cmp::min(existing_span.end, conflicting_span.end);
                    let (line_start, source) = src.surrounding_lines(src_start, src_end);
                    let source_start = src.lines.offset_at_line_number(line_start);
                    vec![Slice {
                        source,
                        line_start,
                        origin: Some(src.path.clone()),
                        fold: false,
                        annotations: vec![
                            SourceAnnotation {
                                label: "previously defined here".to_string(),
                                annotation_type: AnnotationType::Info,
                                range: (
                                    existing_span.start - source_start,
                                    existing_span.end - source_start,
                                ),
                            },
                            SourceAnnotation {
                                label: "definition already exists".to_string(),
                                annotation_type: AnnotationType::Error,
                                range: (
                                    conflicting_span.start - source_start,
                                    conflicting_span.end - source_start,
                                ),
                            },
                        ],
                    }]
                } else {
                    let orig_slice = {
                        let src = srcs.get_file(existing_span.file);
                        let (line_start, source) =
                            src.surrounding_lines(existing_span.start, existing_span.end);
                        let source_start = src.lines.offset_at_line_number(line_start);
                        Slice {
                            source,
                            line_start,
                            origin: Some(src.path.clone()),
                            fold: false,
                            annotations: vec![SourceAnnotation {
                                label: "previously defined here".to_string(),
                                annotation_type: AnnotationType::Info,
                                range: (
                                    existing_span.start - source_start,
                                    existing_span.end - source_start,
                                ),
                            }],
                        }
                    };
                    let dupe_slice = {
                        let src = srcs.get_file(conflicting_span.file);
                        let (line_start, source) =
                            src.surrounding_lines(conflicting_span.start, conflicting_span.end);
                        let source_start = src.lines.offset_at_line_number(line_start);
                        Slice {
                            source,
                            line_start,
                            origin: Some(src.path.clone()),
                            fold: false,
                            annotations: vec![SourceAnnotation {
                                label: "definition already exists".to_string(),
                                annotation_type: AnnotationType::Info,
                                range: (
                                    conflicting_span.start - source_start,
                                    conflicting_span.end - source_start,
                                ),
                            }],
                        }
                    };
                    vec![orig_slice, dupe_slice]
                };
                Snippet {
                    title: Some(Annotation {
                        label: Some(format!("{} is defined twice", name)),
                        id: None,
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices,
                }
            }
            ValidationError(err) => err.into_snippet(srcs),
        }
    }
}
