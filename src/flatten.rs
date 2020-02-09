//! This module's role is to accumulate multiple Files and construct whatever
//! state is needed for the next phase of compilation (resolution, see flat::).
//! Unlike flat:: it operates on unresolved Files, and unlike raw:: it
//! processes multiple Files and accumulates internal state.

use crate::errors::{two_spans_to_snippet, ErrText};
use crate::lexer::Span;
use crate::raw;
use crate::raw::{Attribute, FidlType, File, LibraryName, Spanned};
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Snippet};
use std::collections::HashMap;

#[derive(Default)]
pub struct Flattener {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Option<(String, Span)>,
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
        // each file's attributes are moved out and merged together
        self.attributes.extend(file.attributes);
        file.attributes = Vec::new();

        self.check_name(&file.name);
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

    fn check_name(&mut self, lib_name: &LibraryName) {
        match &self.name {
            Some((existing_name, span)) => {
                let name = library_name_as_string(lib_name);
                if existing_name != &name {
                    self.errors.push(Error::LibraryNameInconsistent {
                        existing_name: existing_name.clone(),
                        existing_span: *span,
                        conflicting_name: name,
                        conflicting_span: get_library_name_span(lib_name),
                    });
                }
            }
            None => {
                let span = get_library_name_span(lib_name);
                let name = library_name_as_string(lib_name);
                self.name = Some((name, span))
            }
        };
    }
}

// TODO: this code is repeated in the grammar
// TODO: these two functions can be merged into a single one that takes in a
// Vec<Spanned<T>> and returns as Spanned<Vec<T>>
fn library_name_as_string(name: &LibraryName) -> String {
    name.iter()
        .map(|s| s.value.clone())
        .collect::<Vec<_>>()
        .join(".")
}

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
            } => two_spans_to_snippet(
                conflicting_span,
                existing_span,
                srcs,
                ErrText {
                    text: "files passed to --files disagree on library name".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "library name specified here".to_string(),
                    ty: AnnotationType::Info,
                },
                ErrText {
                    text: "conflicts with name specified here".to_string(),
                    ty: AnnotationType::Info,
                },
                Some(Annotation {
                    label: Some(format!(
                        "library names are different: {} vs {}",
                        existing_name, conflicting_name
                    )),
                    id: None,
                    annotation_type: AnnotationType::Info,
                }),
            ),
            DupeDecl {
                name,
                existing_span,
                conflicting_span,
            } => two_spans_to_snippet(
                conflicting_span,
                existing_span,
                srcs,
                ErrText {
                    text: format!("{} is defined twice", name),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "previously defined here".to_string(),
                    ty: AnnotationType::Info,
                },
                ErrText {
                    text: "definition already exists".to_string(),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            ValidationError(err) => err.into_snippet(srcs),
        }
    }
}
