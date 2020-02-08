//! This module's role is to accumulate multiple Files and construct whatever
//! state is needed for the next phase of compilation (resolution, see flat::).
//! Unlike flat:: it operates on unresolved Files, and unlike raw:: it
//! processes multiple Files and accumulates internal state.

use crate::lexer::Span;
use crate::raw;
use crate::raw::{Attribute, FidlType, File, LibraryName, Spanned};
use crate::source_file::FileMap;
use annotate_snippets::snippet::Snippet;
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

    pub fn finish(self) -> (ResolverContext, Vec<Error>) {
        let _errors = raw::validate::validate_attributes(&self.attributes, FidlType::Library);
        // TODO: we need to refactor Span to know about file Ids before being able to reuse the
        // code from raw:: here.

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
}

impl Error {
    pub fn into_snippet(self, _srcs: &FileMap) -> Snippet {
        unimplemented!()
    }
}
