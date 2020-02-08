//! This module's role is to accumulate multiple Files and construct whatever
//! state is needed for the next phase of compilation (resolution, see flat::).
//! Unlike flat:: it operates on unresolved Files, and unlike raw:: it
//! processes multiple Files and accumulates internal state.

use crate::lexer::Span;
use crate::raw::{Attribute, File, Spanned};
use crate::source_file::{FileId, FileMap};
use annotate_snippets::snippet::Snippet;
use std::collections::HashMap;

#[derive(Default)]
pub struct Flattener {
    // each file's attributes are moved out and accumulated here
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
    pub fn add_file(&mut self, file: File, id: FileId) {
        unimplemented!()
    }

    pub fn finish(self) -> (ResolverContext, Vec<Error>) {
        unimplemented!()
    }
}

pub enum Error {}

impl Error {
    pub fn into_snippet(self, srcs: &FileMap) -> Snippet {
        unimplemented!()
    }
}
