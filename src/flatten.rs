//! This module's role is to accumulate multiple Files and construct whatever
//! state is needed for the next phase of compilation (resolution, see flat::).
//! Unlike flat:: it operates on unresolved Files, and unlike raw:: it
//! processes multiple Files and accumulates internal state.

use crate::errors::{two_spans_to_snippet, ErrText, ErrorCx};
use crate::lexer::Span;
use crate::raw;
use crate::raw::{Attributes, FidlType, File, LibraryName};
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Snippet};
use std::collections::HashMap;

pub fn flatten_files(srcs: &FileMap, snippets: &mut ErrorCx, files: Vec<File>) -> ResolverContext {
    let mut flattener = Flattener::default();
    for file in files {
        flattener.add_file(file);
    }

    let (lib_ctx, errors) = flattener.finish();
    for error in errors {
        snippets.push(error.into_snippet(srcs));
    }
    lib_ctx
}

pub type UnresolvedScope = HashMap<String, NameDef>;

pub struct NameDef {
    span: Span,
    inner_scope: Option<HashMap<String, Span>>,
}

/// Analogous to Libraries.lookup(), but for the library currently being resolved
pub fn lookup(scope: &UnresolvedScope, name: &String, member: &Option<String>) -> Option<Span> {
    match scope.get(name) {
        Some(NameDef { span, inner_scope }) => match member {
            Some(member) => inner_scope
                .as_ref()
                .map_or(None, |scope| scope.get(member).map(|m| *m)),
            None => Some(*span),
        },
        _ => None,
    }
}

#[derive(Default)]
pub struct Flattener {
    pub attributes: Attributes,
    pub name: Option<(String, Span)>,
    pub defined_names: HashMap<String, NameDef>,
    pub errors: Vec<Error>,
    pub files: Vec<File>,
}

// TODO: is this necessary? it's just Flattener without errors for now.
pub struct ResolverContext {
    pub attributes: Attributes,
    pub name: String,
    pub defined_names: HashMap<String, NameDef>,
    pub files: Vec<File>,
}

impl Flattener {
    pub fn add_file(&mut self, mut file: File) {
        // each file's attributes are moved out and merged together
        self.attributes.extend(file.attributes);
        file.attributes = Vec::new();

        self.check_name(&file.name);
        for decl in &file.decls {
            let inner_scope = match &decl.value {
                // we may want to refactor this logic into an is_block() of some sort. but right
                // now it's simple enough just ot have inline
                raw::Decl::Bits(ref val) => {
                    let mut member_scope = HashMap::new();
                    for member in &val.members {
                        member_scope.insert(member.value.name.value.clone(), member.span);
                    }
                    Some(member_scope)
                }
                raw::Decl::Enum(ref val) => {
                    let mut member_scope = HashMap::new();
                    for member in &val.members {
                        member_scope.insert(member.value.name.value.clone(), member.span);
                    }
                    Some(member_scope)
                }
                _ => None,
            };
            let entry = NameDef {
                span: decl.span,
                inner_scope,
            };
            if let Some(dupe) = self.defined_names.insert(decl.value.name(), entry) {
                self.errors.push(Error::DupeDecl {
                    name: decl.value.name(),
                    existing_span: dupe.span,
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
