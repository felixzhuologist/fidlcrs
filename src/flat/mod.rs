pub mod resolve;
mod tree;
pub mod validate;

pub use tree::*;

use crate::errors::ErrorCx;
use crate::flatten::ResolverContext;
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Snippet};


pub fn resolve_library(
    srcs: &FileMap,
    ctx: ResolverContext,
    deps: &Libraries,
) -> Result<Library, Vec<Snippet>> {
    Library::from_files(ctx, deps)
        .map_err(|errs| errs.into_iter().map(|err| err.into_snippet(srcs)).collect())
}

pub fn add_library(errors: &mut ErrorCx, libs: &mut Libraries, lib: Library) {
    if let Err(name) = libs.add_library(lib) {
        errors.push(Snippet {
            title: Some(Annotation {
                label: Some(format!("multiple libraries with name {}", name)),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: vec![],
        });
    }
}
