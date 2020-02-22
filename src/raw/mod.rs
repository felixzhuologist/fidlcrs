pub mod attributes;
pub mod errors;
mod tree;
pub mod validate;

pub use tree::*;

use crate::errors::ErrorCx;
use crate::source_file::FileMap;

pub fn validate_files(srcs: &FileMap, errors: &mut ErrorCx, files: &Vec<File>) {
    for file in files {
        for error in validate::validate_file(&file) {
            errors.push(error.into_snippet(srcs));
        }
    }
}
