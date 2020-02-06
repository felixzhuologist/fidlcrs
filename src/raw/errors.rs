use super::attributes::Placement;
use crate::raw::{Attribute, Spanned};
use crate::source_file::SourceFile;
use annotate_snippets::snippet::Snippet;

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

impl Error {
    pub fn into_snippet(self, _src: &SourceFile) -> Snippet {
        unimplemented!()
    }
}
