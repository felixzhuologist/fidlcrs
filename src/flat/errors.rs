use crate::lexer::Span;

pub enum Error {
    DuplicateImport {
        import: String,
        orig: Span,
        dupe: Span,
    },
    ImportNameConflict {
        name: String,
        orig: Span,
        dupe: Span,
    },
}
