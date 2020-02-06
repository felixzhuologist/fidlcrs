use crate::grammar;
use crate::lexer;
use crate::raw;
use crate::source_file::{FileId, FileMap, SourceFile};
use crate::span::Span;
use crate::token::Token;
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};
use lalrpop_util;
use std::fmt;

pub fn parse(src: &SourceFile) -> Result<raw::File, Error> {
    let result = grammar::FileParser::new().parse(lexer::Lexer::new(src.contents()));
    result.map_err(|err| Error::from_parse_error(src.id, err))
}

// TOD: separate out all the erorr code
// TODO: follow rustc's parse error format:
// error: expected one of `::`, `;`, or `as`, found keyword `use`
//  --> src/raw/attributes.rs:2:1
//   |
// 1 | use super::errors::Error
//   |                         - expected one of `::`, `;`, or `as`
// 2 | use crate::raw::Attribute;
//   | ^^^ unexpected token

// error: aborting due to previous error

// error: could not compile `fidlc-rs`.

// use crate::lexer::Token

// this needs to be kept in sync with the grammar and lexer
type ParseError<'input> = lalrpop_util::ParseError<usize, Token<'input>, lexer::SpannedError>;

pub struct Error {
    src: FileId,
    error_type: ErrorType,
}

/// This is a wrapper around our instance of lalrpop_util::ParseError that removes
/// all of the instances of Tokens. This avoids having to pipe through the
/// Token lifetimes since they are not necessary for creating error messages.
#[derive(Debug)]
pub enum ErrorType {
    InvalidToken(usize),
    UnrecognizedEOF {
        location: usize,
        expected: Vec<String>,
    },
    UnrecognizedToken {
        start: usize,
        end: usize,
        expected: Vec<String>,
    },
    ExtraToken {
        start: usize,
        end: usize,
    },
    LexError(lexer::SpannedError),
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        match self {
            InvalidToken(_) => write!(f, "invalid token"),
            UnrecognizedEOF {
                location: _,
                expected,
            } =>
            // TODO
            {
                write!(f, "unrecognized EOF, expected one of: {:?}", expected)
            }
            UnrecognizedToken {
                start: _,
                end: _,
                expected,
            } =>
            // TODO
            {
                write!(f, "unrecognized token, expected one of : {:?}", expected)
            }
            ExtraToken { start: _, end: _ } => write!(f, "extra token"),
            LexError(err) => write!(f, "{}", err.value),
        }
    }
}

impl From<ParseError<'_>> for ErrorType {
    fn from(err: ParseError) -> Self {
        match err {
            ParseError::InvalidToken { location } => ErrorType::InvalidToken(location),
            ParseError::UnrecognizedEOF { location, expected } => {
                ErrorType::UnrecognizedEOF { location, expected }
            }
            ParseError::UnrecognizedToken {
                token: (start, _, end),
                expected,
            } => ErrorType::UnrecognizedToken {
                start,
                end,
                expected,
            },
            ParseError::ExtraToken {
                token: (start, _, end),
            } => ErrorType::ExtraToken { start, end },
            ParseError::User { error } => ErrorType::LexError(error),
        }
    }
}

impl ErrorType {
    pub fn get_span(&self) -> Span<usize> {
        use ErrorType::*;
        // TODO: there's probably a way to avoid the explicit dereferencing
        match self {
            InvalidToken(l)
            | UnrecognizedEOF {
                location: l,
                expected: _,
            } => Span { start: *l, end: *l },
            UnrecognizedToken {
                start,
                end,
                expected: _,
            }
            | ExtraToken { start, end } => Span {
                start: *start,
                end: *end,
            },
            LexError(err) =>
            // TODO: is it really useful to have Location during lexing?
            {
                Span {
                    start: err.span.start.absolute,
                    end: err.span.end.absolute,
                }
            }
        }
    }
}

// TODO: clean up this interface once we determine what other args into_snippet
// would need. should this be a trait?
impl Error {
    pub fn into_snippet(self, files: &FileMap) -> Snippet {
        let src = files.get_file(self.src);
        let span = self.error_type.get_span();
        let error_msg = format!("{}", self.error_type);
        let (line_start, source) = src.surrounding_lines(span.start, span.end);
        let source_start = src.lines.offset_at_line_number(line_start);

        Snippet {
            title: Some(Annotation {
                label: Some("parse error:".to_string()),
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
                    label: error_msg,
                    annotation_type: AnnotationType::Error,
                    range: (span.start - source_start, span.end - source_start),
                }],
            }],
        }
    }

    pub fn from_parse_error(src: FileId, err: ParseError) -> Error {
        Error {
            src,
            error_type: err.into(),
        }
    }
}
