use super::*;
use crate::errors::{span_to_snippet, spans_to_snippet, two_spans_to_snippet, ErrText};
use crate::flat::Sort;
use crate::lexer::Span;
use crate::source_file::FileMap;
use annotate_snippets::snippet::{Annotation, AnnotationType, Snippet};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Error {
    SortError {
        /// Span of the occurence of this error
        span: Span,
        // TODO: we may want to include definitions of these variables as well
        expected: Sort,
        actual: Sort,
    },

    /// A param was passed to a Type that does not accept it
    InvalidTypeParam {
        func_call: Span,
        param: ParamType,
        // this Span is optional since it only makes sense to provide it if `func`
        // is a user defined type. If it's a builtin (or, eventually, if it's an
        // inline/anonymous type), the func call span will include the definition
        // TODO: this span doesn't work for e.g. structs
        func_def: Option<Span>,
    },
    NonConcreteType {
        span: Span,
        missing: Vec<ParamType>,
    },

    VarCycle(Vec<(Name, Span)>),
    InfiniteType(Vec<(Name, Span)>),

    TypeError {
        actual: Spanned<Type>,
        expected: Type,
    },
    IntCoercionError {
        span: Span,
        ty: Type,
    },
    StringBoundsError {
        /// The span is on the string literal
        length: Spanned<usize>,
        /// The span is on the bounds
        bounds: Spanned<u64>,
    },

    InvalidBitsType(Spanned<Type>),
    InvalidEnumType(Spanned<Type>),
    DuplicateMemberValue {
        original: Span,
        dupe: Span,
        /// either enum or bits
        decl_kind: &'static str,
    },
    NullableMember {
        span: Span,
        // either union or table
        decl_kind: &'static str,
    },

    // nullability
    TypeCantBeNullable(Spanned<Type>),
    DoubleNullability(Span), // TODO: include definition as help
}

#[derive(Debug, Copy, Clone)]
pub enum ParamType {
    Constraint,
    Layout,
}

impl Error {
    pub fn into_snippet(self, srcs: &FileMap) -> Snippet {
        use Error::*;
        match self {
            SortError {
                span,
                expected,
                actual,
            } => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "sort error".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("expected a {}, but got a {} instead", expected, actual),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            InvalidTypeParam {
                func_call,
                param,
                func_def,
            } => {
                let title = ErrText {
                    text: "invalid type parameter".to_string(),
                    ty: AnnotationType::Error,
                };
                let call_annotation = ErrText {
                    text: format!(
                        "type was passed a {} parameter, which it does not support",
                        param
                    ),
                    ty: AnnotationType::Error,
                };
                match func_def {
                    None => span_to_snippet(func_call, srcs, title, call_annotation, None),
                    Some(defn) => two_spans_to_snippet(
                        func_call,
                        defn,
                        srcs,
                        title,
                        call_annotation,
                        ErrText {
                            text: "which is defined here".to_string(),
                            ty: AnnotationType::Help,
                        },
                        None,
                    ),
                }
            }
            NonConcreteType { span, missing } => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "type is not fully resolved".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "expected a fully resolved type here".to_string(),
                    ty: AnnotationType::Error,
                },
                Some(Annotation {
                    // TODO: print this nicer
                    label: Some(format!("type is missing: {:?}", missing)),
                    id: None,
                    annotation_type: AnnotationType::Help,
                }),
            ),
            VarCycle(cycle) => {
                // note: the top level error name will be the last name, since
                // the first element in the vector is the rhs of the first type
                // that caused this error
                let name = cycle[cycle.len() - 1].0.clone();
                let spans: Vec<_> = cycle
                    .into_iter()
                    .map(|(name, span)| {
                        (
                            span,
                            ErrText {
                                text: format!("...which requires processing `{}`...", name.name),
                                ty: AnnotationType::Info,
                            },
                        )
                    })
                    .collect();
                spans_to_snippet(
                    ErrText {
                        text: format!("cycle detected when processing {}", name.name),
                        ty: AnnotationType::Error,
                    },
                    spans,
                    srcs,
                    None,
                )
            }
            InfiniteType(cycle) => {
                let name = cycle[cycle.len() - 1].0.clone();
                let spans: Vec<_> = cycle
                    .into_iter()
                    .map(|(name, span)| {
                        (
                            span,
                            ErrText {
                                text: format!("contains infinitely sized `{}`...", name.name),
                                ty: AnnotationType::Info,
                            },
                        )
                    })
                    .collect();
                spans_to_snippet(
                    ErrText {
                        text: format!("{} is an infinitely sized type", name.name),
                        ty: AnnotationType::Error,
                    },
                    spans,
                    srcs,
                    Some(Annotation {
                        label: Some("types that can _only_ be infinitely sized can not be represented on the wire".to_string()),
                        id: None,
                        annotation_type: AnnotationType::Info,
                    })
                )
            }
            TypeError { actual, expected } => span_to_snippet(
                actual.span,
                srcs,
                ErrText {
                    text: "type error".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("value has type {}, but expected {}", actual.value, expected),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            IntCoercionError { span, ty } => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "int coercion error".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("cannot be casted to {} - value is out of bounds", ty),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            StringBoundsError { length, bounds } => two_spans_to_snippet(
                length.span,
                bounds.span,
                srcs,
                ErrText {
                    text: "string bounds error".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!(
                        "string has a length of {}, but the type bounds are specified as {}",
                        length.value, bounds.value
                    ),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "bounds were specified here".to_string(),
                    ty: AnnotationType::Help,
                },
                None,
            ),
            InvalidBitsType(ty) => span_to_snippet(
                ty.span,
                srcs,
                ErrText {
                    text: "invalid bits type".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!(
                        "bits type specified as {}, but must be an unsigned integral type",
                        ty.value
                    ),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            InvalidEnumType(ty) => span_to_snippet(
                ty.span,
                srcs,
                ErrText {
                    text: "invalid enum type".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!(
                        "enum type specified as {}, but must be an integral type",
                        ty.value
                    ),
                    ty: AnnotationType::Error,
                },
                None,
            ),
            DuplicateMemberValue {
                original,
                dupe,
                decl_kind,
            } => two_spans_to_snippet(
                original,
                dupe,
                srcs,
                ErrText {
                    text: format!("{} has duplicate member values", decl_kind),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "value conflicts with existing member".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "original member defined here".to_string(),
                    ty: AnnotationType::Info,
                },
                None,
            ),
            NullableMember { span, decl_kind } => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: format!("nullable {} member", decl_kind),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("{} member cannot be nullable", decl_kind),
                    ty: AnnotationType::Error,
                },
                Some(Annotation {
                    label: Some("try moving the nullable member into a struct".to_string()),
                    id: None,
                    annotation_type: AnnotationType::Help,
                }),
            ),
            TypeCantBeNullable(ty) => span_to_snippet(
                ty.span,
                srcs,
                ErrText {
                    text: "invalid nullable type".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: format!("type {} cannot be used as nullable", ty.value),
                    ty: AnnotationType::Error,
                },
                None, // "try wrapping it in a struct"?
            ),
            DoubleNullability(span) => span_to_snippet(
                span,
                srcs,
                ErrText {
                    text: "double nullability".to_string(),
                    ty: AnnotationType::Error,
                },
                ErrText {
                    text: "type is indicated as nullable twice".to_string(),
                    ty: AnnotationType::Error,
                },
                None,
            ),
        }
    }
}

// TODO: move this along their definitions?
impl fmt::Display for ParamType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParamType::Constraint => write!(f, "constraint"),
            ParamType::Layout => write!(f, "layout"),
        }
    }
}

impl fmt::Display for Sort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sort::Term => write!(f, "const"),
            Sort::Type => write!(f, "type"),
            Sort::Protocol => write!(f, "protocol"),
            Sort::Service => write!(f, "service"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Struct(_) => write!(f, "struct"),
            Type::Bits(_) => write!(f, "bits"),
            Type::Enum(_) => write!(f, "enum"),
            Type::Table(_) => write!(f, "table"),
            Type::Union(_) => write!(f, "union"),
            Type::Ptr(ty) => write!(f, "nullable {}", ty.value),
            Type::Array(_) => write!(f, "array"),
            Type::Vector(_) => write!(f, "vector"),
            Type::Str(_) => write!(f, "string"),
            Type::Handle(_) | Type::ClientEnd(_) | Type::ServerEnd(_) => write!(f, "handle"),
            Type::Primitive(sub) => write!(f, "{}", sub),
            Type::Any => write!(f, "any"),
            Type::Int => write!(f, "int"),
            // these shouldn't be called: should pass in the evaled type to the error
            Type::TypeSubstitution(_) | Type::Identifier(_) => write!(f, "todo"),
        }
    }
}

impl fmt::Display for PrimitiveSubtype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(self).unwrap())
    }
}
