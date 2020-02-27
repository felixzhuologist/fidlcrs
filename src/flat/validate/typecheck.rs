use super::errors::Error;
use super::eval::eval_term;
use super::Validator;
use crate::flat::PrimitiveSubtype::*;
use crate::flat::*;
use crate::lexer::Span;
use crate::raw::{IntLiteral, Spanned};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryFrom;

pub fn type_check(
    term: Spanned<&Term>,
    scope: &Libraries,
    cache: &mut HashMap<Name, Type>,
) -> Result<Type, Error> {
    let seen = Vec::new();
    _type_check(term, scope, cache, seen)
}

pub fn _type_check(
    term: Spanned<&Term>,
    scope: &Libraries,
    cache: &mut HashMap<Name, Type>,
    mut seen: Vec<(Name, Span)>,
) -> Result<Type, Error> {
    match &term.value {
        Term::Identifier(name) => {
            if let Some(ty) = cache.get(name) {
                return Ok(ty.clone());
            }

            if seen.iter().any(|(n, _)| n == name) {
                return Err(Error::VarCycle(seen).into());
            }
            // NOTE: once Name is refactored just contain IDs, it can implement Copy and we won't
            // need to clone explicitly here
            seen.push((name.clone(), term.span));
            let ty = _type_check(
                scope.get_term(term.span.wrap(name))?.into(),
                scope,
                cache,
                seen,
            );
            cache.insert(
                name.clone(),
                match ty.clone() {
                    Ok(t) => t,
                    Err(_) => Type::Any,
                },
            );
            ty
        }
        Term::Str(_) => Ok(Type::Str(Str { bounds: None })),
        // TODO: think about this some more
        Term::Int(_) => Ok(Type::Int),
        Term::Float(_) => Ok(Type::Primitive(PrimitiveSubtype::Float64)),
        Term::True | Term::False => Ok(Type::Primitive(PrimitiveSubtype::Bool)),
    }
}

pub fn term_can_have_type(
    term: Spanned<&Term>,
    expected_ty: &Type,
    actual_ty: &Type,
    validator: &mut Validator,
) -> Result<(), Error> {
    match (&term.value, expected_ty) {
        (Term::Identifier(_), _) => panic!("only pass evaled terms"),
        (_, Type::Identifier(_)) => panic!("only pass evaled types"),
        (_, Type::Int) => panic!("users can't specify untyped Ints"),
        (_, Type::Any) => Ok(()),
        (Term::Str(string), Type::Str(Str { bounds })) => match bounds {
            None => Ok(()),
            Some(bounds) => {
                let bounds_ty = validator.type_check(bounds.into());
                if let Type::Any = bounds_ty {
                    return Ok(());
                }
                let bounds = eval_term(bounds.into(), validator.scope)?;
                term_can_have_type(bounds, &Type::Primitive(UInt64), &bounds_ty, validator)?;

                match bounds.value {
                    Term::Int(bounds_literal) => {
                        if bounds_literal.value < string.len() as u64 {
                            Err(Error::StringBoundsError {
                                length: term.span.wrap(string.len()),
                                bounds: bounds.span.wrap(bounds_literal.value),
                            })
                        } else {
                            Ok(())
                        }
                    }
                    _ => panic!("only Ints should have been let through here"),
                }
            }
        },
        (Term::Int(val), Type::Primitive(Int8)) => {
            i8::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(Int8),
                })
        }
        (Term::Int(val), Type::Primitive(Int16)) => {
            i16::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(Int16),
                })
        }
        (Term::Int(val), Type::Primitive(Int32)) => {
            i32::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(Int32),
                })
        }
        (Term::Int(val), Type::Primitive(Int64)) => {
            i64::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(Int64),
                })
        }
        (Term::Int(val), Type::Primitive(UInt8)) => {
            u8::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(UInt8),
                })
        }
        (Term::Int(val), Type::Primitive(UInt16)) => {
            u16::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(UInt16),
                })
        }
        (Term::Int(val), Type::Primitive(UInt32)) => {
            u32::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(UInt32),
                })
        }
        (Term::Int(val), Type::Primitive(UInt64)) => {
            u64::try_from(*val)
                .map(|_| ())
                .map_err(|_| Error::IntCoercionError {
                    span: term.span,
                    ty: Type::Primitive(UInt64),
                })
        }
        (Term::Float(_), Type::Primitive(Float32))
        | (Term::Float(_), Type::Primitive(Float64))
        | (Term::True, Type::Primitive(Bool))
        | (Term::False, Type::Primitive(Bool)) => Ok(()),
        _ => Err(Error::TypeError {
            actual: term.span.wrap(actual_ty.clone()),
            expected: expected_ty.clone(),
        }),
    }
}

impl TryFrom<IntLiteral> for i64 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        if value.is_negative {
            match value.value.cmp(&(std::i64::MIN as u64)) {
                Ordering::Less => Ok(value.value as i64 * -1),
                Ordering::Equal => Ok(value.value as i64),
                Ordering::Greater => Err(()),
            }
        } else {
            match value.value.cmp(&(std::i64::MAX as u64)) {
                Ordering::Less | Ordering::Equal => Ok(value.value as i64),
                Ordering::Greater => Err(()),
            }
        }
    }
}

impl TryFrom<IntLiteral> for i32 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        i64::try_from(value).and_then(|val| i32::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for i16 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        i64::try_from(value).and_then(|val| i16::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for i8 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        i64::try_from(value).and_then(|val| i8::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for u64 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        if value.is_negative {
            Err(())
        } else {
            Ok(value.value)
        }
    }
}

impl TryFrom<IntLiteral> for u32 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        u64::try_from(value).and_then(|val| u32::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for u16 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        u64::try_from(value).and_then(|val| u16::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for u8 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        u64::try_from(value).and_then(|val| u8::try_from(val).map_err(|_| ()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn int_literal_try_from() {
        assert_eq!(
            i64::try_from(IntLiteral {
                value: std::i64::MAX as u64,
                is_negative: false
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i64::try_from(IntLiteral {
                value: std::i64::MIN as u64,
                is_negative: false
            })
            .is_err(),
            true
        );
        assert_eq!(
            i64::try_from(IntLiteral {
                value: std::i64::MIN as u64,
                is_negative: true
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i64::try_from(IntLiteral {
                value: (std::i64::MIN as u64) + 1,
                is_negative: true
            })
            .is_err(),
            true
        );

        assert_eq!(
            i32::try_from(IntLiteral {
                value: std::i32::MIN as u64,
                is_negative: false,
            })
            .is_err(),
            true
        );
        assert_eq!(
            i32::try_from(IntLiteral {
                value: std::i32::MAX as u64,
                is_negative: false,
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i32::try_from(IntLiteral {
                value: std::i32::MIN as u32 as u64,
                is_negative: true,
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i32::try_from(IntLiteral {
                value: (std::i32::MIN as u32 as u64) + 1,
                is_negative: true,
            })
            .is_err(),
            true
        );
    }
}
