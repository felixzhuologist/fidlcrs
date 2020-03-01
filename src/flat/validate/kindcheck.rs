use super::errors::{Error, ParamType};
use super::eval::eval_type;
use crate::flat::*;
use crate::lexer::Span;
use crate::raw::Spanned;
use std::collections::HashMap;

// NOTE: this and type check can be modified not to take the cache arg. instead,
// it will return a Kind and the Names that it has traversed, and the caller
// can store the fact that each of those Names has the returned kind.

pub fn kind_check(
    ty: Spanned<&Type>,
    scope: &Libraries,
    cache: &mut HashMap<Name, Kind>,
) -> Result<Kind, Vec<Error>> {
    let seen = Vec::new();
    _kind_check(ty, scope, cache, seen)
}

fn _kind_check(
    ty: Spanned<&Type>,
    scope: &Libraries,
    cache: &mut HashMap<Name, Kind>,
    mut seen: Vec<(Name, Span)>,
) -> Result<Kind, Vec<Error>> {
    match &ty.value {
        Type::Identifier(name) => {
            if let Some(kind) = cache.get(name) {
                return Ok(*kind);
            }

            if seen.iter().any(|(n, _)| n == name) {
                return Err(Error::VarCycle(seen).into());
            }
            // NOTE: once Name is refactored just contain IDs, it can implement Copy and we won't
            // need to clone explicitly here
            seen.push((name.clone(), ty.span));
            let kind = _kind_check(
                scope.get_type(ty.span.wrap(name))?.into(),
                scope,
                cache,
                seen,
            );
            cache.insert(
                name.clone(),
                match kind {
                    Ok(k) => k,
                    Err(_) => Kind::Any,
                },
            );
            kind
        }
        Type::Array(Array { element_type, size }) => Ok(Kind::Kind {
            layout: Param::required(element_type),
            constraints: Param::required(size),
        }),
        Type::Vector(Vector {
            element_type,
            bounds,
        }) => Ok(Kind::Kind {
            layout: Param::required(element_type),
            constraints: Param::optional(bounds),
        }),
        Type::Str(Str { bounds }) => Ok(Kind::Kind {
            layout: Param::None,
            constraints: Param::optional(bounds),
        }),
        Type::TypeSubstitution(TypeSubstitution {
            func,
            layout: layout_param,
            constraint: constraint_param,
        }) => {
            let func_kind = _kind_check(func.into(), scope, cache, seen)?;
            let func_def = match &*func.value {
                Type::Identifier(name) => Some(scope.get_type(ty.span.wrap(name))?.span),
                _ => None,
            };

            match func_kind {
                Kind::Any => Ok(Kind::Any),
                Kind::Kind {
                    layout,
                    constraints,
                } => {
                    let mut errors = Vec::new();
                    let layout = layout
                        .take(layout_param)
                        .map_err(|_| Error::InvalidTypeParam {
                            func_call: func.span,
                            param: ParamType::Layout,
                            func_def,
                        });
                    let constraints =
                        constraints
                            .take(constraint_param)
                            .map_err(|_| Error::InvalidTypeParam {
                                func_call: func.span,
                                param: ParamType::Constraint,
                                func_def,
                            });
                    if let Err(err) = layout.clone() {
                        errors.push(err);
                    }
                    if let Err(err) = constraints.clone() {
                        errors.push(err);
                    }

                    // NOTE: we error if an argument is provided that is not supported by the func type,
                    // but don't if an argument that is "needed" is not provided (so we provide some
                    // "currying" like behavior to match fidlc's type constructors)
                    if errors.is_empty() {
                        Ok(Kind::Kind {
                            layout: layout.unwrap(),
                            constraints: constraints.unwrap(),
                        })
                    } else {
                        Err(errors)
                    }
                }
            }
        }
        // TODO: think about this some more
        Type::Any => Ok(Kind::Any),
        _ => Ok(Kind::base_kind()),
    }
}

// really the caller should handle Kind::Any to have whatever result it expects,
// but currently fidlc will only ever expect concrete types so this hardcoded
// here
impl Kind {
    pub fn is_concrete(&self) -> bool {
        match self {
            Kind::Any => true,
            Kind::Kind {
                layout,
                constraints,
            } => !layout.needs_value() || !constraints.needs_value(),
        }
    }

    pub fn missing(&self) -> Vec<ParamType> {
        let mut missing = Vec::new();
        match self {
            Kind::Any => missing,
            Kind::Kind {
                layout,
                constraints,
            } => {
                if layout.needs_value() {
                    missing.push(ParamType::Layout);
                }
                if constraints.needs_value() {
                    missing.push(ParamType::Constraint);
                }
                missing
            }
        }
    }
}

pub fn recursion_check(ty: Spanned<&Type>, scope: &Libraries, cache: &HashMap<Name, Kind>) -> Result<(), Vec<(Name, Span)>> {
    let mut seen = Vec::new();
    if !can_be_finite(ty, scope, &mut seen, cache) {
        Err(seen)
    } else {
        Ok(())
    }
}

fn can_be_finite(ty: Spanned<&Type>, scope: &Libraries, seen: &mut Vec<(Name, Span)>, cache: &HashMap<Name, Kind>) -> bool {
    match ty.value {
        Type::Struct(Struct { members }) => members
            .iter()
            .all(|member| can_be_finite((&member.value.ty).into(), scope, seen, cache)),
        Type::Table(_) => true,
        Type::Union(Union { members, .. }) => {
            members.iter().any(|member| match member.value.inner {
                UnionMemberInner::Reserved => false,
                UnionMemberInner::Used { ref ty, .. } => can_be_finite(ty.into(), scope, seen, cache),
            })
        }
        Type::Identifier(name) => {
            if let Some(Kind::Any) = cache.get(name) {
                // don't duplicate cycle errors
                return true;
            }

            if seen.iter().any(|(n, _)| n == name) {
                return false;
            }
            seen.push((name.clone(), ty.span));
            let ty = scope.get_type(ty.span.wrap(name)).unwrap();
            let can_be_finite = can_be_finite(ty.into(), scope, seen, cache);
            if can_be_finite {
                seen.pop();
            }
            can_be_finite
        }
        Type::Ptr(_) => true,
        Type::Array(Array { element_type, .. }) => {
            if let Some(ref inner) = element_type {
                can_be_finite(inner.into(), scope, seen, cache)
            } else {
                true
            }
        }
        // a vector could always have 0 elements
        Type::Vector(_) | Type::Str(_) => true,
        Type::TypeSubstitution(_) => {
            let evaled = eval_type(ty.into(), scope).unwrap();
            can_be_finite(evaled.span.wrap(&evaled.value), scope, seen, cache)
        }
        _ => true,
    }
}
