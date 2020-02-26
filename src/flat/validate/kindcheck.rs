use super::errors::{Error, ParamType};
use crate::flat::*;
use crate::lexer::Span;
use crate::raw::Spanned;
use std::collections::HashMap;

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
