//! Once everything is resolved, this does validation of the FIDL file, including things like
//! type and kind checking
mod errors;
mod visitor;

use crate::flat::*;
use crate::lexer::Span;
use crate::raw::Spanned;
use errors::{Error, ParamType};
use std::collections::HashMap;

impl Libraries {
    pub fn validate_latest(&self) {
        // let errors = Vec::new();
        let lib = self.libraries.last().unwrap();
        for entry in lib.terms.values() {
            let (_, _ty, term) = &entry.value;
            term.value.validate(self);
        }
        for _ty in lib.types.values() {
            // ty.validate(self);
        }
        for _protocol in lib.protocols.values() {
            // protocol.validate(self);
        }
        for _service in lib.services.values() {
            // service.validate(self);
        }
    }

    pub fn get_type(&self, name: &Name) -> Result<&Spanned<Type>, Error> {
        let lib = &self.libraries[name.library.0];
        match lib.types.get(&name.name) {
            Some(sp) => Ok(&sp.value.1),
            None => Err(Error::SortError {
                expected: Sort::Type,
                actual: self.get_sort(name),
            }),
        }
    }

    pub fn get_term(&self, name: &Name) -> Result<&Spanned<Term>, Error> {
        let lib = &self.libraries[name.library.0];
        match lib.terms.get(&name.name) {
            Some(sp) => Ok(&sp.value.2),
            None => Err(Error::SortError {
                expected: Sort::Term,
                actual: self.get_sort(name),
            }),
        }
    }

    pub fn get_protocol(&self, name: &Name) -> Result<&Spanned<Protocol>, Error> {
        let lib = &self.libraries[name.library.0];
        lib.protocols
            .get(&name.name)
            .ok_or_else(|| Error::SortError {
                expected: Sort::Protocol,
                actual: self.get_sort(name),
            })
    }

    pub fn get_service(&self, name: &Name) -> Result<&Spanned<Service>, Error> {
        let lib = &self.libraries[name.library.0];
        lib.services
            .get(&name.name)
            .ok_or_else(|| Error::SortError {
                expected: Sort::Service,
                actual: self.get_sort(name),
            })
    }

    fn get_sort(&self, name: &Name) -> Sort {
        let lib = &self.libraries[name.library.0];
        if lib.terms.contains_key(&name.name) {
            return Sort::Term;
        }
        if lib.types.contains_key(&name.name) {
            return Sort::Type;
        }
        if lib.protocols.contains_key(&name.name) {
            return Sort::Protocol;
        }
        if lib.services.contains_key(&name.name) {
            return Sort::Service;
        }
        panic!("unknown name {:?}", name);
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Sort {
    Term,
    Type,
    Protocol,
    Service,
}

// TODO: this can be cached: we can have a map from Name to Result<Kind, Error>.
// would need to be careful not to return the same error multiple times - one
// way to do this is the value for an errored type is just a "top" kind, so that
// any type that refers to that type can interpret it as any kind it wants. in the
// case of an alias cycle, we would want to store the error once and mark the other
// types in the cycle
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
                // update the scope so that this error doesn't get recomputed
                for (n, _) in seen.iter() {
                    cache.insert(n.clone(), Kind::Any);
                }
                return Err(Error::VarCycle(seen).into());
            }
            // NOTE: once Name is refactored just contain IDs, it can implement Copy and we won't
            // need to clone explicitly here
            seen.push((name.clone(), ty.span));
            _kind_check(scope.get_type(name)?.into(), scope, cache, seen)
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
                Type::Identifier(name) => Some(scope.get_type(name)?.span),
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

// TODO: these Froms are kind of gnarly, but i'm not sure there's a way around them right now:
// kind_check is going to be called from 2 places: recursively on a flat::Type, which will always
// be a Spanned<Box<Type>>, but it will also be called on a type fetched from the scope which is
// a &Spanned<Type>. I wasn't able to pick one and get it to work, but both of these types can
// be converted to Spanned<&Type> so that is currently the input type of kind_check. The other
// constraint is that we want this function to borrow it's input spanned type and avoid copying.

impl<'a, T> From<&'a Spanned<Box<T>>> for Spanned<&'a T> {
    fn from(ty: &'a Spanned<Box<T>>) -> Self {
        let borrowed_inner = &ty.value;
        ty.span.wrap(borrowed_inner)
    }
}

impl<'a, T> From<&'a Spanned<T>> for Spanned<&'a T> {
    fn from(ty: &'a Spanned<T>) -> Self {
        let borrowed_inner = &ty.value;
        ty.span.wrap(borrowed_inner)
    }
}

impl From<Error> for Vec<Error> {
    fn from(err: Error) -> Self {
        vec![err]
    }
}

// impl fmt::Display for Kind {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match (
//             self.needs_layout,
//             self.needs_constraints,
//             self.can_take_constraints,
//         ) {
//             (false, false, false) => write!(f, "Type"),
//             (false, false, true) => write!(f, "Constraint? -> Type"),
//             (false, true, _) => write!(f, "Constraint -> Type"),
//             (true, false, false) => write!(f, "Layout -> Type"),
//             (true, false, true) => write!(f, "(Layout, Constraint?) -> Type"),
//             (true, true, _) => write!(f, "(Layout, Constraint) -> Type"),
//         }
//     }
// }

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
                // update the scope so that this error doesn't get recomputed
                for (n, _) in seen.iter() {
                    cache.insert(n.clone(), Type::Any);
                }
                return Err(Error::VarCycle(seen).into());
            }
            // NOTE: once Name is refactored just contain IDs, it can implement Copy and we won't
            // need to clone explicitly here
            seen.push((name.clone(), term.span));
            _type_check(scope.get_term(name)?.into(), scope, cache, seen)
        }
        Term::Str(_) => Ok(Type::Str(Str { bounds: None })),
        // TODO: think about this some more
        Term::Int(_) => Ok(Type::Int),
        Term::Float(_) => Ok(Type::Primitive(PrimitiveSubtype::Float64)),
        Term::True | Term::False => Ok(Type::Primitive(PrimitiveSubtype::Bool)),
    }
}
