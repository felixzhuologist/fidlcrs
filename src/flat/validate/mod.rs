//! Once everything is resolved, this does validation of the FIDL file, including things like
//! type and kind checking
mod errors;
mod visitor;

use crate::flat::PrimitiveSubtype::*;
use crate::flat::*;
use crate::lexer::Span;
use crate::raw::{IntLiteral, Spanned};
use errors::{Error, ParamType};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryFrom;

impl Libraries {
    pub fn validate_latest(&self) {
        let mut errors = Vec::new();
        let lib = self.libraries.last().unwrap();

        // kind checking
        let mut kinds = HashMap::new();
        for (name, entry) in lib.types.iter() {
            let name = self.to_name(name);
            let (_, ty) = &entry.value;
            // TODO: this logic is duplicated in kind_check
            match kind_check(ty.into(), self, &mut kinds) {
                Ok(kind) => kinds.insert(name, kind),
                Err(errs) => {
                    errors.extend(errs);
                    kinds.insert(name, Kind::Any)
                }
            };
        }

        // type checking
        let mut types = HashMap::new();
        for (name, entry) in lib.terms.iter() {
            let name = self.to_name(name);
            let (_, _, term) = &entry.value;
            // TODO: this logic is duplicated in type_check
            match type_check(term.into(), self, &mut types) {
                Ok(ty) => types.insert(name, ty),
                Err(errs) => {
                    errors.push(errs);
                    types.insert(name, Type::Any)
                }
            };
        }

        // term validation
        for (name, entry) in lib.terms.iter() {
            let (_, ty, term) = &entry.value;
            let kind = kind_check(ty.into(), self, &mut kinds).unwrap();
            if !kind.is_concrete() {
                errors.push(Error::NonConcreteType {
                    span: ty.span,
                    missing: kind.missing(),
                });
                // NOTE: this may not be necessary if we want to return
                // both errors
                continue;
            }

            let name = self.to_name(name);
            match types.get(&name).unwrap() {
                Type::Any => continue,
                ref actual => {
                    if let Err(err) =
                        term_can_have_type(eval_term(&term, self).unwrap(), &ty.value, actual)
                    {
                        errors.push(err);
                    }
                }
            }
        }

        // type validation
        for (_, entry) in lib.types.iter() {
            let (_, ty) = &entry.value;
            match &ty.value {
                Type::Struct(Struct { ref members }) => {
                    for member in members {
                        let kind = kind_check((&member.value.ty).into(), self, &mut kinds).unwrap();
                        if !kind.is_concrete() {
                            errors.push(Error::NonConcreteType {
                                span: member.value.ty.span,
                                missing: kind.missing(),
                            });
                            continue;
                        }

                        if let Some(term) = member.value.default_value.as_ref() {
                            match type_check(term.into(), self, &mut types).unwrap() {
                                Type::Any => continue,
                                ref actual => {
                                    if let Err(err) = term_can_have_type(
                                        eval_term(&term, self).unwrap(),
                                        &ty.value,
                                        actual,
                                    ) {
                                        errors.push(err);
                                    }
                                }
                            }
                        }
                    }
                }
                Type::Bits(bits) => {
                    if let Some(ty) = bits.ty.as_ref() {
                        let kind = kind_check(ty.into(), self, &mut kinds).unwrap();
                        if !kind.is_concrete() {
                            errors.push(Error::NonConcreteType {
                                span: ty.span,
                                missing: kind.missing(),
                            });
                            continue;
                        }
                        // TODO: can be bits
                        // if !ty.value.is_numeric() {
                        //     errors.push(Error::InvalidBitsType(ty.into().clone()));
                        // }
                    }

                    // TODO: need to get the type of the term as actual
                    // let expected = bits.get_type();
                    // for member in bits.members {
                    //     if let Err(err) =
                    //         term_can_have_type(eval_term(&member.value, self).unwrap(), &expected, actual)
                    //     {
                    //         errors.push(err);
                    //     }
                    // }
                }
                Type::Enum(_) => unimplemented!(),
                Type::Table(Table {
                    strictness: _,
                    members,
                }) => {
                    for member in members {
                        let inner = &member.value.inner;
                        if let TableMemberInner::Used {
                            ty,
                            name: _,
                            default_value: _,
                        } = inner
                        {
                            let kind = kind_check(ty.into(), self, &mut kinds).unwrap();
                            if !kind.is_concrete() {
                                errors.push(Error::NonConcreteType {
                                    span: ty.span,
                                    missing: kind.missing(),
                                });
                            }
                            continue;
                        }
                        if let Type::Ptr(_) = ty.value {
                            errors.push(Error::NullableTableMember(member.span));
                        }
                        // TODO: just remove default_values altogether for tables
                    }
                }
                Type::Union(_) => unimplemented!(),
                Type::Ptr(_) => {
                    // TODO: need to eval type first to deterine if it's nullable?
                }
                Type::ClientEnd(name) | Type::ServerEnd(name) => {
                    if let Err(err) = self.get_protocol(ty.span.wrap(name)) {
                        errors.push(err);
                    }
                }
                _ => continue,
            };
        }

        // protocol validation
        for protocol in lib.protocols.values() {
            for compose in &protocol.value.compose {
                if let Err(err) = self.get_protocol(compose.into()) {
                    errors.push(err);
                }
            }
        }

        // service validation
        for service in lib.services.values() {
            for member in &service.value.members {
                if let Err(err) = self.get_protocol((&member.value.protocol).into()) {
                    errors.push(err);
                }
            }
        }
    }

    // TODO: this won't be needed once library's keys are Names
    fn to_name(&self, _name: &String) -> Name {
        unimplemented!()
    }

    pub fn get_type(&self, name: Spanned<&Name>) -> Result<&Spanned<Type>, Error> {
        let span = name.span;
        let name = name.value;
        let lib = &self.libraries[name.library.0];
        match lib.types.get(&name.name) {
            Some(sp) => Ok(&sp.value.1),
            None => Err(Error::SortError {
                span,
                expected: Sort::Type,
                actual: self.get_sort(name),
            }),
        }
    }

    pub fn get_term(&self, name: Spanned<&Name>) -> Result<&Spanned<Term>, Error> {
        let span = name.span;
        let name = name.value;
        let lib = &self.libraries[name.library.0];
        match lib.terms.get(&name.name) {
            Some(sp) => Ok(&sp.value.2),
            None => Err(Error::SortError {
                span,
                expected: Sort::Term,
                actual: self.get_sort(name),
            }),
        }
    }

    pub fn get_protocol(&self, name: Spanned<&Name>) -> Result<&Spanned<Protocol>, Error> {
        let span = name.span;
        let name = name.value;
        let lib = &self.libraries[name.library.0];
        lib.protocols
            .get(&name.name)
            .ok_or_else(|| Error::SortError {
                span,
                expected: Sort::Protocol,
                actual: self.get_sort(name),
            })
    }

    pub fn get_service(&self, name: Spanned<&Name>) -> Result<&Spanned<Service>, Error> {
        let span = name.span;
        let name = name.value;
        let lib = &self.libraries[name.library.0];
        lib.services
            .get(&name.name)
            .ok_or_else(|| Error::SortError {
                span,
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

// TODO: implement
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

// TODO: we can either pass in evaled terms or have the functions recurse
// themselves
pub fn eval_term<'a>(
    term: &'a Spanned<Term>,
    scope: &'a Libraries,
) -> Result<&'a Spanned<Term>, Error> {
    match &term.value {
        Term::Identifier(name) => scope.get_term(term.span.wrap(name)),
        _ => Ok(term),
    }
}

pub fn term_can_have_type(
    term: &Spanned<Term>,
    expected_ty: &Type,
    actual_ty: &Type,
) -> Result<(), Error> {
    match (&term.value, expected_ty) {
        (Term::Identifier(_), _) => panic!("only pass evaled terms"),
        (_, Type::Identifier(_)) => panic!("only pass evaled types"),
        (_, Type::Int) => panic!("users can't specify untyped Ints"),
        // // TODO: check bounds. to do this, we need to kind check and
        // // type eval the bounds
        // (Term::Str(_), Type::Str(_bounds)) => Ok(expected_ty),
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
