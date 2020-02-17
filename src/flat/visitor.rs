//! Calls .resolve_type(), .resolve_value(), and .resolve_name() on each
//! Type, Term, and CompoundIdentifier in a raw AST, converting them to their
//! flat equivalents. This is roughly implemented in the visitor pattern but
//! the visitor is hardcoded to be the Resolver, and each OnFoo returns the
//! key (i.e. name as a String) and value (i.e. for types, Attributes and Type,
//! and for terms, Attributes, Term, and Type) that this decl would take inside
//! the library's scope
use super::errors::Error;
use super::resolve::Resolver;
use super::*;
use crate::raw;
use crate::raw::Spanned;
use crate::span;

// TODO: it doesn't really make sense to have a span for the rhs of a type decl
// (e.g. in struct Foo {...}). we could reconstruct a span starting at the open
// brace and ending at the close brace but since we currently never need to use
// this when displaying errors we just use a dummy span for now.
fn dummy_span<T>(value: T) -> Spanned<T> {
    span::spanned(span::FileId(0), 0, 0, value)
}

impl raw::Alias {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TypeEntry), Error> {
        let ty = resolver.resolve_type(self.ty)?;
        Ok((self.name.value, (self.attributes, ty)))
    }
}

impl raw::Const {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TermEntry), Vec<Error>> {
        let ty = resolver.resolve_type(self.ty);
        let term = resolver.resolve_term(self.value);

        let mut errors = Vec::new();
        if let Err(err) = &ty {
            errors.push(err.clone());
        }
        if let Err(err) = &term {
            errors.push(err.clone());
        }

        if errors.is_empty() {
            Ok((
                self.name.value,
                (self.attributes, ty.unwrap(), term.unwrap()),
            ))
        } else {
            Err(errors)
        }
    }
}

// TODO: some copies are unecessary but are done to get around partial moves when
// using the current pattern of doing if let and appending to a Vec of Errors

impl raw::Struct {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TypeEntry), Vec<Error>> {
        let mut members = Vec::new();
        let mut errors = Vec::new();
        for spanned in self.members {
            match spanned.try_map(|member| member.resolve(resolver)) {
                Ok(member) => members.push(member),
                Err(errs) => errors.extend(errs),
            };
        }
        if errors.is_empty() {
            let ty = Type::Struct(Struct { members });
            Ok((self.name.value, (self.attributes, dummy_span(ty))))
        } else {
            Err(errors)
        }
    }
}

impl raw::StructMember {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<StructMember, Vec<Error>> {
        let ty = resolver.resolve_type_boxed(self.ty);
        let default_value = self
            .default_value
            .map(|t| resolver.resolve_term(t))
            .transpose();

        let mut errors = Vec::new();
        if let Err(err) = &ty {
            errors.push(err.clone());
        }
        if let Err(err) = &default_value {
            errors.push(err.clone());
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(StructMember {
                attributes: self.attributes,
                ty: ty.unwrap(),
                name: self.name,
                default_value: default_value.unwrap(),
            })
        }
    }
}

impl raw::Bits {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TypeEntry), Vec<Error>> {
        let mut errors = Vec::new();

        let ty = self.ty.map(|t| resolver.resolve_type_boxed(t)).transpose();
        if let Err(err) = &ty {
            errors.push(err.clone());
        }

        let mut members = Vec::new();
        for spanned in self.members {
            match spanned.try_map(|member| member.resolve(resolver)) {
                Ok(member) => members.push(member),
                Err(err) => errors.push(err),
            };
        }

        if errors.is_empty() {
            let bits = Type::Bits(Bits {
                strictness: self.strictness,
                ty: ty.unwrap(),
                members,
            });
            Ok((self.name.value, (self.attributes, dummy_span(bits))))
        } else {
            Err(errors)
        }
    }
}

impl raw::BitsMember {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<BitsMember, Error> {
        Ok(BitsMember {
            attributes: self.attributes,
            name: self.name,
            value: resolver.resolve_term(self.value)?,
        })
    }
}

impl raw::Enum {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TypeEntry), Vec<Error>> {
        let mut errors = Vec::new();

        let ty = self.ty.map(|t| resolver.resolve_type_boxed(t)).transpose();
        if let Err(err) = &ty {
            errors.push(err.clone());
        }

        let mut members = Vec::new();
        for spanned in self.members {
            match spanned.try_map(|member| member.resolve(resolver)) {
                Ok(member) => members.push(member),
                Err(err) => errors.push(err),
            };
        }

        if errors.is_empty() {
            let num = Type::Enum(Enum {
                strictness: self.strictness,
                ty: ty.unwrap(),
                members,
            });
            Ok((self.name.value, (self.attributes, dummy_span(num))))
        } else {
            Err(errors)
        }
    }
}

impl raw::EnumMember {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<EnumMember, Error> {
        Ok(EnumMember {
            attributes: self.attributes,
            name: self.name,
            value: resolver.resolve_term(self.value)?,
        })
    }
}

impl raw::Table {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TypeEntry), Vec<Error>> {
        let mut members = Vec::new();
        let mut errors = Vec::new();
        for spanned in self.members {
            match spanned.try_map(|member| member.resolve(resolver)) {
                Ok(member) => members.push(member),
                Err(errs) => errors.extend(errs),
            }
        }

        if errors.is_empty() {
            let table = Type::Table(Table {
                strictness: self.strictness,
                members,
            });
            Ok((self.name.value, (self.attributes, dummy_span(table))))
        } else {
            Err(errors)
        }
    }
}

impl raw::TableMember {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<TableMember, Vec<Error>> {
        Ok(TableMember {
            attributes: self.attributes,
            ordinal: self.ordinal,
            inner: self.inner.resolve(resolver)?,
        })
    }
}

impl raw::TableMemberInner {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<TableMemberInner, Vec<Error>> {
        match self {
            raw::TableMemberInner::Reserved => Ok(TableMemberInner::Reserved),
            raw::TableMemberInner::Used {
                ty,
                name,
                default_value,
            } => {
                let ty = resolver.resolve_type_boxed(ty);
                let default_value = default_value.map(|t| resolver.resolve_term(t)).transpose();
                let mut errors = Vec::new();
                if let Err(err) = &ty {
                    errors.push(err.clone());
                }
                if let Err(err) = &default_value {
                    errors.push(err.clone());
                }
                if !errors.is_empty() {
                    Err(errors)
                } else {
                    Ok(TableMemberInner::Used {
                        ty: ty.unwrap(),
                        name,
                        default_value: default_value.unwrap(),
                    })
                }
            }
        }
    }
}

impl raw::Union {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, TypeEntry), Vec<Error>> {
        let mut members = Vec::new();
        let mut errors = Vec::new();
        for spanned in self.members {
            match spanned.try_map(|member| member.resolve(resolver)) {
                Ok(member) => members.push(member),
                Err(err) => errors.push(err),
            }
        }

        if errors.is_empty() {
            let union = Type::Union(Union {
                strictness: self.strictness,
                members,
            });
            Ok((self.name.value, (self.attributes, dummy_span(union))))
        } else {
            Err(errors)
        }
    }
}

impl raw::UnionMember {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<UnionMember, Error> {
        Ok(UnionMember {
            attributes: self.attributes,
            ordinal: self.ordinal,
            inner: self.inner.resolve(resolver)?,
        })
    }
}

impl raw::UnionMemberInner {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<UnionMemberInner, Error> {
        match self {
            raw::UnionMemberInner::Reserved => Ok(UnionMemberInner::Reserved),
            raw::UnionMemberInner::Used { ty, name } => Ok(UnionMemberInner::Used {
                ty: resolver.resolve_type_boxed(ty)?,
                name,
            }),
        }
    }
}

impl raw::Protocol {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, Protocol), Vec<Error>> {
        let mut errors = Vec::new();

        let mut methods = Vec::new();
        for spanned in self.methods {
            match spanned.try_map(|method| method.resolve(resolver)) {
                Ok(method) => methods.push(method),
                Err(errs) => errors.extend(errs),
            }
        }
        let mut compose = Vec::new();
        for c in self.compose {
            match resolver.resolve_name(c) {
                Ok(c) => compose.push(c),
                Err(errs) => errors.push(errs),
            }
        }

        if errors.is_empty() {
            let protocol = Protocol {
                attributes: self.attributes,
                methods,
                compose,
            };
            Ok((self.name.value, protocol))
        } else {
            Err(errors)
        }
    }
}

impl raw::Method {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<Method, Vec<Error>> {
        let request = self
            .request
            .map(|params| resolve_params(params, resolver))
            .transpose();
        let response = self
            .response
            .map(|params| resolve_params(params, resolver))
            .transpose();
        let error = self
            .error
            .map(|t| resolver.resolve_type_boxed(t))
            .transpose();

        let mut errors = Vec::new();
        if let Err(errs) = &request {
            errors.extend(errs.clone());
        }
        if let Err(errs) = &response {
            errors.extend(errs.clone());
        }
        if let Err(err) = &error {
            errors.push(err.clone());
        }
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(Method {
            attributes: self.attributes,
            name: self.name,
            request: request.unwrap(),
            response: response.unwrap(),
            error: error.unwrap(),
        })
    }
}

pub fn resolve_params(
    params: Vec<Spanned<raw::Parameter>>,
    resolver: &mut Resolver,
) -> Result<Vec<Spanned<Parameter>>, Vec<Error>> {
    let mut out = Vec::new();
    let mut errors = Vec::new();
    for spanned in params {
        match spanned.try_map(|param| param.resolve(resolver)) {
            Ok(param) => out.push(param),
            Err(err) => errors.push(err),
        };
    }
    if errors.is_empty() {
        Ok(out)
    } else {
        Err(errors)
    }
}

impl raw::Parameter {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<Parameter, Error> {
        Ok(Parameter {
            attributes: self.attributes,
            name: self.name,
            ty: resolver.resolve_type_boxed(self.ty)?,
        })
    }
}

impl raw::Service {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<(String, Service), Vec<Error>> {
        let mut members = Vec::new();
        let mut errors = Vec::new();
        for spanned in self.members {
            match spanned.try_map(|member| member.resolve(resolver)) {
                Ok(member) => members.push(member),
                Err(err) => errors.push(err),
            };
        }
        if errors.is_empty() {
            let service = Service {
                attributes: self.attributes,
                members,
            };
            Ok((self.name.value, service))
        } else {
            Err(errors)
        }
    }
}

impl raw::ServiceMember {
    pub fn resolve(self, resolver: &mut Resolver) -> Result<ServiceMember, Error> {
        Ok(ServiceMember {
            attributes: self.attributes,
            protocol: resolver.resolve_name(self.protocol)?,
            name: self.name,
        })
    }
}
