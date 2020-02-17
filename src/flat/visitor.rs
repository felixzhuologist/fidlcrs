//! Calls .resolve_type(), .resolve_value(), and .resolve_name() on each
//! Type, Term, and CompoundIdentifier in a raw AST, converting them to their
//! flat equivalents. This is roughly implemented in the visitor pattern but
//! the visitor is hardcoded to be the Resolver, and each OnFoo returns the
//! key (i.e. name as a String) and value (i.e. for types, Attributes and Type,
//! and for terms, Attributes, Term, and Type) that this decl would take inside
//! the library's scope
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
    pub fn resolve(self, resolver: &Resolver) -> (String, TypeEntry) {
        (
            self.name.value,
            (self.attributes, resolver.resolve_type(self.ty)),
        )
    }
}

impl raw::Const {
    pub fn resolve(self, resolver: &Resolver) -> (String, TermEntry) {
        let ty = resolver.resolve_type(self.ty);
        let term = resolver.resolve_term(self.value);
        (self.name.value, (self.attributes, ty, term))
    }
}

// TODO: implement map over Vec<Spanned<T>>, once error handling is done

impl raw::Struct {
    pub fn resolve(self, resolver: &Resolver) -> (String, TypeEntry) {
        let mut members = Vec::new();
        for spanned in self.members {
            members.push(spanned.map(|member| member.resolve(resolver)));
        }
        let ty = Type::Struct(Struct { members });
        (self.name.value, (self.attributes, dummy_span(ty)))
    }
}

impl raw::StructMember {
    pub fn resolve(self, resolver: &Resolver) -> StructMember {
        let ty = resolver.resolve_type_boxed(self.ty);
        let default_value = self.default_value.map(|t| resolver.resolve_term(t));
        StructMember {
            attributes: self.attributes,
            ty,
            name: self.name,
            default_value,
        }
    }
}

impl raw::Bits {
    pub fn resolve(self, resolver: &Resolver) -> (String, TypeEntry) {
        let ty = self.ty.map(|t| resolver.resolve_type_boxed(t));
        let mut members = Vec::new();
        for spanned in self.members {
            members.push(spanned.map(|member| member.resolve(resolver)));
        }
        let bits = Type::Bits(Bits {
            strictness: self.strictness,
            ty,
            members,
        });
        (self.name.value, (self.attributes, dummy_span(bits)))
    }
}

impl raw::BitsMember {
    pub fn resolve(self, resolver: &Resolver) -> BitsMember {
        BitsMember {
            attributes: self.attributes,
            name: self.name,
            value: resolver.resolve_term(self.value),
        }
    }
}

impl raw::Table {
    pub fn resolve(self, resolver: &Resolver) -> (String, TypeEntry) {
        let mut members = Vec::new();
        for spanned in self.members {
            members.push(spanned.map(|member| member.resolve(resolver)));
        }
        let table = Type::Table(Table {
            strictness: self.strictness,
            members,
        });
        (self.name.value, (self.attributes, dummy_span(table)))
    }
}

impl raw::TableMember {
    pub fn resolve(self, resolver: &Resolver) -> TableMember {
        TableMember {
            attributes: self.attributes,
            ordinal: self.ordinal,
            inner: self.inner.resolve(resolver),
        }
    }
}

impl raw::TableMemberInner {
    pub fn resolve(self, resolver: &Resolver) -> TableMemberInner {
        match self {
            raw::TableMemberInner::Reserved => TableMemberInner::Reserved,
            raw::TableMemberInner::Used {
                ty,
                name,
                default_value,
            } => TableMemberInner::Used {
                ty: resolver.resolve_type_boxed(ty),
                name,
                default_value: default_value.map(|t| resolver.resolve_term(t)),
            },
        }
    }
}

impl raw::Protocol {
    pub fn resolve(self, resolver: &Resolver) -> (String, Protocol) {
        let mut methods = Vec::new();
        for spanned in self.methods {
            methods.push(spanned.map(|method| method.resolve(resolver)));
        }
        let mut compose = Vec::new();
        for c in self.compose {
            compose.push(resolver.resolve_name(c));
        }
        let protocol = Protocol {
            attributes: self.attributes,
            methods,
            compose,
        };
        (self.name.value, protocol)
    }
}

impl raw::Method {
    pub fn resolve(self, resolver: &Resolver) -> Method {
        let request = self.request.map(|params| {
            params
                .into_iter()
                .map(|sp| sp.map(|param| param.resolve(resolver)))
                .collect::<Vec<_>>()
        });
        let response = self.response.map(|params| {
            params
                .into_iter()
                .map(|sp| sp.map(|param| param.resolve(resolver)))
                .collect::<Vec<_>>()
        });
        Method {
            attributes: self.attributes,
            name: self.name,
            request,
            response,
            error: self.error.map(|t| resolver.resolve_type_boxed(t)),
        }
    }
}

impl raw::Parameter {
    pub fn resolve(self, resolver: &Resolver) -> Parameter {
        Parameter {
            attributes: self.attributes,
            name: self.name,
            ty: resolver.resolve_type_boxed(self.ty),
        }
    }
}

impl raw::Service {
    pub fn resolve(self, resolver: &Resolver) -> (String, Service) {
        let mut members = Vec::new();
        for spanned in self.members {
            members.push(spanned.map(|member| member.resolve(resolver)));
        }
        let service = Service {
            attributes: self.attributes,
            members,
        };
        (self.name.value, service)
    }
}

impl raw::ServiceMember {
    pub fn resolve(self, resolver: &Resolver) -> ServiceMember {
        ServiceMember {
            attributes: self.attributes,
            protocol: resolver.resolve_name(self.protocol),
            name: self.name,
        }
    }
}
