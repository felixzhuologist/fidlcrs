//! Once everything is resolved, this does validation of the FIDL file, including things like
//! type and kind checking
mod errors;
mod eval;
mod kindcheck;
mod typecheck;
mod visitor;

use crate::flat::*;
use crate::raw::Spanned;
use errors::Error;
use eval::{eval_term, eval_type};
use kindcheck::kind_check;
use std::collections::HashMap;
use typecheck::{term_can_have_type, type_check};

pub struct Validator<'a> {
    pub scope: &'a Libraries,
    /// Stores the computed Kinds of the Types in scope. Types with Kind errors
    /// have a Kind of Kind::Any - keeping track of these ensures that the same
    /// error isn't returned each the type is used
    pub kinds: HashMap<Name, Kind>,
    /// Stores the computed Types of the Terms in scope. Terms with Type errors
    /// have a Type of Type::Any - keeping track of these ensures that the same
    /// error isn't returned each the type is used
    pub types: HashMap<Name, Type>,
    pub errors: Vec<Error>,
}

impl<'a> Validator<'a> {
    pub fn new(libs: &'a Libraries) -> Validator {
        Validator {
            scope: libs,
            kinds: HashMap::new(),
            types: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn run(mut self) -> Vec<Error> {
        let lib = self.scope.libraries.last().unwrap();

        self.precompute_types(&lib.terms);
        self.precompute_kinds(&lib.types);

        self.validate_terms(&lib.terms);
        self.validate_types(&lib.types);
        self.validate_protocols(lib);
        self.validate_services(lib);

        self.errors
    }

    /// Determine Types for all top level const declarations and store them in
    /// self.types
    fn precompute_types(&mut self, scope: &TermScope) {
        for (name, entry) in scope {
            let name = self.to_name(name);
            let (_, _, term) = &entry.value;
            if !self.types.contains_key(&name) {
                let kind = self.type_check(term.into());
                self.types.insert(name, kind);
            }
        }
    }

    /// Determine Kinds for all top level type declarations and store them in
    /// self.kinds
    fn precompute_kinds(&mut self, scope: &TypeScope) {
        for (name, entry) in scope {
            let name = self.to_name(name);
            let (_, ty) = &entry.value;
            if !self.kinds.contains_key(&name) {
                let kind = self.kind_check(ty.into());
                self.kinds.insert(name, kind);
            }
        }
    }

    /// For each const declaration, check that the type/terms are valid, and
    /// that the term can be coerced to have the user specified type.
    fn validate_terms(&mut self, scope: &TermScope) {
        // at this point, self.types.keys() == lib.terms.keys()
        for (name, entry) in scope {
            // NOTE: this checking of a type and term is common, but used in different
            // contexts (e.g. in Struct, the term is optional and in bits/enums, there
            // is a single non spanned expected type for each member value). we may want
            // to refactor them all into a single code path
            let (_, ty, term) = &entry.value;
            let ref_ty: Spanned<&Type> = ty.into();
            if let Err(err) = ref_ty.validate(self) {
                self.errors.push(err);
            }

            let name = self.to_name(name);
            let actual = self.types.get(&name).unwrap().clone();
            match actual {
                Type::Any => continue,
                ref actual => {
                    if let Err(err) = self.term_can_have_type(&term, ty, actual) {
                        self.errors.push(err);
                    }
                }
            }
        }
    }

    fn validate_types(&mut self, scope: &TypeScope) {
        // at this point, self.kinds.keys() == lib.types.keys()
        for (_, entry) in scope {
            let (_, ty) = &entry.value;
            self.validate_type(ty.into());
        }
    }

    fn validate_type(&mut self, ty: Spanned<&Type>) {
        match &ty.value {
            Type::Struct(val) => val.validate(self),
            Type::Bits(val) => val.validate(self),
            Type::Enum(val) => val.validate(self),
            Type::Table(val) => val.validate(self),
            Type::Union(val) => val.validate(self),
            Type::Ptr(val) => {
                let evaled = eval_type(val.into(), self.scope).unwrap();
                if let Err(err) = can_be_nullable(&evaled) {
                    self.errors.push(err);
                }
                self.validate_type(evaled.span.wrap(&evaled.value));
            }
            Type::ClientEnd(name) | Type::ServerEnd(name) => {
                if let Err(err) = self.scope.get_protocol(ty.span.wrap(name)) {
                    self.errors.push(err);
                }
            }
            Type::TypeSubstitution(_) => {
                // this is OK to unwrap since ty should already be kind checked
                let evaled = eval_type(ty, self.scope).unwrap();
                self.validate_type(evaled.span.wrap(&evaled.value));
            }
            // if this is an identifier its entry will exist in the type scope. since we call
            // validate_type on all top level types, we don't need to recurse here.
            Type::Identifier(_) => (),
            // base types
            Type::Array(_)
            | Type::Str(_)
            | Type::Vector(_)
            | Type::Handle(_)
            | Type::Primitive(_)
            | Type::Int
            | Type::Any => (),
        }
    }

    fn validate_protocols(&mut self, lib: &Library) {
        for protocol in lib.protocols.values() {
            protocol.value.validate(self);
        }
    }

    fn validate_services(&mut self, lib: &Library) {
        for service in lib.services.values() {
            for member in &service.value.members {
                if let Err(err) = self.scope.get_protocol((&member.value.protocol).into()) {
                    self.errors.push(err);
                }
            }
        }
    }

    // TODO: cloning. is it possible to make library keys Names? or to make
    // Names only consist of IDs?
    fn to_name(&self, name: &String) -> Name {
        Name {
            // hardcoded to the most recent library
            library: LibraryId(self.scope.libraries.len() - 1),
            name: name.clone(),
            member: None,
        }
    }

    pub fn kind_check(&mut self, ty: Spanned<&Type>) -> Kind {
        match kind_check(ty.into(), self.scope, &mut self.kinds) {
            Ok(kind) => kind,
            Err(errs) => {
                self.errors.extend(errs);
                Kind::Any
            }
        }
    }

    pub fn type_check(&mut self, term: Spanned<&Term>) -> Type {
        match type_check(term.into(), self.scope, &mut self.types) {
            Ok(ty) => ty,
            Err(errs) => {
                self.errors.push(errs);
                Type::Any
            }
        }
    }

    pub fn term_can_have_type(
        &mut self,
        term: &Spanned<Term>,
        ty: &Spanned<Type>,
        actual: &Type,
    ) -> Result<(), Error> {
        let term = eval_term(term.into(), self.scope)?;
        let ty = eval_type(ty.into(), self.scope)?;
        term_can_have_type(term, &ty.value, &actual, self)
    }
}

// TODO: these Froms are kind of gnarly, but i'm not sure there's a way around them right now:
// kind_check is going to be called from 2 places: recursively on a flat::Type, which will always
// be a Spanned<Box<Type>>, but it will also be called on a type fetched from the scope which is
// a &Spanned<Type>. I wasn't able to pick one and get it to work, but both of these types can
// be converted to Spanned<&Type> so that is currently the input type of kind_check. The other
// constraint is that we want this function to borrow its input spanned type and avoid copying.

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

impl Libraries {
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

fn can_be_nullable(ty: &Spanned<Type>) -> Result<(), Error> {
    match &ty.value {
        // TODO: not allowing double nullable types makes me realize that
        // nullability is maybe better included in the kind system
        Type::Primitive(_) | Type::Array(_) => Err(Error::TypeCantBeNullable(ty.clone())),
        Type::Ptr(_) => Err(Error::DoubleNullability(ty.span)),
        _ => Ok(()),
    }
}
