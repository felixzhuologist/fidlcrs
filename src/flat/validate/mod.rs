//! Once everything is resolved, this does validation of the FIDL file, including things like
//! type and kind checking
mod errors;
mod visitor;

use crate::flat::*;
use crate::raw::Spanned;
use errors::{Error, ParamType};

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

// The set of possible Kinds would essentially be the 2^3 possible combinations
// of these booleans, which is a bit excessive to represent as an enum
#[derive(Debug, Copy, Clone)]
pub struct Kind {
    pub layout: Param,
    pub constraints: Param,
}

impl Kind {
    pub fn base_kind() -> Kind {
        Kind {
            layout: Param::None,
            constraints: Param::None,
        }
    }

    pub fn is_concrete(&self) -> bool {
        self.layout.needs_value() && self.constraints.needs_value()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Param {
    /// this parameter is required for the type to be concrete
    Required,
    /// this parameter is not required for the type to be concrete, but could
    /// be passed
    Optional,
    /// the type does not take this parameter
    // technically Param should only have two states, and layout/constraints
    // should be Option<Param> but it's simpler just to flatten into 3 states
    None,
}

impl Param {
    pub fn required<T>(val: &Option<T>) -> Param {
        if val.is_some() {
            Param::None
        } else {
            Param::Required
        }
    }

    pub fn optional<T>(val: &Option<T>) -> Param {
        if val.is_some() {
            Param::None
        } else {
            Param::Optional
        }
    }

    // substitute this Option into this Param
    pub fn take<T>(self, val: &Option<T>) -> Result<Param, ()> {
        match (self, val) {
            // param can take a value, and value is defined
            (Param::Required, Some(_)) | (Param::Optional, Some(_)) => Ok(Param::None),
            // param can't take any values but there is a value
            (Param::None, Some(_)) => Err(()),
            // param can't take any values but there is no value
            (Param::None, None) => Ok(Param::None),
            // param can take a value but there's no value
            (param, None) => Ok(param),
        }
    }

    pub fn needs_value(&self) -> bool {
        match self {
            Param::Required => true,
            Param::Optional | Param::None => false,
        }
    }
}

// // TODO: it seems like this will be a general "type checking" function that will
// // be called on every type, so it should be updated to not only do "kind" stuff
// // but also do other validation
// pub fn kind_check(ty: &Spanned<Type>, scope: &Libraries) -> Result<Kind, Error> {
//     match &ty.value {
//         // NOTE: since there are no anonymous types yet, we don't need to recurse
//         // here to check that member kinds are valid because that will be called
//         // by top level validate eventually as it iterates through the scope. if
//         // we wanted this to be a full recursive kind check instead of just at the
//         // top level, we could cache the results
//         Type::Struct(_) |
//         Type::Bits(_) |
//         Type::Enum(_) |
//         Type::Table(_) |
//         Type::Union(_) => Ok(Kind::base_kind()),
//         // TODO: check that it can be nullable
//         // Type::Ptr(ref ty) => kind_check(ty, scope),
//         // TODO: make recursive type/term field types consistent
//         Type::Ptr(ref ty) => unimplemented!(),
//         // TODO: this can be cached
//         Type::Identifier(name) => kind_check(scope.get_type(name)?, scope),
//         Type::Array(Array { element_type, size }) => Ok(Kind {
//             layout: Param::required(element_type),
//             constraints: Param::required(size),
//         }),
//         // TODO: recursively check the element type and bounds? for checking the
//         // term, would we just want to make sure that its type is numeric?
//         Type::Vector(Vector {
//             element_type,
//             bounds,
//         }) => Ok(Kind {
//             layout: Param::required(element_type),
//             constraints: Param::optional(bounds),
//         }),
//         Type::Str(Str { bounds }) => Ok(Kind {
//             layout: Param::None,
//             constraints: Param::optional(bounds),
//         }),
//         // TODO: do we want to check that they refer to actual protocols? this isn't really a "kind"
//         // check... but it is a check that the type is valid.
//         Type::ClientEnd(_) => unimplemented!(),
//         Type::ServerEnd(_) => unimplemented!(),
//         Type::TypeSubstitution(TypeSubstitution {
//             func,
//             layout,
//             constraint,
//         }) => {
//             // TODO: make recursive type/term field types consistent
//             // let func_kind = kind_check(func, scope)?;
//             let func_kind: Kind = unimplemented!();

//             // TODO: we want to return both errors if there multiple. currently trying to
//             // sub both a layout and constraint into a concrete type will only error on the layout

//             // NOTE: we error if an argument is provided that is not supported by the func type,
//             // but don't if an argument that is "needed" is not provided (so we provide some
//             // "currying" like behavior to match fidlc's type constructors)
//             Ok(Kind {
//                 layout: func_kind.layout
//                     .take(layout)
//                     .map_err(|_| Error::InvalidTypeParam {
//                         func_call: unimplemented!(),
//                         param: ParamType::Layout,
//                         func_def: unimplemented!()
//                     })?,
//                 constraints: func_kind.constraints
//                     .take(constraint)
//                     .map_err(|_| Error::InvalidTypeParam {
//                         func_call: unimplemented!(),
//                         param: ParamType::Constraint,
//                         func_def: unimplemented!(),
//                     })?,
//             })
//         }
//         _ => Ok(Kind::base_kind()),
//     }
// }

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
