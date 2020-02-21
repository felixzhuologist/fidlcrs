//! Once everything is resolved, this does validation of the FIDL file, including things like
//! type and kind checking
use crate::flat;
use crate::flat::resolve::Libraries;
use crate::flat::Library;

// TODO: go in separate module?
pub enum Error {}

pub struct Validator<'a> {
    lib: &'a Library,
    deps: &'a Libraries,
}

impl<'a> Validator<'a> {
    pub fn new(lib: &'a Library, deps: &'a Libraries) -> Self {
        Validator { lib, deps }
    }

    pub fn validate(&self) {
        for _term in self.lib.terms.values() {
            // term.validate(self);
        }
        for _ty in self.lib.types.values() {
            // ty.validate(self);
        }
        for _protocol in self.lib.protocols.values() {
            // protocol.validate(self);
        }
        for _service in self.lib.services.values() {
            // service.validate(self);
        }
    }
}

impl flat::Term {
    pub fn validate(&self, _validator: &Validator) {
        unimplemented!()
    }
}

impl flat::Type {
    pub fn validate(&self, _validator: &Validator) {
        unimplemented!()
    }
}

impl flat::Protocol {
    pub fn validate(&self, _validator: &Validator) {
        unimplemented!()
    }
}

impl flat::Service {
    pub fn validate(&self, _validator: &Validator) {
        unimplemented!()
    }
}
