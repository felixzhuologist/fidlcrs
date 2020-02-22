//! Once everything is resolved, this does validation of the FIDL file, including things like
//! type and kind checking
use crate::flat;
use crate::flat::Libraries;

// TODO: go in separate module?
pub enum Error {}

impl Libraries {
    pub fn validate_latest(&self) {
        let lib = self.libraries.last().unwrap();
        for _term in lib.terms.values() {
            // term.validate(self);
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
}

impl flat::Term {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}

impl flat::Type {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}

impl flat::Protocol {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}

impl flat::Service {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}
