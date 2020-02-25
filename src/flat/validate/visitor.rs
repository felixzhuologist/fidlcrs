use super::errors::Error;
use crate::flat::*;

impl Term {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}

impl Type {
    pub fn validate(&self, _scope: &Libraries) -> Result<(), Vec<Error>> {
        //     match self {
        //         Type::Struct(Struct { ref members }) => {
        //             let mut errors = Vec::new();
        //             for member in members {
        //                 if
        //             }
        //         },
        //         Type::Bits(_) => unimplemented!(),
        //         Type::Enum(_) => unimplemented!(),
        //         Type::Table(_) => unimplemented!(),
        //         Type::Union(_) => unimplemented!(),
        //         Type::Ptr(_) => unimplemented!(),
        //         Type::ClientEnd(_) | Type::ServerEnd(_) => unimplemented!(),
        //         _ => Ok(()),
        //     }
        unimplemented!()
    }
}

impl Protocol {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}

impl Service {
    pub fn validate(&self, _scope: &Libraries) {
        unimplemented!()
    }
}
