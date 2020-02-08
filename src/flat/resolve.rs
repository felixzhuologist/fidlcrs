use super::errors::Error;
use super::*;
use crate::lexer::Span;
use crate::raw;
use crate::raw::{File, LibraryName, Spanned};
use std::collections::HashMap;

impl Library {
    pub fn from_files(
        &self,
        files: Vec<File>,
        lib_names: &HashMap<String, Span>,
        deps: &Dependencies,
    ) -> Library {
        // TODO: check these in the previous step.
        let mut attributes: Vec<Spanned<Attribute>> = Vec::new();
        let name: LibraryName = files[0].name.clone();

        let mut decls: HashMap<String, Spanned<Decl>> = HashMap::new();
        let mut errors: Vec<Error> = Vec::new();
        for file in files {
            attributes.extend(file.attributes);
            for decl in file.decls {
                match decl.try_map(|val| resolve(val, lib_names, deps)) {
                    Ok(decl) => {
                        let result = decls.insert(decl.value.name().clone(), decl);
                        if result.is_some() {
                            panic!("did not correctly check for duplicates");
                        }
                    }
                    Err(err) => errors.push(err),
                };
            }
        }

        Library {
            attributes,
            name,
            decls,
        }
    }

    pub fn lookup(&self, var_name: String) -> Option<Spanned<Decl>> {
        unimplemented!()
    }
}

pub fn resolve(
    decl: raw::Decl,
    local_names: &HashMap<String, Span>,
    dep_names: &Dependencies,
) -> Result<Decl, Error> {
    unimplemented!()
}

// TODO: this will be its own module once it's fleshed out some more
pub struct Dependencies {
    libraries: HashMap<String, Library>,
}

impl Dependencies {
    fn lookup(&self, lib_name: &str, var_name: String) -> Option<Spanned<Decl>> {
        self.libraries
            .get(lib_name)
            .and_then(|lib| lib.lookup(var_name))
    }
}
