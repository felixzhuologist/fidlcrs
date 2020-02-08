use super::errors::Error;
use super::*;
use crate::flatten::ResolverContext;
use crate::lexer::Span;
use crate::raw;
use crate::raw::Spanned;
use crate::source_file::FileId;
use std::collections::HashMap;

impl Library {
    // TODO: return errors
    pub fn from_files(lib_cx: ResolverContext, deps: &Dependencies) -> Library {
        let ResolverContext {
            attributes,
            name,
            defined_names,
            files,
        } = lib_cx;

        let mut decls: HashMap<String, Spanned<Decl>> = HashMap::new();
        let mut errors: Vec<Error> = Vec::new();
        // TODO: improve nesting
        for file in files {
            let maybe_imports = ImportContext::from_imports(file.imports);
            match maybe_imports {
                Err(errs) => {
                    errors.extend(errs);
                    continue;
                }
                Ok(ctx) => {
                    for decl in file.decls {
                        match decl.try_map(|val| resolve(val, &ctx, &defined_names, deps)) {
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
            }
        }

        Library {
            attributes,
            name,
            decls,
        }
    }

    pub fn lookup(&self, _var_name: String) -> Option<Spanned<Decl>> {
        unimplemented!()
    }
}

pub fn resolve(
    _decl: raw::Decl,
    _ctx: &ImportContext,
    _local_names: &HashMap<String, (Span, FileId)>,
    _dep_names: &Dependencies,
) -> Result<Decl, Error> {
    unimplemented!()
}

// TODO: this will be its own module once it's fleshed out some more
#[derive(Default)]
pub struct Dependencies {
    libraries: HashMap<String, Library>,
}

impl Dependencies {
    pub fn add_library(&mut self, _lib: Library) {
        unimplemented!()
    }

    fn lookup(&self, lib_name: &str, var_name: String) -> Option<Spanned<Decl>> {
        self.libraries
            .get(lib_name)
            .and_then(|lib| lib.lookup(var_name))
    }
}

pub struct ImportContext {}

impl ImportContext {
    pub fn from_imports(_imports: Vec<Spanned<raw::Import>>) -> Result<ImportContext, Vec<Error>> {
        unimplemented!()
    }
}
