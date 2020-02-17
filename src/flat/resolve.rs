use super::errors::Error;
use super::*;
use crate::flatten::{get_nested_def, ResolverContext, UnresolvedScope};
use crate::lexer::Span;
use crate::raw;
// use crate::raw::Spanned;
use std::collections::HashMap;

impl Library {
    // TODO: return errors
    pub fn from_files(lib_cx: ResolverContext, deps: &Dependencies) -> Result<Library, Vec<Error>> {
        let ResolverContext {
            attributes,
            name,
            defined_names,
            files,
        } = lib_cx;
        let mut terms: TermScope = HashMap::new();
        let mut types: TypeScope = HashMap::new();
        let mut protocols: HashMap<String, Spanned<Protocol>> = HashMap::new();
        let mut services: HashMap<String, Spanned<Service>> = HashMap::new();
        let mut errors: Vec<Error> = Vec::new();
        for file in files {
            // TODO: can we continue here? note that on import duplicates, currently the old one
            // is overwritten in the new one. instead, it should just not exist?
            let mut imports = FileImports::from_imports(file.imports)?;
            let mut resolver = Resolver {
                imports: &mut imports,
                local_names: &defined_names,
                deps,
            };
            for decl in file.decls {
                match decl.value {
                    raw::Decl::Alias(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(types.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.push(errs),
                    },
                    raw::Decl::Const(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(terms.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Struct(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(types.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Bits(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(types.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Enum(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(types.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Table(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(types.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Union(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(types.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Protocol(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(protocols.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                    raw::Decl::Service(val) => match val.resolve(&mut resolver) {
                        Ok((name, entry)) => drop(services.insert(name, decl.span.wrap(entry))),
                        Err(errs) => errors.extend(errs),
                    },
                }
            }
        }

        Ok(Library {
            attributes,
            name,
            terms,
            types,
            protocols,
            services,
        })
    }

    pub fn lookup(&self, name: &String) -> Option<Span> {
        // NOTE: Flattener ensures that each of these scopes are mutually exclusive
        if let Some(entry) = self.terms.get(name) {
            return Some(entry.span);
        }
        if let Some(entry) = self.types.get(name) {
            return Some(entry.span);
        }
        if let Some(entry) = self.protocols.get(name) {
            return Some(entry.span);
        }
        if let Some(entry) = self.services.get(name) {
            return Some(entry.span);
        }
        return None;
    }
}

pub struct Resolver<'a> {
    pub imports: &'a mut FileImports,
    pub local_names: &'a UnresolvedScope,
    pub deps: &'a Dependencies,
}

impl<'a> Resolver<'a> {
    pub fn new(
        imports: &'a mut FileImports,
        local_names: &'a UnresolvedScope,
        deps: &'a Dependencies,
    ) -> Self {
        Resolver {
            imports,
            local_names,
            deps,
        }
    }

    pub fn resolve_term_boxed(
        &mut self,
        sp: Spanned<raw::Term>,
    ) -> Result<Spanned<Box<Term>>, Error> {
        self.resolve_term(sp)
            .map(|spanned| spanned.map(|term| Box::new(term)))
    }

    pub fn resolve_term(&mut self, spanned: Spanned<raw::Term>) -> Result<Spanned<Term>, Error> {
        spanned.try_map(|term| match term {
            raw::Term::Identifier(name) => Ok(Term::Identifier(self.resolve_name(name)?.value)),
            raw::Term::Str(s) => Ok(Term::Str(s)),
            raw::Term::Int(n) => Ok(Term::Int(n)),
            raw::Term::Float(n) => Ok(Term::Float(n)),
            raw::Term::True => Ok(Term::True),
            raw::Term::False => Ok(Term::False),
        })
    }

    pub fn resolve_type_boxed(
        &mut self,
        sp: Spanned<Box<raw::Type>>,
    ) -> Result<Spanned<Box<Type>>, Error> {
        self.resolve_type(sp)
            .map(|spanned| spanned.map(|ty| Box::new(ty)))
    }

    pub fn resolve_type(
        &mut self,
        spanned: Spanned<Box<raw::Type>>,
    ) -> Result<Spanned<Type>, Error> {
        spanned.try_map(|ty| {
            // the type that is being aliased
            let target_name = self.resolve_name(ty.name)?.value;
            let inner = if ty.layout.is_some() || ty.constraint.is_some() {
                Type::TypeSubstitution(TypeSubstitution {
                    func: Box::new(Type::Identifier(target_name)),
                    // TODO: allow return possibly both errors
                    layout: ty
                        .layout
                        .map(|layout| self.resolve_type_boxed(layout))
                        .transpose()?,
                    constraint: ty
                        .constraint
                        .map(|constraint| self.resolve_term_boxed(constraint))
                        .transpose()?,
                })
            } else {
                Type::Identifier(target_name)
            };
            if ty.nullable {
                Ok(Type::Ptr(Box::new(inner)))
            } else {
                Ok(inner)
            }
        })
    }

    pub fn resolve_name(&mut self, name: raw::CompoundIdentifier) -> Result<Spanned<Name>, Error> {
        let span = name.span;
        let name = name.value;
        match name.len() {
            1 => {
                // this must be referring to a top level value in the local context
                let name = name.into_iter().next().unwrap();
                if self.local_names.contains_key(&name) {
                    Ok(span.wrap(Name {
                        library: None,
                        name,
                        member: None,
                    }))
                } else {
                    Err(Error::Undefined(span))
                }
            }
            2 => {
                // if there are two components a.b, this can refer to two things:
                // decl b in library a (`dep_value`), or member b of decl a in the
                // local library (`local_value`).
                let local_value = {
                    let var = name.first().unwrap();
                    let member = name.last().unwrap();
                    match get_nested_def(self.local_names, var, member) {
                        Some(span) => Some((
                            span,
                            Name {
                                library: None,
                                name: var.clone(),
                                member: Some(member.clone()),
                            },
                        )),
                        _ => None,
                    }
                };
                let dep_value = {
                    let lib_name = name.first().unwrap();
                    let var = name.last().unwrap();
                    match self.dep_lookup(lib_name, var) {
                        Some(span) => Some((
                            span,
                            Name {
                                library: Some(lib_name.clone()),
                                name: var.clone(),
                                member: None,
                            },
                        )),
                        _ => None,
                    }
                };
                match (local_value, dep_value) {
                    (Some((local_span, local_name)), Some((dep_span, dep_name))) => {
                        Err(Error::AmbiguousReference {
                            span,
                            interp1: local_span.wrap(local_name),
                            interp2: dep_span.wrap(dep_name),
                        })
                    }
                    (Some((_, local_name)), None) => Ok(span.wrap(local_name)),
                    (None, Some((_, dep_name))) => Ok(span.wrap(dep_name)),
                    (None, None) => Err(Error::Undefined(span)),
                }
            }
            _ => unimplemented!(),
        }
    }

    // any name lookup in the dependencies should go through these methods, so that
    // import usage can be tracked
    fn dep_lookup(&mut self, lib_name: &String, var: &String) -> Option<Span> {
        let lib_name = self.imports.get_absolute(lib_name);
        self.imports.mark_used(&lib_name);

        self.deps.lookup(&lib_name, var)
    }

    // fn dep_lookup_nested(&self, _lib_name: &String, _name: &String, _member: &String) -> Option<Span> {
    //     unimplemented!()
    // }
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

    fn lookup(&self, lib_name: &String, var: &String) -> Option<Span> {
        self.libraries.get(lib_name).and_then(|lib| lib.lookup(var))
    }
}

#[derive(Debug)]
pub struct FileImports {
    /// Each full import has an entry in `imports` with a value of None, and each
    /// aliased import has an entry in `imports` with a value of Some of the full
    /// import that it's aliasing.
    // TODO: this needs many copies of each import string. we could do better at
    // the expense of more searching (e.g. a Vector of absolute imports, and a map
    // from alias to index in that owning vector)
    imports: HashMap<String, Option<String>>,
}

// TODO: check that imports are in deps, and keep track of which imports are used.
// would be better to do this once we have a better idea of what Depedencies looks
// like. we could check that imports actually exist "lazily" (i.e. whenever they are
// used), and this would give the benefit that if it's not actually used we'd error with
// unused imoprt instead of invalid import. but when we error, we want the import span
// and we only have that here, so FileImports must be responsible for it.
impl FileImports {
    // NOTE: this only takes in raw:: data, so technically this can be done during the File
    // "validation" step as well. but conceptually it belongs here and this at least avoids some
    // duplicate work. Alternatively, validation could transform the File.
    // TODO: does it even make sense to return multiple errors here?
    pub fn from_imports(imports: Vec<Spanned<raw::Import>>) -> Result<FileImports, Vec<Error>> {
        let mut import_map: HashMap<String, (Span, Option<String>)> = HashMap::new();
        let mut errors: Vec<Error> = Vec::new();
        for import in imports {
            let absolute_name = import.value.name.value.join(".");
            // add the absolute import
            let span = import.value.name.span;
            if let Some((entry_span, entry)) =
                import_map.insert(absolute_name.clone(), (span, None))
            {
                errors.push(match entry {
                    Some(name) => Error::ImportNameConflict {
                        name: name.clone(),
                        orig: entry_span,
                        dupe: span,
                    },
                    None => Error::DuplicateImport {
                        import: absolute_name.clone(),
                        orig: entry_span,
                        dupe: span,
                    },
                });
                continue;
            }

            // add the alias
            if let Some(alias) = import.value.alias {
                let span = alias.span;
                let name = alias.value;
                if let Some((entry_span, _)) =
                    import_map.insert(name.clone(), (span, Some(absolute_name.clone())))
                {
                    errors.push(Error::ImportNameConflict {
                        name: name,
                        orig: entry_span,
                        dupe: span,
                    })
                }
            }
        }
        if errors.is_empty() {
            let without_spans = import_map
                .into_iter()
                .map(|(k, v)| (k, v.1))
                .collect::<HashMap<_, _>>();
            Ok(FileImports {
                imports: without_spans,
            })
        } else {
            Err(errors)
        }
    }

    // TODO: unecessary copying
    pub fn get_absolute(&self, import: &String) -> String {
        match self.imports.get(import) {
            Some(Some(name)) => name.clone(),
            _ => import.clone(),
        }
    }

    pub fn mark_used(&mut self, _absolute_import: &String) {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;
    use crate::source_file::SourceFile;
    use crate::span::FileId;

    #[test]
    fn import_success() {
        let contents = r#"library test;
using foo;
using bar;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        assert_eq!(FileImports::from_imports(file.imports).is_ok(), true);
    }

    #[test]
    fn import_dupe_no_alias() {
        let contents = r#"library test;
using foo;
using foo;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        let errs = FileImports::from_imports(file.imports).unwrap_err();
        assert_eq!(errs.len(), 1);
        match errs[0] {
            Error::DuplicateImport {
                import: _,
                orig: _,
                dupe: _,
            } => (),
            _ => assert!(false),
        }
    }

    #[test]
    fn import_dupe_aliases() {
        let contents = r#"library test;
using foo as bar;
using foo as baz;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        let errs = FileImports::from_imports(file.imports).unwrap_err();
        assert_eq!(errs.len(), 1);
        match errs[0] {
            Error::DuplicateImport {
                import: _,
                orig: _,
                dupe: _,
            } => (),
            _ => assert!(false),
        }
    }

    #[test]
    fn import_conflict() {
        let contents = r#"library test;
using foo;
using bar as foo;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        let errs = FileImports::from_imports(file.imports).unwrap_err();
        assert_eq!(errs.len(), 1);
        match errs[0] {
            Error::ImportNameConflict {
                name: _,
                orig: _,
                dupe: _,
            } => (),
            _ => assert!(false),
        }
    }
}
