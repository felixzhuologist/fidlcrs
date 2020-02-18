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
            let mut imports = FileImports::from_imports(file.imports, &deps)?;
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
            errors.extend(imports.get_unused_imports());
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

    // used for testing
    pub fn empty(name: String) -> Library {
        let mut lib = Library::default();
        lib.name = name;
        lib
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

    pub fn lookup_nested(&self, name: &String, member: &String) -> Option<Span> {
        match self.types.get(name) {
            Some(entry) => {
                if let Type::Bits(bits) = &entry.value.1.value {
                    return bits.lookup(member);
                }
                if let Type::Enum(num) = &entry.value.1.value {
                    return num.lookup(member);
                }
                return None;
            }
            None => return None,
        }
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
            0 => panic!("cant have an empty name"),
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
                    match self.dep_lookup(lib_name, var, None) {
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
                resolve_names(span, local_value, dep_value)
            }
            _ => {
                // if there are more than two components, this can't refer to a local value.
                // it must either refer to a member or top level value in a dependency
                let member_val = {
                    let member = &name[name.len() - 1];
                    let var = &name[name.len() - 2];
                    let library = name[..name.len() - 2].join(".");
                    match self.dep_lookup(&library, var, Some(member)) {
                        Some(span) => Some((
                            span,
                            Name {
                                library: Some(library),
                                name: var.clone(),
                                member: Some(member.clone()),
                            },
                        )),
                        _ => None,
                    }
                };
                let toplevel_val = {
                    let var = &name[name.len() - 1];
                    let library = name[..name.len() - 1].join(".");
                    match self.dep_lookup(&library, &var, None) {
                        Some(span) => Some((
                            span,
                            Name {
                                library: Some(library),
                                name: var.clone(),
                                member: None,
                            },
                        )),
                        _ => None,
                    }
                };
                resolve_names(span, member_val, toplevel_val)
            }
        }
    }

    // any name lookup in the dependencies should go through these methods, so that
    // import usage can be tracked
    fn dep_lookup(
        &mut self,
        lib_name: &String,
        var: &String,
        member: Option<&String>,
    ) -> Option<Span> {
        let lib_name = self.imports.get_absolute(lib_name);
        self.imports.mark_used(&lib_name);

        self.deps.lookup(&lib_name, var, member)
    }
}

// TODO: this will be its own module once it's fleshed out some more
#[derive(Default)]
pub struct Dependencies {
    libraries: HashMap<String, Library>,
}

impl Dependencies {
    pub fn contains_library(&self, library: &String) -> bool {
        self.libraries.contains_key(library)
    }

    pub fn add_library(&mut self, lib: Library) -> Result<(), Error> {
        if let Some(_) = self.libraries.insert(lib.name.clone(), lib) {
            // TODO: attach more information to this error?
            Err(Error::DuplicateLibrary)
        } else {
            Ok(())
        }
    }

    fn lookup(&self, lib_name: &String, var: &String, member: Option<&String>) -> Option<Span> {
        self.libraries.get(lib_name).and_then(|lib| match member {
            Some(member) => lib.lookup_nested(var, member),
            None => lib.lookup(var),
        })
    }
}

// TODO: this needs many copies of each import string. we could do better at
// the expense of more searching (e.g. a Vector of absolute imports, and a map
// from alias to index in that owning vector)
#[derive(Debug)]
pub struct FileImports {
    /// A map from alias to the full library name.
    aliases: HashMap<String, String>,
    /// A map from full library name to whether this import is actually used.
    imports: HashMap<String, Spanned<bool>>,
}

// TODO: check that imports are in deps, and keep track of which imports are used.
// would be better to do this once we have a better idea of what Depedencies looks
// like. we could check that imports actually exist "lazily" (i.e. whenever they are
// used), and this would give the benefit that if it's not actually used we'd error with
// unused imoprt instead of invalid import. but when we error, we want the import span
// and we only have that here, so FileImports must be responsible for it.
impl FileImports {
    // TODO: does it even make sense to return multiple errors here?
    // TODO: now that self.imports stores the spans, we could check the imports against
    // the Dependencies separately, and make from_imports only take one parameter (which
    // would reduce test boilerplate)
    pub fn from_imports(
        imports: Vec<Spanned<raw::Import>>,
        deps: &Dependencies,
    ) -> Result<FileImports, Vec<Error>> {
        // map from import name (either absolute or alias) to a tuple of (its span,
        // and if it's an alias, the full name of the library it aliases). it's easier
        // to use a single hashmap when checking for duplicate imports or name conflicts.
        let mut import_map: HashMap<String, (Span, Option<String>)> = HashMap::new();
        let mut errors: Vec<Error> = Vec::new();

        // check for duplicate imports and name conflicts
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

        for (import, (span, maybe_full_name)) in &import_map {
            match maybe_full_name {
                None if !deps.contains_library(import) => {
                    errors.push(Error::DependencyNotFound(*span));
                }
                _ => (),
            }
        }

        if errors.is_empty() {
            // TODO: is it possible to parition without collecting?
            let (full_imports, aliases): (Vec<_>, Vec<_>) =
                import_map.into_iter().partition(|(_, v)| v.1.is_none());

            let full_imports = full_imports
                .into_iter()
                .map(|(k, v)| (k, v.0.wrap(false)))
                .collect::<HashMap<_, _>>();
            let aliases = aliases
                .into_iter()
                .map(|(k, v)| (k, v.1.unwrap()))
                .collect::<HashMap<_, _>>();

            Ok(FileImports {
                aliases,
                imports: full_imports,
            })
        } else {
            Err(errors)
        }
    }

    // TODO: unecessary copying
    // TODO: this succeeds for imports that do not exist in a using statement,
    // and just returns a copy of the import back. whether this should return an
    // Error or not will depend on whether we care about distinguishing between
    // undefined because of an import not being imported, or because the variable doesn't
    // exist. the third possible kind of failure, an import not being found is
    // checked when resolving the imports (i.e. on construction of FileImports)
    pub fn get_absolute(&self, import: &String) -> String {
        match self.aliases.get(import) {
            Some(name) => name.clone(),
            _ => import.clone(),
        }
    }

    pub fn mark_used(&mut self, absolute_import: &String) {
        if let Some(is_used) = self.imports.get_mut(absolute_import) {
            is_used.value = true;
        }
    }

    pub fn get_unused_imports(&self) -> Vec<Error> {
        self.imports
            .iter()
            .filter_map(|(_, is_used)| {
                if is_used.value {
                    Some(Error::UnusedImport(is_used.span))
                } else {
                    None
                }
            })
            .collect()
    }
}

fn resolve_names(
    span: Span,
    interp1: Option<(Span, Name)>,
    interp2: Option<(Span, Name)>,
) -> Result<Spanned<Name>, Error> {
    match (interp1, interp2) {
        (Some((span1, name1)), Some((span2, name2))) => Err(Error::AmbiguousReference {
            span,
            interp1: span1.wrap(name1),
            interp2: span2.wrap(name2),
        }),
        (Some((_, name1)), None) => Ok(span.wrap(name1)),
        (None, Some((_, name2))) => Ok(span.wrap(name2)),
        (None, None) => Err(Error::Undefined(span)),
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
        let mut deps = Dependencies::default();
        deps.add_library(Library::empty("foo".to_string())).unwrap();
        deps.add_library(Library::empty("bar".to_string())).unwrap();
        let contents = r#"library test;
using foo;
using bar;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        assert_eq!(FileImports::from_imports(file.imports, &deps).is_ok(), true);
    }

    #[test]
    fn import_dupe_no_alias() {
        let mut deps = Dependencies::default();
        deps.add_library(Library::empty("foo".to_string())).unwrap();
        let contents = r#"library test;
using foo;
using foo;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        let errs = FileImports::from_imports(file.imports, &deps).unwrap_err();
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
        let mut deps = Dependencies::default();
        deps.add_library(Library::empty("foo".to_string())).unwrap();
        let contents = r#"library test;
using foo as bar;
using foo as baz;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        let errs = FileImports::from_imports(file.imports, &deps).unwrap_err();
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
        let mut deps = Dependencies::default();
        deps.add_library(Library::empty("foo".to_string())).unwrap();
        deps.add_library(Library::empty("bar".to_string())).unwrap();
        let contents = r#"library test;
using foo;
using bar as foo;
"#;
        let src = SourceFile::new(FileId(0), "test.fidl".to_string(), contents.to_string());
        let file = parse(&src).unwrap();
        let errs = FileImports::from_imports(file.imports, &deps).unwrap_err();
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
