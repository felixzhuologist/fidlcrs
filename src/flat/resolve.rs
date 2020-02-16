use super::errors::Error;
use super::*;
use crate::flatten::{ResolverContext, UnresolvedScope};
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
        let mut protocols: HashMap<String, Protocol> = HashMap::new();
        let mut services: HashMap<String, Service> = HashMap::new();
        let mut _errors: Vec<Error> = Vec::new();
        for file in files {
            // TODO: can we continue here? note that on import duplicates, currently the old one
            // is overwritten in the new one. instead, it should just not exist?
            let imports = FileImports::from_imports(file.imports)?;
            let resolver = Resolver {
                imports: &imports,
                local_names: &defined_names,
                imported_names: deps,
            };
            for decl in file.decls {
                match decl.value {
                    raw::Decl::Alias(raw::Alias {
                        attributes,
                        name,
                        ty,
                    }) => {
                        let resolved = resolver.resolve_type(ty);
                        types.insert(name.value, (attributes, resolved));
                    }
                    raw::Decl::Const(raw::Const {
                        attributes,
                        ty,
                        name,
                        value,
                    }) => {
                        let ty = resolver.resolve_type(ty);
                        let term = resolver.resolve_term(value);
                        terms.insert(name.value, (attributes, ty, term));
                    }
                    raw::Decl::Struct(_) => unimplemented!(),
                    raw::Decl::Bits(_) => unimplemented!(),
                    raw::Decl::Enum(_) => unimplemented!(),
                    raw::Decl::Table(_) => unimplemented!(),
                    raw::Decl::Union(_) => unimplemented!(),
                    raw::Decl::Protocol(_) => unimplemented!(),
                    raw::Decl::Service(_) => unimplemented!(),
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

    // pub fn lookup(&self, _var_name: String) -> Option<Spanned<Decl>> {
    //     unimplemented!()
    // }
}

pub struct Resolver<'a> {
    pub imports: &'a FileImports,
    pub local_names: &'a UnresolvedScope,
    pub imported_names: &'a Dependencies,
}

impl<'a> Resolver<'a> {
    pub fn new(
        imports: &'a FileImports,
        local_names: &'a UnresolvedScope,
        imported_names: &'a Dependencies,
    ) -> Self {
        Resolver {
            imports,
            local_names,
            imported_names,
        }
    }

    fn resolve_term(&self, term: Spanned<raw::Term>) -> Term {
        match term.value {
            raw::Term::Identifier(name) => Term::Identifier(self.resolve_name(name)),
            // TODO: make literal inline in Term, so that raw and flat are the
            // same?
            _ => unimplemented!(),
        }
    }

    fn resolve_type(&self, ty: Spanned<Box<raw::Type>>) -> Type {
        // the type that is being aliased
        let target_name = self.resolve_name(ty.value.name);
        let inner = if ty.value.layout.is_some() || ty.value.constraint.is_some() {
            Type::TypeSubstitution(TypeSubstitution {
                func: Box::new(Type::Identifier(target_name)),
                layout: unimplemented!(),
                constraint: unimplemented!(),
                // layout: ty.value.layout.map(|layout| self.resolve_type(layout)),
                // constraint: ty.value.constraint.map(|constraint| self.resolve_term(constraint)),
            })
        } else {
            Type::Identifier(target_name)
        };
        if ty.value.nullable {
            Type::Ptr(Box::new(inner))
        } else {
            inner
        }
    }

    // fn resolve_name(&self, name: raw::CompoundIdentifier) -> Result<Name, Error> {
    fn resolve_name(&self, name: raw::CompoundIdentifier) -> Name {
        unimplemented!()
        // let span = name.span;
        // let name = name.value;
        // match name.len() {
        //     1 => {
        //         // this must be referring to something in the local context
        //         let name = name.into_iter().next().unwrap();
        //         if self.local_names.contains_key(&name) {
        //             Ok(Name {
        //                 library: None,
        //                 name,
        //                 member: None,
        //             })
        //         } else {
        //             Err(Error::UnresolvedLocal(span))
        //         }
        //     }
        //     2 => {
        //         // if there are two components a.b, this can refer to two things:
        //         // decl b in library a, or member b of decl a in the local library.
        //         // TODO: the local names will need to keep track of existing member
        //         // names too...
        //         unimplemented!()
        //     }
        //     _ => unimplemented!(),
        // }
    }
}

//     // This should really return an Either - the Error is just stored somewhere and doesn't
//     // actually indicate a failure. Also note that this return type means that only the
//     // first error will be returned when resolving a decl. Being able to use ? makes things
//     // way simpler for now
//     pub fn resolve(&self, decl: raw::Decl) -> Result<Decl, Error> {
//         // TODO: refactor this to use the visitor pattern? this looks horrible but all it's doing
//         // is travering the tree and colling resolve_type, resolve_value, or resolve_name on the
//         // types, values, and names that it's encountering.
//         match decl {
//             raw::Decl::Alias(decl) => Ok(Decl::Alias(Alias {
//                 attributes: decl.attributes,
//                 name: decl.name,
//                 ty: self.resolve_type(decl.ty)?,
//             })),
//             raw::Decl::Const(decl) => Ok(Decl::Const(ConstDecl {
//                 attributes: decl.attributes,
//                 ty: self.resolve_type(decl.ty)?,
//                 name: decl.name,
//                 value: self.resolve_value(decl.value)?,
//             })),
//             raw::Decl::Struct(decl) => {
//                 let members: Result<Vec<_>, _> = decl
//                     .members
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|member| {
//                             Ok(StructMember {
//                                 attributes: member.attributes,
//                                 ty: self.resolve_type(member.ty)?,
//                                 name: member.name,
//                                 default_value: match member.default_value {
//                                     Some(val) => Some(self.resolve_value(val)?),
//                                     None => None,
//                                 },
//                             })
//                         })
//                     })
//                     .collect();
//                 Ok(Decl::Struct(Struct {
//                     attributes: decl.attributes,
//                     name: decl.name,
//                     members: members?,
//                 }))
//             }
//             raw::Decl::Bits(decl) => {
//                 let members: Result<Vec<_>, _> = decl
//                     .members
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|member| {
//                             Ok(BitsMember {
//                                 attributes: member.attributes,
//                                 name: member.name,
//                                 value: self.resolve_value(member.value)?,
//                             })
//                         })
//                     })
//                     .collect();
//                 Ok(Decl::Bits(Bits {
//                     attributes: decl.attributes,
//                     strictness: decl.strictness,
//                     ty: match decl.ty {
//                         Some(ty) => Some(self.resolve_type(ty)?),
//                         None => None,
//                     },
//                     name: decl.name,
//                     members: members?,
//                 }))
//             }
//             raw::Decl::Enum(decl) => {
//                 let members: Result<Vec<_>, _> = decl
//                     .members
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|member| {
//                             Ok(EnumMember {
//                                 attributes: member.attributes,
//                                 name: member.name,
//                                 value: self.resolve_value(member.value)?,
//                             })
//                         })
//                     })
//                     .collect();
//                 Ok(Decl::Enum(Enum {
//                     attributes: decl.attributes,
//                     strictness: decl.strictness,
//                     ty: match decl.ty {
//                         Some(ty) => Some(self.resolve_type(ty)?),
//                         None => None,
//                     },
//                     name: decl.name,
//                     members: members?,
//                 }))
//             }
//             raw::Decl::Table(decl) => {
//                 let members: Result<Vec<_>, _> = decl
//                     .members
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|member| {
//                             Ok(TableMember {
//                                 attributes: member.attributes,
//                                 ordinal: member.ordinal,
//                                 inner: match member.inner {
//                                     raw::TableMemberInner::Reserved => TableMemberInner::Reserved,
//                                     raw::TableMemberInner::Used {
//                                         ty,
//                                         name,
//                                         default_value,
//                                     } => TableMemberInner::Used {
//                                         ty: self.resolve_type(ty)?,
//                                         name: name,
//                                         default_value: match default_value {
//                                             Some(val) => Some(self.resolve_value(val)?),
//                                             None => None,
//                                         },
//                                     },
//                                 },
//                             })
//                         })
//                     })
//                     .collect();
//                 Ok(Decl::Table(Table {
//                     attributes: decl.attributes,
//                     strictness: decl.strictness,
//                     name: decl.name,
//                     members: members?,
//                 }))
//             }
//             raw::Decl::Union(decl) => {
//                 let members: Result<Vec<_>, _> = decl
//                     .members
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|member| {
//                             Ok(UnionMember {
//                                 attributes: member.attributes,
//                                 ordinal: member.ordinal,
//                                 inner: match member.inner {
//                                     raw::UnionMemberInner::Reserved => UnionMemberInner::Reserved,
//                                     raw::UnionMemberInner::Used { ty, name } => {
//                                         UnionMemberInner::Used {
//                                             ty: self.resolve_type(ty)?,
//                                             name: name,
//                                         }
//                                     }
//                                 },
//                             })
//                         })
//                     })
//                     .collect();
//                 Ok(Decl::Union(Union {
//                     attributes: decl.attributes,
//                     strictness: decl.strictness,
//                     name: decl.name,
//                     members: members?,
//                 }))
//             }
//             raw::Decl::Protocol(decl) => {
//                 let methods: Result<Vec<_>, _> = decl
//                     .methods
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|method| {
//                             let request = match method.request {
//                                 None => None,
//                                 Some(req) => {
//                                     let params: Result<Vec<_>, _> = req
//                                         .into_iter()
//                                         .map(|spanned| {
//                                             spanned.try_map(|param| {
//                                                 Ok(Parameter {
//                                                     attributes: param.attributes,
//                                                     name: param.name,
//                                                     ty: self.resolve_type(param.ty)?,
//                                                 })
//                                             })
//                                         })
//                                         .collect();
//                                     Some(params?)
//                                 }
//                             };
//                             let response = match method.response {
//                                 None => None,
//                                 Some(resp) => {
//                                     let params: Result<Vec<_>, _> = resp
//                                         .into_iter()
//                                         .map(|spanned| {
//                                             spanned.try_map(|param| {
//                                                 Ok(Parameter {
//                                                     attributes: param.attributes,
//                                                     name: param.name,
//                                                     ty: self.resolve_type(param.ty)?,
//                                                 })
//                                             })
//                                         })
//                                         .collect();
//                                     Some(params?)
//                                 }
//                             };
//                             let error = match method.error {
//                                 None => None,
//                                 Some(err) => Some(self.resolve_type(err)?),
//                             };
//                             Ok(Method {
//                                 attributes: method.attributes,
//                                 name: method.name,
//                                 request,
//                                 response,
//                                 error,
//                             })
//                         })
//                     })
//                     .collect();
//                 let compose: Result<Vec<_>, _> = decl
//                     .compose
//                     .into_iter()
//                     .map(|ident| self.resolve_name(ident))
//                     .collect();
//                 Ok(Decl::Protocol(Protocol {
//                     attributes: decl.attributes,
//                     name: decl.name,
//                     methods: methods?,
//                     compose: compose?,
//                 }))
//             }
//             raw::Decl::Service(decl) => {
//                 let members: Result<Vec<_>, _> = decl
//                     .members
//                     .into_iter()
//                     .map(|spanned| {
//                         spanned.try_map(|member| {
//                             Ok(ServiceMember {
//                                 attributes: member.attributes,
//                                 protocol: self.resolve_name(member.protocol)?,
//                                 name: member.name,
//                             })
//                         })
//                     })
//                     .collect();
//                 Ok(Decl::Service(Service {
//                     attributes: decl.attributes,
//                     name: decl.name,
//                     members: members?,
//                 }))
//             }
//         }
//     }

//     fn resolve_type(&self, ty: Spanned<Box<raw::Type>>) -> Result<Spanned<TypeRef>, Error> {
//         unimplemented!()
//     }

//     fn resolve_value(&self, val: Spanned<raw::Term>) -> Result<Spanned<ConstVal>, Error> {
//         unimplemented!()
//     }

// }

// TODO: this will be its own module once it's fleshed out some more
#[derive(Default)]
pub struct Dependencies {
    libraries: HashMap<String, Library>,
}

impl Dependencies {
    pub fn add_library(&mut self, _lib: Library) {
        unimplemented!()
    }

    // fn lookup(&self, lib_name: &str, var_name: String) -> Option<Spanned<Decl>> {
    //     self.libraries
    //         .get(lib_name)
    //         .and_then(|lib| lib.lookup(var_name))
    // }
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
