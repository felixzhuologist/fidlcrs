use super::errors::Error;
use super::*;
use crate::flatten::ResolverContext;
use crate::lexer::Span;
use crate::raw;
use crate::raw::Spanned;
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
                    let resolver = Resolver::new(&ctx, &defined_names, deps);
                    for decl in file.decls {
                        match decl.try_map(|val| resolver.resolve(val)) {
                            Ok(decl) => {
                                let result = decls.insert(decl.value.name(), decl);
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

pub struct Resolver<'a> {
    pub import_cx: &'a ImportContext,
    pub local_names: &'a HashMap<String, Span>,
    pub dep_names: &'a Dependencies,
}

impl<'a> Resolver<'a> {
    pub fn new(
        import_cx: &'a ImportContext,
        local_names: &'a HashMap<String, Span>,
        dep_names: &'a Dependencies,
    ) -> Self {
        Resolver {
            import_cx,
            local_names,
            dep_names,
        }
    }

    // This should really return an Either - the Error is just stored somewhere and doesn't
    // actually indicate a failure. Also note that this return type means that only the
    // first error will be returned when resolving a decl. Being able to use ? makes things
    // way simpler for now
    pub fn resolve(&self, decl: raw::Decl) -> Result<Decl, Error> {
        // TODO: refactor this to use the visitor pattern? this looks horrible but all it's doing
        // is travering the tree and colling resolve_type, resolve_value, or resolve_name on the
        // types, values, and names that it's encountering.
        match decl {
            raw::Decl::Alias(decl) => Ok(Decl::Alias(Alias {
                attributes: decl.attributes,
                name: decl.name,
                ty: self.resolve_type(decl.ty)?,
            })),
            raw::Decl::Const(decl) => Ok(Decl::Const(ConstDecl {
                attributes: decl.attributes,
                ty: self.resolve_type(decl.ty)?,
                name: decl.name,
                value: self.resolve_value(decl.value)?,
            })),
            raw::Decl::Struct(decl) => {
                let members: Result<Vec<_>, _> = decl
                    .members
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|member| {
                            Ok(StructMember {
                                attributes: member.attributes,
                                ty: self.resolve_type(member.ty)?,
                                name: member.name,
                                default_value: match member.default_value {
                                    Some(val) => Some(self.resolve_value(val)?),
                                    None => None,
                                },
                            })
                        })
                    })
                    .collect();
                Ok(Decl::Struct(Struct {
                    attributes: decl.attributes,
                    name: decl.name,
                    members: members?,
                }))
            }
            raw::Decl::Bits(decl) => {
                let members: Result<Vec<_>, _> = decl
                    .members
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|member| {
                            Ok(BitsMember {
                                attributes: member.attributes,
                                name: member.name,
                                value: self.resolve_value(member.value)?,
                            })
                        })
                    })
                    .collect();
                Ok(Decl::Bits(Bits {
                    attributes: decl.attributes,
                    strictness: decl.strictness,
                    ty: match decl.ty {
                        Some(ty) => Some(self.resolve_type(ty)?),
                        None => None,
                    },
                    name: decl.name,
                    members: members?,
                }))
            }
            raw::Decl::Enum(decl) => {
                let members: Result<Vec<_>, _> = decl
                    .members
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|member| {
                            Ok(EnumMember {
                                attributes: member.attributes,
                                name: member.name,
                                value: self.resolve_value(member.value)?,
                            })
                        })
                    })
                    .collect();
                Ok(Decl::Enum(Enum {
                    attributes: decl.attributes,
                    strictness: decl.strictness,
                    ty: match decl.ty {
                        Some(ty) => Some(self.resolve_type(ty)?),
                        None => None,
                    },
                    name: decl.name,
                    members: members?,
                }))
            }
            raw::Decl::Table(decl) => {
                let members: Result<Vec<_>, _> = decl
                    .members
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|member| {
                            Ok(TableMember {
                                attributes: member.attributes,
                                ordinal: member.ordinal,
                                inner: match member.inner {
                                    raw::TableMemberInner::Reserved => TableMemberInner::Reserved,
                                    raw::TableMemberInner::Used {
                                        ty,
                                        name,
                                        default_value,
                                    } => TableMemberInner::Used {
                                        ty: self.resolve_type(ty)?,
                                        name: name,
                                        default_value: match default_value {
                                            Some(val) => Some(self.resolve_value(val)?),
                                            None => None,
                                        },
                                    },
                                },
                            })
                        })
                    })
                    .collect();
                Ok(Decl::Table(Table {
                    attributes: decl.attributes,
                    strictness: decl.strictness,
                    name: decl.name,
                    members: members?,
                }))
            }
            raw::Decl::Union(decl) => {
                let members: Result<Vec<_>, _> = decl
                    .members
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|member| {
                            Ok(UnionMember {
                                attributes: member.attributes,
                                ordinal: member.ordinal,
                                inner: match member.inner {
                                    raw::UnionMemberInner::Reserved => UnionMemberInner::Reserved,
                                    raw::UnionMemberInner::Used { ty, name } => {
                                        UnionMemberInner::Used {
                                            ty: self.resolve_type(ty)?,
                                            name: name,
                                        }
                                    }
                                },
                            })
                        })
                    })
                    .collect();
                Ok(Decl::Union(Union {
                    attributes: decl.attributes,
                    strictness: decl.strictness,
                    name: decl.name,
                    members: members?,
                }))
            }
            raw::Decl::Protocol(decl) => {
                let methods: Result<Vec<_>, _> = decl
                    .methods
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|method| {
                            let request = match method.request {
                                None => None,
                                Some(req) => {
                                    let params: Result<Vec<_>, _> = req
                                        .into_iter()
                                        .map(|spanned| {
                                            spanned.try_map(|param| {
                                                Ok(Parameter {
                                                    attributes: param.attributes,
                                                    name: param.name,
                                                    ty: self.resolve_type(param.ty)?,
                                                })
                                            })
                                        })
                                        .collect();
                                    Some(params?)
                                }
                            };
                            let response = match method.response {
                                None => None,
                                Some(resp) => {
                                    let params: Result<Vec<_>, _> = resp
                                        .into_iter()
                                        .map(|spanned| {
                                            spanned.try_map(|param| {
                                                Ok(Parameter {
                                                    attributes: param.attributes,
                                                    name: param.name,
                                                    ty: self.resolve_type(param.ty)?,
                                                })
                                            })
                                        })
                                        .collect();
                                    Some(params?)
                                }
                            };
                            let error = match method.error {
                                None => None,
                                Some(err) => Some(self.resolve_type(err)?),
                            };
                            Ok(Method {
                                attributes: method.attributes,
                                name: method.name,
                                request,
                                response,
                                error,
                            })
                        })
                    })
                    .collect();
                let compose: Result<Vec<_>, _> = decl
                    .compose
                    .into_iter()
                    .map(|ident| self.resolve_name(ident))
                    .collect();
                Ok(Decl::Protocol(Protocol {
                    attributes: decl.attributes,
                    name: decl.name,
                    methods: methods?,
                    compose: compose?,
                }))
            }
            raw::Decl::Service(decl) => {
                let members: Result<Vec<_>, _> = decl
                    .members
                    .into_iter()
                    .map(|spanned| {
                        spanned.try_map(|member| {
                            Ok(ServiceMember {
                                attributes: member.attributes,
                                protocol: self.resolve_name(member.protocol)?,
                                name: member.name,
                            })
                        })
                    })
                    .collect();
                Ok(Decl::Service(Service {
                    attributes: decl.attributes,
                    name: decl.name,
                    members: members?,
                }))
            }
        }
    }

    fn resolve_type(&self, ty: Spanned<Box<raw::Type>>) -> Result<Spanned<Box<Type>>, Error> {
        unimplemented!()
    }

    fn resolve_value(&self, val: Spanned<raw::ConstVal>) -> Result<Spanned<ConstVal>, Error> {
        unimplemented!()
    }

    fn resolve_name(&self, name: raw::CompoundIdentifier) -> Result<Name, Error> {
        let span = name.span;
        let name = name.value;
        match name.len() {
            1 => {
                // this must be referring to something in the local context
                let name = name.into_iter().next().unwrap();
                if self.local_names.contains_key(&name) {
                    Ok(Name {
                        library: None,
                        name,
                        member: None,
                    })
                } else {
                    Err(Error::UnresolvedLocal(span))
                }
            }
            2 => {
                // if there are two components a.b, this can refer to two things:
                // decl b in library a, or member b of decl a in the local library.
                // TODO: the local names will need to keep track of existing member
                // names too...
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }
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
