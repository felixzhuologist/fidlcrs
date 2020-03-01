use crate::flat;
use crate::raw;
use crate::typeshape;
use crate::typeshape::{desugar, typeshape, FieldShape, TypeShape, WireFormat};
// use crate::raw::Spanned;
// use serde::ser::SerializeMap;
use serde::{Serialize, Serializer};
use std::collections::HashMap;

// impl Serialize for flat::Libraries {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         let lib = self.libraries.last().unwrap();

//         let mut ir = serializer.serialize_map(None)?;
//         ir.serialize_entry("version", "0.0.1")?;
//         ir.serialize_entry("name", &lib.name)?;

//         ir.end()
//     }
// }

impl flat::Libraries {
    pub fn to_ir(mut self) -> Library {
        // TODO: this block of code does stuff on the library before mutating it (by popping
        // below) while it's still valid. this should be moved to a separate step, including
        // storing precomputed constant values. for time reasons of getting this step working
        // it's here (and half done) for now. in general this step needs to be thought through
        // some more, e.g. consider moving it along with kinds/types from flat::Validator into
        // either Libraries (along with flattened scope) or some other thing.
        let lib = self.libraries.last().unwrap();
        let mut typeshapes = HashMap::new();
        let mut fieldshapes = HashMap::new();
        for (name, entry) in lib.types.iter() {
            let (_, ty) = &entry.value;
            let desugared = desugar(&ty.value, &self, false);
            typeshapes.insert(name.clone(), typeshape(&desugared, WireFormat::V1));
            if let flat::Type::Struct(_) = &ty.value {
                fieldshapes.insert(
                    name.clone(),
                    typeshape::fieldshapes(&desugared, false, WireFormat::V1).unwrap(),
                );
            }
        }

        let lib = self.libraries.pop().unwrap();
        let lib_id = self.next_library_id();
        let id_to_name: HashMap<_, _> = self
            .name_to_id
            .into_iter()
            .map(|(name, id)| (id, name))
            .collect();

        let mut declmap: HashMap<CompoundIdentifier, DeclType> = HashMap::new();

        let mut consts = Vec::new();
        for (name, entry) in lib.terms {
            let (attributes, ty, term) = entry.value;
            let full_name = CompoundIdentifier {
                library_name: id_to_name.get(&lib_id).unwrap().clone(),
                decl_name: name,
            };
            consts.push(ConstDecl {
                attributes: attributes,
                ty: to_type(ty.value, &id_to_name, false),
                name: full_name.clone(),
                value: to_constant(term.value, &id_to_name),
            });
            declmap.insert(full_name, DeclType::Const);
        }

        let mut structs = Vec::new();
        let mut bits = Vec::new();
        let mut enums = Vec::new();
        let mut tables = Vec::new();
        let mut unions = Vec::new();
        for (name, entry) in lib.types {
            let (attributes, ty) = entry.value;
            let full_name = CompoundIdentifier {
                library_name: id_to_name.get(&lib_id).unwrap().clone(),
                decl_name: name.clone(),
            };
            match ty.value {
                flat::Type::Struct(decl) => {
                    let members = decl
                        .members
                        .into_iter()
                        .zip(fieldshapes.remove(&name).unwrap().into_iter())
                        .map(|(m, fs)| to_structmember(m.value, fs, &id_to_name))
                        .collect();
                    structs.push(StructDecl {
                        maybe_attributes: attributes,
                        name: full_name.clone(),
                        anonymous: false,
                        members,
                        type_shape_v1: typeshapes.remove(&name).unwrap(),
                    });
                    declmap.insert(full_name, DeclType::Struct);
                }
                flat::Type::Bits(decl) => {
                    let ty = decl.get_type().clone();
                    let members = decl
                        .members
                        .into_iter()
                        .map(|m| to_bitsmember(m.value, &id_to_name))
                        .collect();
                    bits.push(BitsDecl {
                        maybe_attributes: attributes,
                        name: full_name.clone(),
                        ty: to_type(ty, &id_to_name, false),
                        members,
                        strict: decl.strictness.map_or(true, |s| !s.value.is_flexible()),
                    });
                    declmap.insert(full_name, DeclType::Bits);
                }
                flat::Type::Enum(decl) => {
                    let ty = decl.get_type().clone();
                    let members = decl
                        .members
                        .into_iter()
                        .map(|m| to_enummember(m.value, &id_to_name))
                        .collect();
                    enums.push(EnumDecl {
                        maybe_attributes: attributes,
                        name: full_name.clone(),
                        ty: to_type(ty, &id_to_name, false),
                        members,
                        strict: decl.strictness.map_or(true, |s| !s.value.is_flexible()),
                    });
                    declmap.insert(full_name, DeclType::Enum);
                }
                flat::Type::Table(decl) => {
                    let members = decl
                        .members
                        .into_iter()
                        .map(|m| to_tablemember(m.value, &id_to_name))
                        .collect();
                    tables.push(TableDecl {
                        maybe_attributes: attributes,
                        name: full_name.clone(),
                        members,
                        strict: decl.strictness.map_or(false, |s| !s.value.is_flexible()),
                        type_shape_v1: typeshapes.remove(&name).unwrap(),
                    });
                    declmap.insert(full_name, DeclType::Table);
                }
                flat::Type::Union(decl) => {
                    let members = decl
                        .members
                        .into_iter()
                        .map(|m| to_unionmember(m.value, &id_to_name))
                        .collect();
                    unions.push(UnionDecl {
                        maybe_attributes: attributes,
                        name: full_name.clone(),
                        members,
                        strict: decl.strictness.map_or(false, |s| !s.value.is_flexible()),
                        type_shape_v1: typeshapes.remove(&name).unwrap(),
                    });
                    declmap.insert(full_name, DeclType::Union);
                }
                _ => panic!("only layouts here"),
            }
        }

        let mut protocols = Vec::new();
        for (name, protocol) in lib.protocols {
            let full_name = CompoundIdentifier {
                library_name: id_to_name.get(&lib_id).unwrap().clone(),
                decl_name: name,
            };
            let methods = protocol
                .value
                .methods
                .into_iter()
                .map(|m| to_method(m.value, &id_to_name))
                .collect();
            protocols.push(Interface {
                maybe_attributes: protocol.value.attributes,
                name: full_name.clone(),
                methods,
            });
            declmap.insert(full_name, DeclType::Protocol);
        }

        let mut services = Vec::new();
        for (name, service) in lib.services {
            let full_name = CompoundIdentifier {
                library_name: id_to_name.get(&lib_id).unwrap().clone(),
                decl_name: name,
            };
            let members = service
                .value
                .members
                .into_iter()
                .map(|m| to_servicemember(m.value, &id_to_name))
                .collect();
            services.push(Service {
                maybe_attributes: service.value.attributes,
                name: full_name.clone(),
                members,
            });
            declmap.insert(full_name, DeclType::Service);
        }

        Library {
            version: "0.0.1",
            name: lib.name,             // TODO: move instead
            attributes: lib.attributes, // TODO: move instead
            consts,
            structs,
            bits,
            enums,
            tables,
            unions,
            protocols,
            services,
            declarations: declmap,
            // library_dependencies: Vec::new(),
        }
    }
}

pub fn to_structmember(
    member: flat::StructMember,
    fieldshape: FieldShape,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> StructMember {
    StructMember {
        maybe_attributes: member.attributes,
        name: member.name.value,
        ty: to_type(*member.ty.value, id_to_name, false),
        field_shape_v1: fieldshape,
        maybe_default_value: member
            .default_value
            .map(|v| to_constant(v.value, id_to_name)),
    }
}
pub fn to_bitsmember(
    member: flat::BitsMember,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> BitsMember {
    BitsMember {
        name: member.name.value,
        maybe_attributes: member.attributes,
        value: to_constant(member.value.value, id_to_name),
    }
}

pub fn to_enummember(
    member: flat::EnumMember,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> EnumMember {
    EnumMember {
        name: member.name.value,
        maybe_attributes: member.attributes,
        value: to_constant(member.value.value, id_to_name),
    }
}

pub fn to_tablemember(
    member: flat::TableMember,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> TableMember {
    let (reserved, ty, name) = match member.inner {
        flat::TableMemberInner::Reserved => (true, None, None),
        flat::TableMemberInner::Used { ty, name } => (
            false,
            Some(to_type(*ty.value, id_to_name, false)),
            Some(name.value),
        ),
    };
    TableMember {
        maybe_attributes: member.attributes,
        ordinal: member.ordinal.value.value as u32,
        reserved,
        ty,
        name,
    }
}

pub fn to_unionmember(
    member: flat::UnionMember,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> UnionMember {
    let (reserved, ty, name) = match member.inner {
        flat::UnionMemberInner::Reserved => (true, None, None),
        flat::UnionMemberInner::Used { ty, name } => (
            false,
            Some(to_type(*ty.value, id_to_name, false)),
            Some(name.value),
        ),
    };
    UnionMember {
        maybe_attributes: member.attributes,
        ordinal: member.ordinal.value.value as u32,
        reserved,
        ty,
        name,
    }
}

pub fn to_method(method: flat::Method, _id_to_name: &HashMap<flat::LibraryId, String>) -> Method {
    Method {
        maybe_attributes: method.attributes,
        ordinal: 0, // TODO
        name: method.name.value,
        has_request: method.request.is_some(),
        maybe_request_payload: None, // TODO
        has_response: method.response.is_some(),
        maybe_response_payload: None, // TODO
        is_composed: false,           // TODO
    }
}

pub fn to_servicemember(
    member: flat::ServiceMember,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> ServiceMember {
    ServiceMember {
        maybe_attributes: member.attributes,
        name: member.name.value,
        ty: Type {
            kind: TypeKind::Identifier {
                identifier: to_name(member.protocol.value, id_to_name),
            },
            nullable: false,
        },
    }
}

pub fn to_type(
    ty: flat::Type,
    id_to_name: &HashMap<flat::LibraryId, String>,
    nullable: bool,
) -> Type {
    match ty {
        // these are stored separately in a foo_declarations field, we don't need
        // to deal with this until anonymous layouts are introduced
        flat::Type::Struct(_)
        | flat::Type::Bits(_)
        | flat::Type::Enum(_)
        | flat::Type::Table(_)
        | flat::Type::Union(_) => panic!("should not get here"),
        // nothing in the scope should have this type, since it's only used during
        // validation
        flat::Type::Int | flat::Type::Any => panic!("should not get here either"),
        flat::Type::Identifier(n) => Type {
            kind: TypeKind::Identifier {
                identifier: to_name(n, id_to_name),
            },
            nullable,
        },
        flat::Type::Ptr(ty) => to_type(*ty.value, id_to_name, true),
        flat::Type::Array(flat::Array { element_type, .. }) => Type {
            kind: TypeKind::Array {
                element_type: Box::new(to_type(*element_type.unwrap().value, id_to_name, nullable)),
                // TODO: we need to save this inside Libraries, because here the Library
                // has already been consumed and we can't do eval
                element_count: 0,
            },
            nullable,
        },
        flat::Type::Vector(flat::Vector { element_type, .. }) => Type {
            kind: TypeKind::Vector {
                element_type: Box::new(to_type(*element_type.unwrap().value, id_to_name, nullable)),
                // TODO: same as for arrays
                maybe_element_count: None,
            },
            nullable,
        },
        flat::Type::Str(_) => Type {
            kind: TypeKind::String {
                // TODO: same as for arrays
                maybe_element_count: None,
            },
            nullable,
        },
        flat::Type::Handle(subtype) => Type {
            kind: TypeKind::Handle {
                // TODO: add a Handle subtype, which is actually just when there is no
                // subtype. set the subtype to it if none is specified, and remove the Option
                subtype: subtype.unwrap(),
            },
            nullable,
        },
        flat::Type::ClientEnd(n) => Type {
            kind: TypeKind::Identifier {
                identifier: to_name(n, id_to_name),
            },
            nullable,
        },
        flat::Type::ServerEnd(n) => Type {
            kind: TypeKind::Request {
                subtype: to_name(n, id_to_name),
            },
            nullable,
        },
        // TODO: we actually do need to handle this, e.g. for a member of type array<foo>:bar, since
        // it's possible to have non type aliases here (though we'd be able to assume that this is
        // of kind *). in the same way that we "unflatten" type function application, we'd need to
        // re flatten them for the IR. either that or we'd need to keep self valid up to here so that
        // we can eval
        flat::Type::TypeSubstitution(_) => panic!("fidlc currently doesn't emit these"),
        flat::Type::Primitive(subtype) => Type {
            kind: TypeKind::Primitive { subtype },
            nullable,
        },
    }
}

pub fn to_constant(term: flat::Term, id_to_name: &HashMap<flat::LibraryId, String>) -> Constant {
    match term {
        flat::Term::Identifier(n) => Constant::Identifier {
            identifier: to_name(n, id_to_name),
        },
        flat::Term::Str(_) => Constant::Literal {
            literal: Literal {
                kind: LiteralKind::String,
            },
        },
        flat::Term::Int(_) => Constant::Literal {
            literal: Literal {
                kind: LiteralKind::Numeric,
            },
        },
        flat::Term::Float(_) => Constant::Literal {
            literal: Literal {
                kind: LiteralKind::Numeric,
            },
        },
        flat::Term::True => Constant::Literal {
            literal: Literal {
                kind: LiteralKind::True,
            },
        },
        flat::Term::False => Constant::Literal {
            literal: Literal {
                kind: LiteralKind::True,
            },
        },
    }
}

pub fn to_name(
    name: flat::Name,
    id_to_name: &HashMap<flat::LibraryId, String>,
) -> CompoundIdentifier {
    if name.member.is_some() {
        panic!("todo")
    }
    CompoundIdentifier {
        library_name: id_to_name.get(&name.library).unwrap().clone(),
        decl_name: name.name,
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Library {
    pub version: &'static str,
    pub name: String,
    pub attributes: raw::Attributes,
    #[serde(rename = "const_declarations")]
    pub consts: Vec<ConstDecl>,
    #[serde(rename = "struct_declarations")]
    pub structs: Vec<StructDecl>,
    #[serde(rename = "bits_declarations")]
    pub bits: Vec<BitsDecl>,
    #[serde(rename = "enum_declarations")]
    pub enums: Vec<EnumDecl>,
    #[serde(rename = "table_declarations")]
    pub tables: Vec<TableDecl>,
    #[serde(rename = "union_declarations")]
    pub unions: Vec<UnionDecl>,
    #[serde(rename = "interface_declarations")]
    pub protocols: Vec<Interface>,
    #[serde(rename = "service_declarations")]
    pub services: Vec<Service>,
    // pub declaration_order: Vec<String>,
    pub declarations: HashMap<CompoundIdentifier, DeclType>,
    // pub library_dependencies: Vec<LibraryDep>,
    // note: these are used only prior to resolution.
    // #[serde(skip_serializing, skip_deserializing)]
    // pub usings: Vec<Spanned<Using>>,
}

// NOTE: fidlc omits attributes if they're empty. is this behavior necessary?

#[derive(Debug, Clone, Serialize)]
pub struct ConstDecl {
    pub attributes: raw::Attributes,
    #[serde(rename = "type")]
    pub ty: Type,
    pub name: CompoundIdentifier,
    pub value: Constant,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructDecl {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    pub anonymous: bool,
    pub members: Vec<StructMember>,
    pub type_shape_v1: TypeShape,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructMember {
    pub maybe_attributes: raw::Attributes,
    pub name: String,
    #[serde(rename = "type")]
    pub ty: Type,
    pub field_shape_v1: FieldShape,
    pub maybe_default_value: Option<Constant>,
}

#[derive(Debug, Clone, Serialize)]
pub struct BitsDecl {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    #[serde(rename = "type")]
    pub ty: Type,
    // TODO: mask
    pub members: Vec<BitsMember>,
    pub strict: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct BitsMember {
    pub name: String,
    pub maybe_attributes: raw::Attributes,
    pub value: Constant,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumDecl {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    #[serde(rename = "type")]
    pub ty: Type,
    pub members: Vec<EnumMember>,
    pub strict: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct EnumMember {
    pub name: String,
    pub maybe_attributes: raw::Attributes,
    pub value: Constant,
}

#[derive(Debug, Clone, Serialize)]
pub struct TableDecl {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    pub members: Vec<TableMember>,
    pub strict: bool,
    pub type_shape_v1: TypeShape,
}

#[derive(Debug, Clone, Serialize)]
pub struct TableMember {
    pub maybe_attributes: raw::Attributes,
    pub ordinal: u32,
    pub reserved: bool,
    #[serde(rename = "type")]
    pub ty: Option<Type>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnionDecl {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    pub members: Vec<UnionMember>,
    pub strict: bool,
    pub type_shape_v1: TypeShape,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnionMember {
    pub maybe_attributes: raw::Attributes,
    pub ordinal: u32,
    pub reserved: bool,
    #[serde(rename = "type")]
    pub ty: Option<Type>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Interface {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    pub methods: Vec<Method>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Method {
    pub maybe_attributes: raw::Attributes,
    pub ordinal: u64,
    pub name: String,
    pub has_request: bool,
    pub maybe_request_payload: Option<CompoundIdentifier>,
    pub has_response: bool,
    pub maybe_response_payload: Option<CompoundIdentifier>,
    pub is_composed: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct Service {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    pub members: Vec<ServiceMember>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ServiceMember {
    pub maybe_attributes: raw::Attributes,
    pub name: String,
    #[serde(rename = "type")]
    pub ty: Type,
}

#[derive(Debug, Clone, Serialize)]
pub struct Type {
    #[serde(flatten)]
    pub kind: TypeKind,
    pub nullable: bool,
}

#[derive(Debug, Clone, Serialize)]
pub enum TypeKind {
    Array {
        element_type: Box<Type>,
        element_count: u32,
    },
    Vector {
        element_type: Box<Type>,
        #[serde(default)]
        maybe_element_count: Option<u32>,
    },
    String {
        #[serde(default)]
        maybe_element_count: Option<u32>,
    },
    Handle {
        subtype: flat::HandleSubtype,
    },
    Request {
        subtype: CompoundIdentifier,
    },
    Primitive {
        subtype: flat::PrimitiveSubtype,
    },
    Identifier {
        identifier: CompoundIdentifier,
    },
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind")]
#[serde(rename_all = "lowercase")]
pub enum Constant {
    Identifier { identifier: CompoundIdentifier }, // missing value, expression
    Literal { literal: Literal },                  // missing value, expression
}

#[derive(Debug, Clone, Serialize)]
pub struct Literal {
    pub kind: LiteralKind,
    // #[serde(default)]
    // pub value: Option<String>,
    // missing expression
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum LiteralKind {
    String,
    Numeric,
    True,
    False,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompoundIdentifier {
    pub library_name: String,
    pub decl_name: String,
}

impl Serialize for CompoundIdentifier {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        [&self.library_name, "/", &self.decl_name]
            .concat()
            .serialize(serializer)
    }
}

// TODO: can this be merged with placement?
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum DeclType {
    Const,
    Bits,
    Enum,
    #[serde(rename = "interface")]
    Protocol,
    Struct,
    Table,
    Union,
    Service,
}
