use crate::flat;
use crate::raw;
use crate::typeshape;
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

        // let mut structs = Vec::new();
        let mut bits = Vec::new();
        let mut enums = Vec::new();
        for (name, entry) in lib.types {
            let (attributes, ty) = entry.value;
            let full_name = CompoundIdentifier {
                library_name: id_to_name.get(&lib_id).unwrap().clone(),
                decl_name: name,
            };
            match ty.value {
                // TODO: we need to precompute layout typeshpaes becaues it requires
                // access to Libraries which is consumed this point.
                // flat::Type::Struct(decl) => {
                    
                //     structs.push(StructDecl {
                //         maybe_attributes: attributes,
                //         name: full_name.clone(),
                //         anonymous: false,
                //         members,
                //         type_shape_v1: unimplemented!(),
                //     });
                //     declmap.insert(full_name, DeclType::Struct);
                // }
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
                _ => unimplemented!(),
            }
        }

        Library {
            version: "0.0.1",
            name: lib.name,             // TODO: move instead
            attributes: lib.attributes, // TODO: move instead
            consts,
            // structs,
            bits,
            enums,
            // tables: Vec::new(),
            // unions: Vec::new(),
            // protocols: Vec::new(),
            // services: Vec::new(),
            // declarations: fidl_json_ir::DeclMap::new(),
            // library_dependencies: Vec::new(),
        }
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
    #[serde(rename = "bits_declarations")]
    pub bits: Vec<BitsDecl>,
    #[serde(rename = "enum_declarations")]
    pub enums: Vec<EnumDecl>,
    // #[serde(rename = "interface_declarations")]
    // pub protocols: Vec<Spanned<Protocol>>,
    // #[serde(rename = "struct_declarations")]
    // pub structs: Vec<Spanned<Struct>>,
    // #[serde(rename = "table_declarations")]
    // pub tables: Vec<Spanned<Table>>,
    // #[serde(rename = "union_declarations")]
    // pub unions: Vec<Spanned<Union>>,
    // pub declaration_order: Vec<String>,
    // pub declarations: DeclMap,
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
    pub type_shape_v1: typeshape::TypeShape,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructMember {
    pub maybe_attributes: raw::Attributes,
    pub name: String,
    #[serde(rename = "type")]
    pub ty: Type,
    pub field_shape_v1: typeshape::FieldShape,
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
    pub type_shape_v1: typeshape::TypeShape,
}

#[derive(Debug, Clone, Serialize)]
pub struct TableMember {
    pub maybe_attributes: raw::Attributes,
    pub ordinal: u32,
    #[serde(flatten)]
    pub member_type: TableMemberType,
}

#[derive(Debug, Clone, Serialize)]
pub enum TableMemberType {
    Reserved,
    Field { r#type: Type, name: String },
}

#[derive(Debug, Clone, Serialize)]
pub struct UnionDecl {
    pub maybe_attributes: raw::Attributes,
    pub name: CompoundIdentifier,
    pub members: Vec<UnionMember>,
    #[serde(default)]
    pub strict: bool,
    pub type_shape_v1: typeshape::TypeShape,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnionMember {
    pub maybe_attributes: raw::Attributes,
    pub ordinal: u32,
    #[serde(flatten)]
    pub member_type: UnionMemberType,
}

#[derive(Debug, Clone, Serialize)]
pub enum UnionMemberType {
    Reserved,
    Field { r#type: Type, name: String },
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
    pub name: CompoundIdentifier,
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
}
