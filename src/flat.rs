use crate::raw::{Attribute, IntLiteral, Literal, Spanned, Strictness};
use std::collections::HashMap;

pub mod errors;
pub mod resolve;

#[derive(Debug)]
pub struct Library {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: String,
    pub decls: HashMap<String, Spanned<Decl>>,
}

// TODO: compare this and the raw version. they might be able to be
// refactored into specializations of a common ast
// so far the only differences are:
//   - Name instead of CompoundIdentifier
//   - flat::Type instead of raw::Type

#[derive(Debug)]
pub enum Decl {
    Alias(Alias),

    Const(ConstDecl),

    Struct(Struct),
    Bits(Bits),
    Enum(Enum),
    Table(Table),
    Union(Union),

    Protocol(Protocol),
    Service(Service),
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub ty: Spanned<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub attributes: Vec<Spanned<Attribute>>,
    pub ty: Spanned<Box<Type>>,
    pub name: Spanned<String>,
    pub value: Spanned<ConstVal>,
}

#[derive(Debug, Clone)]
pub struct Bits {
    pub attributes: Vec<Spanned<Attribute>>,
    pub strictness: Option<Spanned<Strictness>>,
    pub ty: Option<Spanned<Box<Type>>>,
    pub name: Spanned<String>,
    pub members: Vec<Spanned<BitsMember>>,
}

#[derive(Debug, Clone)]
pub struct BitsMember {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub value: Spanned<ConstVal>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub attributes: Vec<Spanned<Attribute>>,
    pub strictness: Option<Spanned<Strictness>>,
    pub ty: Option<Spanned<Box<Type>>>,
    pub name: Spanned<String>,
    pub members: Vec<Spanned<EnumMember>>,
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub value: Spanned<ConstVal>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub members: Vec<Spanned<StructMember>>,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub attributes: Vec<Spanned<Attribute>>,
    pub ty: Spanned<Box<Type>>,
    pub name: Spanned<String>,
    pub default_value: Option<Spanned<ConstVal>>,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub attributes: Vec<Spanned<Attribute>>,
    pub strictness: Option<Spanned<Strictness>>,
    pub name: Spanned<String>,
    pub members: Vec<Spanned<TableMember>>,
}

#[derive(Debug, Clone)]
pub struct TableMember {
    pub attributes: Vec<Spanned<Attribute>>,
    pub ordinal: Spanned<IntLiteral>,
    // TODO: naming :(
    pub inner: TableMemberInner,
}

#[derive(Debug, Clone)]
pub enum TableMemberInner {
    Reserved,
    Used {
        ty: Spanned<Box<Type>>,
        name: Spanned<String>,
        default_value: Option<Spanned<ConstVal>>,
    },
}

#[derive(Debug, Clone)]
pub struct Union {
    pub attributes: Vec<Spanned<Attribute>>,
    pub strictness: Option<Spanned<Strictness>>,
    pub name: Spanned<String>,
    pub members: Vec<Spanned<UnionMember>>,
}

#[derive(Debug, Clone)]
pub struct UnionMember {
    pub attributes: Vec<Spanned<Attribute>>,
    pub ordinal: Spanned<IntLiteral>,
    pub inner: UnionMemberInner,
}

#[derive(Debug, Clone)]
pub enum UnionMemberInner {
    Reserved,
    Used {
        ty: Spanned<Box<Type>>,
        name: Spanned<String>,
    },
}

#[derive(Debug, Clone)]
pub struct Protocol {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub methods: Vec<Spanned<Method>>,
    pub compose: Vec<Name>,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub request: Option<Vec<Spanned<Parameter>>>,
    pub response: Option<Vec<Spanned<Parameter>>>,
    pub error: Option<Spanned<Box<Type>>>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub ty: Spanned<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct Service {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: Spanned<String>,
    pub members: Vec<Spanned<ServiceMember>>,
}

#[derive(Debug, Clone)]
pub struct ServiceMember {
    pub attributes: Vec<Spanned<Attribute>>,
    pub protocol: Name,
    pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub enum ConstVal {
    Identifier(Name),
    Literal(Spanned<Literal>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Array {
        element_type: Box<Type>,
        size: ConstVal,
    },
    Vector {
        element_type: Box<Type>,
        maybe_max_size: Option<ConstVal>,
        nullable: bool,
    },
    Str {
        maybe_max_size: Option<ConstVal>,
        nullable: bool,
    },
    Handle {
        subtype: Option<HandleSubtype>,
        nullable: bool,
    },
    ServerEnd {
        protocol: Option<Spanned<String>>,
        nullable: bool,
    },
    Primitive(PrimitiveSubtype),
    Identifier {
        name: Spanned<Name>,
        layout: Option<Spanned<Box<Type>>>,
        constraint: Option<Spanned<ConstVal>>,
        nullable: bool,
    },
}

#[derive(Debug, Clone)]
pub struct Name {
    pub library: Option<String>,
    pub name: String,
    pub member: Option<String>,
}

#[derive(Debug, Copy, Clone)]
pub enum HandleSubtype {
    Bti,
    Channel,
    DebugLog,
    Event,
    Eventpair,
    Exception,
    Fifo,
    Guest,
    Interrupt,
    Iommu,
    Job,
    Pager,
    PciDevice,
    Pmt,
    Port,
    Process,
    Profile,
    Resource,
    Socket,
    SuspendToken,
    Thread,
    Timer,
    VCpu,
    Vmar,
    Vmo,
}

#[derive(Debug, Copy, Clone)]
pub enum PrimitiveSubtype {
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
}

impl Decl {
    // this is used to populated the map of library names before doing
    // resolution. TODO: should this implement Nameable instead?
    pub fn name(&self) -> String {
        match self {
            Decl::Alias(decl) => decl.name.value.clone(),
            Decl::Const(decl) => decl.name.value.clone(),
            Decl::Struct(decl) => decl.name.value.clone(),
            Decl::Bits(decl) => decl.name.value.clone(),
            Decl::Enum(decl) => decl.name.value.clone(),
            Decl::Table(decl) => decl.name.value.clone(),
            Decl::Union(decl) => decl.name.value.clone(),
            Decl::Protocol(decl) => decl.name.value.clone(),
            Decl::Service(decl) => decl.name.value.clone(),
        }
    }
}
