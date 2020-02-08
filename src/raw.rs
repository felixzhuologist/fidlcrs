use crate::span;
use std::fmt;

pub mod attributes;
pub mod errors;
pub mod validate;

pub type Spanned<T> = span::Spanned<T, usize>;

// The natural thing that lalrpop wants to parse is a Vec<Spanned<String>>,
// but for regular compound identifiers we discard the spans of individual
// elements since they're not used.
pub type LibraryName = Vec<Spanned<String>>;
pub type CompoundIdentifier = Spanned<Vec<String>>;

#[derive(Debug)]
pub struct File {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: LibraryName,
    pub imports: Vec<Spanned<Import>>,
    pub decls: Vec<Spanned<Decl>>,
}

// TODO: compare this rep and the resolved version. they might be able to be
// refactored into specializations of a common ast

/// The possible toplevel things that can be declared in a FIDL file. They
/// comprise of:
///   - aliases, which store a name to another `Decl`
///   - `const`s, which declare a value and have a type
///   - types (structs, bits, enums, tables, and unions)
///   - protocols (protocol, service)
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
pub struct Import {
    // This is actually not allowed but we allow it at the lexing/parsing stage
    // to avoid grammar ambiguities
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: CompoundIdentifier,
    pub alias: Option<Spanned<String>>,
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
    pub compose: Vec<CompoundIdentifier>,
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
    pub protocol: CompoundIdentifier,
    pub name: Spanned<String>,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Spanned<String>,
    pub value: Option<Spanned<String>>,
}

#[derive(Debug, Clone)]
pub enum ConstVal {
    Identifier(CompoundIdentifier),
    Literal(Spanned<Literal>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Str(String),
    Int(IntLiteral),
    Float(f64),
    True,
    False,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: CompoundIdentifier,
    pub layout: Option<Spanned<Box<Type>>>,
    pub constraint: Option<Spanned<ConstVal>>,
    // doesn't make sense to have a Span in the false case. so just use the end
    // of the previous element
    pub nullable: bool,
}

#[derive(Debug, Copy, Clone)]
pub struct IntLiteral {
    pub value: u64,
    pub is_negative: bool,
}

#[derive(Debug, Clone)]
pub enum Strictness {
    Strict,
    Flexible,
}

// TODO: what's a good name for this? it's all "things" that can be specified in FIDL
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FidlType {
    Alias,
    Attribute,
    BitsDecl,
    BitsMember,
    ConstDecl,
    EnumDecl,
    EnumMember,
    ProtocolDecl,
    Library,
    Method,
    Parameter,
    ServiceDecl,
    ServiceMember,
    StructDecl,
    StructMember,
    TableDecl,
    TableMember,
    TypeAliasDecl,
    UnionDecl,
    UnionMember,
}

impl fmt::Display for FidlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use FidlType::*;
        match self {
            Alias => write!(f, "alias"),
            Attribute => write!(f, "attribute"),
            BitsDecl => write!(f, "bits declaration"),
            BitsMember => write!(f, "bits field"),
            ConstDecl => write!(f, "const declaration"),
            EnumDecl => write!(f, "enum declaration"),
            EnumMember => write!(f, "bits field"),
            ProtocolDecl => write!(f, "interface declaration"),
            Library => write!(f, "library declaration"),
            Method => write!(f, "interface method"),
            Parameter => write!(f, "parameter"),
            ServiceDecl => write!(f, "service declaration"),
            ServiceMember => write!(f, "service member"),
            StructDecl => write!(f, "struct declaration"),
            StructMember => write!(f, "struct field"),
            TableDecl => write!(f, "table declaration"),
            TableMember => write!(f, "table member"),
            TypeAliasDecl => write!(f, "type alias"),
            UnionDecl => write!(f, "union declaration"),
            UnionMember => write!(f, "union field"),
        }
    }
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
