use crate::span;

pub mod attributes;
pub mod errors;
pub mod validate;

pub type Spanned<T> = span::Spanned<T, usize>;

pub type CompoundIdentifier = Vec<Spanned<String>>;

pub enum Decl {
    Struct(Struct),
    Const(ConstDecl),
    Bits(Bits),
    Enum(Enum),
    Table(Table),
    Union(Union),
    Alias(Alias),
    Protocol(Protocol),
    Service(Service),
}

#[derive(Debug)]
pub struct File {
    pub attributes: Vec<Spanned<Attribute>>,
    pub name: CompoundIdentifier,
    pub imports: Vec<Spanned<Import>>,

    pub aliases: Vec<Spanned<Alias>>,

    pub consts: Vec<Spanned<ConstDecl>>,

    pub bits: Vec<Spanned<Bits>>,
    pub enums: Vec<Spanned<Enum>>,
    pub structs: Vec<Spanned<Struct>>,
    pub tables: Vec<Spanned<Table>>,
    pub unions: Vec<Spanned<Union>>,

    pub protocols: Vec<Spanned<Protocol>>,
    pub services: Vec<Spanned<Service>>,
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

// TODO: post FTP-50/templated types, should just have a generic type struct
// #[derive(Debug, Clone)]
// pub struct Type {
//     pub name: Spanned<CompoundIdentifier>,
//     pub layout: Option<Box<Type>>,
//     pub constraint: Option<Spanned<String>>,
// }

#[derive(Debug, Clone)]
pub struct IntLiteral {
    pub value: u64,
    pub is_negative: bool,
}

#[derive(Debug, Clone)]
pub enum Strictness {
    Strict,
    Flexible,
}
