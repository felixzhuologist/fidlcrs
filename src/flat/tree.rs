use crate::lexer::Span;
use crate::raw::{Attributes, IntLiteral, Spanned, Strictness};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct LibraryId(pub usize);

// TODO: this will be its own module once it's fleshed out some more
// TODO: keep track of unused dependencies
#[derive(Default)]
pub struct Libraries {
    /// Topologically sorted list of Libraries
    pub libraries: Vec<Library>,
    name_to_id: HashMap<String, LibraryId>,
}

impl Libraries {
    pub fn contains_library(&self, library: &String) -> bool {
        self.name_to_id.contains_key(library)
    }

    pub fn get_id(&self, library: &String) -> Option<LibraryId> {
        self.name_to_id.get(library).map(|l| *l)
    }

    pub fn get_name(&self, id: LibraryId) -> String {
        self.libraries[id.0].name.clone()
    }

    // TODO: should this error be in resolve?
    pub fn add_library(&mut self, lib: Library) -> Result<(), String> {
        // validate::validate_library(&lib, &self);

        // TODO: extra copies
        let name = lib.name.clone();
        if let Some(_) = self.name_to_id.insert(name.clone(), self.next_library_id()) {
            // TODO: attach more information to this error?
            Err(name)
        } else {
            self.libraries.push(lib);
            Ok(())
        }
    }

    pub fn lookup(&self, lib_name: &String, var: &String, member: &Option<String>) -> Option<Span> {
        self.name_to_id
            .get(lib_name)
            .map(|id| &self.libraries[id.0])
            .and_then(|lib| match member {
                Some(member) => lib.lookup_nested(var, member),
                None => lib.lookup(var),
            })
    }

    // Libraries are fully constructed before being added here, so they need to know what
    // ID they're going to have during Name resolution
    pub fn next_library_id(&self) -> LibraryId {
        LibraryId(self.libraries.len())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Sort {
    Term,
    Type,
    Protocol,
    Service,
}

// we can't place the attributes directly into the term/type, because
// it's associated with the _declaration_ of that value, and not its usage
// as an rhs value. for example, in the statement
//
// [Foo = "bar"] const uint8 x = 3;
//
// it doesn't make sense to associate the attributes with the rhs (3), rather
// it needs to be stored alongside the scope entry for x. as another example, in
//
// [Foo] using Foo = Bar<Baz>:Qux;
//
// the rhs is a Type::FunctionApplication. It wouldn't make sense to have an
// attributes on this variant, because it wouldn't exist in the context of e.g.
//
// struct Foo {
//   Bar<Baz>:Qux foo;
// }
//
// so we need to store the attributes alongside the entry and not the type itself.
// we get away with storing attributes inline on protocols and services
// because they can never be used in an rhs expression.
//
// note that this means that if we ever modelled builtins as a scope, each entry
// would need to explicitly specify no attributes (e.g. for uint8)

pub type TypeEntry = (Attributes, Spanned<Type>);
pub type TermEntry = (Attributes, Spanned<Type>, Spanned<Term>);

pub type TypeScope = HashMap<String, Spanned<TypeEntry>>;
pub type TermScope = HashMap<String, Spanned<TermEntry>>;

#[derive(Default)]
pub struct Library {
    pub attributes: Attributes,
    pub name: String,

    pub terms: TermScope,
    pub types: TypeScope,
    pub protocols: HashMap<String, Spanned<Protocol>>,
    pub services: HashMap<String, Spanned<Service>>,
}

#[derive(Debug, Clone)]
pub enum Term {
    Identifier(Name),
    Str(String),
    Int(IntLiteral),
    Float(f64),
    True,
    False,
}

// TODO: is it better to have these variants defined separately? or should they
// be inline on the enum?
// TODO: what should the distinction be? e.g. should bytes/byte/string be separate
// variants? handle vs client end vs server end?
#[derive(Debug, Clone)]
pub enum Type {
    Struct(Struct),
    Bits(Bits),
    Enum(Enum),
    Table(Table),
    Union(Union),
    // TODO: this is redundant. we can merge TypeSubstitution and
    // Identifier
    Identifier(Name),
    Ptr(Spanned<Box<Type>>),
    Array(Array),
    Vector(Vector),
    // Note: a `string` is just a `vector<uint8>`, but gets its own variant in the
    // Type enum since it is emitted as a different "kind" in the JSON IR.
    Str(Str),
    Handle(HandleSubtype),
    /// In fidl, this is just a `P` where `P` is a protocol
    ClientEnd(Name),
    /// In fidl, this is a `request<P>` where `P` is a protocol
    ServerEnd(Name),
    TypeSubstitution(TypeSubstitution),
    Primitive(PrimitiveSubtype),

    // an "untyped" int literal: this is the type that we assign to e.g. "3"
    // before we check that it's assignable to the type that the user says
    // it should be (e.g. in const uint8 = 3)
    // TODO: this is only relevant for the REPL, and we may want to remove it
    Int,
    Any,
}

#[derive(Debug, Copy, Clone)]
pub enum Kind {
    /// The top kind which is valid in any context. It's used to as the kind for
    /// types that are not well defined/errored, so that validation can continue
    /// while ensuring that the same error isn't returned multiple times
    Any,
    /// A regular Kind, which is defined in terms of its layout/constraints params
    Kind { layout: Param, constraints: Param },
}

#[derive(Debug, Clone)]
pub struct Bits {
    pub strictness: Option<Spanned<Strictness>>,
    pub ty: Option<Spanned<Box<Type>>>,
    pub members: Vec<Spanned<BitsMember>>,
}

impl Bits {
    pub fn lookup(&self, member_name: &String) -> Option<Span> {
        for member in &self.members {
            if &member.value.name.value == member_name {
                return Some(member.span);
            }
        }
        return None;
    }

    pub fn get_type(&self) -> &Type {
        self.ty
            .as_ref()
            .map(|ty| &*ty.value)
            .unwrap_or(&Type::Primitive(PrimitiveSubtype::UInt32))
    }
}

#[derive(Debug, Clone)]
pub struct BitsMember {
    pub attributes: Attributes,
    pub name: Spanned<String>,
    pub value: Spanned<Term>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub strictness: Option<Spanned<Strictness>>,
    pub ty: Option<Spanned<Box<Type>>>,
    pub members: Vec<Spanned<EnumMember>>,
}

impl Enum {
    pub fn lookup(&self, member_name: &String) -> Option<Span> {
        for member in &self.members {
            if &member.value.name.value == member_name {
                return Some(member.span);
            }
        }
        return None;
    }

    pub fn get_type(&self) -> &Type {
        self.ty
            .as_ref()
            .map(|ty| &*ty.value)
            .unwrap_or(&Type::Primitive(PrimitiveSubtype::UInt32))
    }
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub attributes: Attributes,
    pub name: Spanned<String>,
    pub value: Spanned<Term>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub members: Vec<Spanned<StructMember>>,
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub attributes: Attributes,
    pub ty: Spanned<Box<Type>>,
    pub name: Spanned<String>,
    pub default_value: Option<Spanned<Term>>,
}

#[derive(Debug, Clone)]
pub struct Table {
    pub strictness: Option<Spanned<Strictness>>,
    pub members: Vec<Spanned<TableMember>>,
}

impl Table {
    pub fn strictness(&self) -> Strictness {
        self.strictness
            .map(|sp| sp.value)
            .unwrap_or(Strictness::Flexible)
    }
}

#[derive(Debug, Clone)]
pub struct TableMember {
    pub attributes: Attributes,
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
    },
}

#[derive(Debug, Clone)]
pub struct Union {
    pub strictness: Option<Spanned<Strictness>>,
    pub members: Vec<Spanned<UnionMember>>,
}

impl Union {
    pub fn strictness(&self) -> Strictness {
        self.strictness
            .map(|sp| sp.value)
            .unwrap_or(Strictness::Flexible)
    }
}

#[derive(Debug, Clone)]
pub struct UnionMember {
    pub attributes: Attributes,
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
    pub attributes: Attributes,
    pub methods: Vec<Spanned<Method>>,
    pub compose: Vec<Spanned<Name>>,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub attributes: Attributes,
    pub name: Spanned<String>,
    pub request: Option<Vec<Spanned<Parameter>>>,
    pub response: Option<Vec<Spanned<Parameter>>>,
    pub error: Option<Spanned<Box<Type>>>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub attributes: Attributes,
    pub name: Spanned<String>,
    pub ty: Spanned<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct Service {
    pub attributes: Attributes,
    pub members: Vec<Spanned<ServiceMember>>,
}

#[derive(Debug, Clone)]
pub struct ServiceMember {
    pub attributes: Attributes,
    pub protocol: Spanned<Name>,
    pub name: Spanned<String>,
}

// TODO: .name can actually be an Id, since after resolution it's guaranteed
// to exist. we would just need to make sure they're unique across all sorts
// (maybe every value should be stored in an Entry enum?)
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name {
    pub library: LibraryId,
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

#[derive(Debug, Clone)]
pub struct Array {
    pub element_type: Option<Spanned<Box<Type>>>,
    pub size: Option<Spanned<Box<Term>>>,
}

#[derive(Debug, Clone)]
pub struct Vector {
    pub element_type: Option<Spanned<Box<Type>>>,
    pub bounds: Option<Spanned<Box<Term>>>,
}

#[derive(Debug, Clone)]
pub struct Str {
    pub bounds: Option<Spanned<Box<Term>>>,
}

// TODO: s/constraint/constraints to be consistent?
// NOTE: either layout or constraint will be Some, otherwise this would just be flattened
// directly into `func`
/// Substitute layout and constraint into the func type, e.g. func<layout>:constraint
#[derive(Debug, Clone)]
pub struct TypeSubstitution {
    pub func: Spanned<Box<Type>>,
    pub layout: Option<Spanned<Box<Type>>>,
    pub constraint: Option<Spanned<Box<Term>>>,
}

impl Kind {
    pub fn base_kind() -> Kind {
        Kind::Kind {
            layout: Param::None,
            constraints: Param::None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Param {
    /// this parameter is required for the type to be concrete
    Required,
    /// this parameter is not required for the type to be concrete, but could
    /// be passed
    Optional,
    /// the type does not take this parameter
    // technically Param should only have two states, and layout/constraints
    // should be Option<Param> but it's simpler just to flatten into 3 states
    None,
}

impl Param {
    pub fn required<T>(val: &Option<T>) -> Param {
        if val.is_some() {
            Param::None
        } else {
            Param::Required
        }
    }

    pub fn optional<T>(val: &Option<T>) -> Param {
        if val.is_some() {
            Param::None
        } else {
            Param::Optional
        }
    }

    // substitute this Option into this Param
    pub fn take<T>(self, val: &Option<T>) -> Result<Param, ()> {
        match (self, val) {
            // param can take a value, and value is defined
            (Param::Required, Some(_)) | (Param::Optional, Some(_)) => Ok(Param::None),
            // param can't take any values but there is a value
            (Param::None, Some(_)) => Err(()),
            // param can't take any values but there is no value
            (Param::None, None) => Ok(Param::None),
            // param can take a value but there's no value
            (param, None) => Ok(param),
        }
    }

    pub fn needs_value(&self) -> bool {
        match self {
            Param::Required => true,
            Param::Optional | Param::None => false,
        }
    }
}
