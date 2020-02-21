use crate::lexer::Span;
use crate::raw::{Attributes, IntLiteral, Spanned, Strictness};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

pub mod errors;
pub mod resolve;
pub mod visitor;

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

// TODO: should this be in resolve instead?
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
    Identifier(Name),
    Ptr(Box<Type>),
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
        default_value: Option<Spanned<Term>>,
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

#[derive(Debug, Clone)]
pub struct Array {
    pub element_type: Option<Box<Type>>,
    pub size: Option<Box<Term>>,
}

#[derive(Debug, Clone)]
pub struct Vector {
    pub element_type: Option<Box<Type>>,
    pub bounds: Option<Box<Term>>,
}

#[derive(Debug, Clone)]
pub struct Str {
    pub bounds: Option<Box<Term>>,
}

// NOTE: the either layout or constraint should be Some, otherwise this would just be flattened
// directly into `func`
/// Substitute layout and constraint into the func type, e.g. func<layout>:constraint
#[derive(Debug, Clone)]
pub struct TypeSubstitution {
    pub func: Box<Type>,
    pub layout: Option<Spanned<Box<Type>>>,
    pub constraint: Option<Spanned<Box<Term>>>,
}

// pub trait TypeScope {
//     type Type;

//     // TODO: what's the sig?
//     fn lookup(&self, constraints: u32, layout: u32) -> &Self::Type;
// }

// pub struct Builtins {
// }

// impl Builtins {
//     pub fn lookup(&self, _ty: &str) -> Type {
//         unimplemented!()
//     }
// }

// pub enum TermEvalError {
//     Undefined(Name)
// }

// #[derive(Debug, Copy, Clone)]
// pub enum Kind {
//     NeedsLayoutAndConstraints,
//     NeedsLayout,
//     NeedsConstraints,
//     Type,
//     // this kind is unique, in that a type of this kind is considered "concrete" or "closed",
//     // (i.e. it can be used as the type of a member), but it can also take in a parameter to
//     // return another kind
//     // TODO: think about how this should work some more
//     CanTakeConstraints,
//     NeedsLayoutCanTakeConstraints,
// }

// the "Kind enum" would essentially be the 2^3 possible combinations of these booleans
#[derive(Debug, Copy, Clone)]
pub struct Kind {
    pub needs_layout: bool,
    // this currently isn't possible in FIDL? though i suppose a handle optionally takes
    // a layout parameter
    // pub can_take_layout: bool,
    pub needs_constraints: bool,
    pub can_take_constraints: bool,
}

impl Kind {
    pub fn base_type() -> Kind {
        Kind {
            needs_layout: false,
            needs_constraints: false,
            can_take_constraints: false,
        }
    }

    pub fn is_concrete(&self) -> bool {
        !self.needs_layout && !self.needs_constraints
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (
            self.needs_layout,
            self.needs_constraints,
            self.can_take_constraints,
        ) {
            (false, false, false) => write!(f, "Type"),
            (false, false, true) => write!(f, "Constraint? -> Type"),
            (false, true, _) => write!(f, "Constraint -> Type"),
            (true, false, false) => write!(f, "Layout -> Type"),
            (true, false, true) => write!(f, "(Layout, Constraint?) -> Type"),
            (true, true, _) => write!(f, "(Layout, Constraint) -> Type"),
        }
    }
}

// this concept should be merged with Scope?
pub trait Namespace {
    fn lookup_term(&self, name: &Name) -> Option<&Term>;
    fn lookup_ty(&self, name: &Name) -> Option<&Type>;
    fn lookup_kind(&self, name: &Name) -> Option<&Kind>;
}

pub fn eval<T: Namespace>(term: &Term, scope: &T) -> Result<Term, Name> {
    use Term::*;
    match term {
        Identifier(ref name) => match scope.lookup_term(name) {
            Some(ref t) => eval(t, scope),
            None => Err(name.clone()),
        },
        _ => Ok(term.clone()),
    }
}

// NOTE: this is currently the same as eval, since we only have identifiers and
// literals
pub fn resolve<T: Namespace>(term: &Term, scope: &T) -> Result<Term, Name> {
    use Term::*;
    match term {
        Identifier(ref name) => match scope.lookup_term(name) {
            Some(ref t) => resolve(t, scope),
            None => Err(name.clone()),
        },
        _ => Ok(term.clone()),
    }
}

// really the scope here should be mapping names to types, so that typechecking an
// identifier is just looking up its type in the scope and comparing to the ascribed type.
// however, this means that we need to keep track of the value inside the
// int type (or, this ident should be findable in the term scope), in order to handle
// cases like this, where y should fail but z should be OK.
// const uint16 x = 300;
// const uint8 y = x;
// const uint32 z = y;
// kindchecking is handled in this way.
// pub fn typecheck<T: Namespace>(term: &Term, scope: &T) -> Result<Type, ()> {
//     match term {
//         Term::Identifier(_) => {
//             let resolved = resolve(term, scope).map_err(|_| ())?;
//             // this comes from the scope
//             let expected_ty = unimplemented!();
//             match (resolved, &expected_ty) {
//                 // TODO: check bounds. to do this, we need to kind check and
//                 // type eval the bounds
//                 (Term::Str(_), Type::Str(_bounds)) => Ok(expected_ty),
//                 (Term::Int(val), Type::Int8) => i8::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::Int16) => i16::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::Int32) => i32::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::Int64) => i64::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::UInt8) => u8::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::UInt16) => u16::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::UInt32) => u32::try_from(val).map(|_| expected_ty),
//                 (Term::Int(val), Type::UInt64) => u64::try_from(val).map(|_| expected_ty),
//                 (Term::Float(_), Type::Float32)
//                 | (Term::Float(_), Type::Float64)
//                 | (Term::True, Type::Bool)
//                 | (Term::False, Type::Bool) => Ok(Type::Bool),
//                 (_, Type::Int) => panic!("users can't specify untyped Ints"),
//                 (Term::Identifier(_), _) => panic!("should be fully resolved"),
//                 _ => Err(()),
//             }
//         }
//         Term::Str(_) => Ok(Type::Str(Str { bounds: None })),
//         Term::Int(_) => Ok(Type::Int),
//         Term::Float(_) => Ok(Type::Float64),
//         Term::True | Term::False => Ok(Type::Bool),
//     }
// }

// TODO: make evalable a trait? or move this to a method?
// TODO: do we fuly evaluate the terms as well?
pub fn eval_ty<T: Namespace>(ty: &Type, scope: &T) -> Result<Type, Name> {
    match ty {
        Type::Identifier(ref name) => match scope.lookup_ty(name) {
            Some(ref ty) => eval_ty(ty, scope),
            None => Err(name.clone()),
        },
        // TODO: these are kind of uninteresting, just recursively resolving
        // the inner types
        Type::Struct(_) => unimplemented!(),
        Type::Bits(_) => unimplemented!(),
        Type::Enum(_) => unimplemented!(),
        Type::Table(_) => unimplemented!(),
        Type::Union(_) => unimplemented!(),
        Type::Ptr(_) => unimplemented!(),
        Type::Array(_) => unimplemented!(),
        Type::Vector(_) => unimplemented!(),
        // this is the more interesting one. "type operator application"
        Type::TypeSubstitution(sub) => {
            let TypeSubstitution {
                func,
                layout,
                constraint,
            } = sub;
            // we can simplify this if we assume that kind checking passed before
            // calling eval_ty
            let layout_arg = layout.clone().map(|spanned| spanned.value);
            let constraint_arg = constraint.clone().map(|spanned| spanned.value);
            match eval_ty(func, scope)? {
                Type::Array(Array {
                    ref element_type,
                    ref size,
                }) => Ok({
                    if (element_type.is_some() ^ layout_arg.is_some())
                        && (size.is_some() ^ constraint_arg.is_some())
                    {
                        let element_type = element_type.clone().or(layout_arg);
                        let size = size.clone().or(constraint_arg);
                        Type::Array(Array { element_type, size })
                    } else {
                        ty.clone()
                    }
                }),
                Type::Vector(_) => unimplemented!(),
                Type::Str(_) => unimplemented!(),
                // stuck type
                _ => Ok(ty.clone()),
            }
        }
        // do these eval further? having a valid protocol here is not "checked"
        // in either type eval or in kind checking.
        // Str(_),
        // ClientEnd(_),
        // ServerEnd(_)
        _ => Ok(ty.clone()),
    }
}

pub fn kind_check<T: Namespace>(ty: &Type, scope: &T) -> Result<Kind, Name> {
    match ty {
        // TODO: finish implementing, and also share this code.
        Type::Struct(val) => {
            let _are_members_valid = val
                .members
                .iter()
                .map(|spanned| kind_check(&spanned.value.ty.value, scope))
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .all(|kind| kind.is_concrete());
            unimplemented!()
        }
        Type::Bits(_) => unimplemented!(),
        Type::Enum(_) => unimplemented!(),
        Type::Table(_) => unimplemented!(),
        Type::Union(_) => unimplemented!(),
        Type::Ptr(_) => unimplemented!(),
        Type::Identifier(name) => match scope.lookup_kind(name) {
            Some(kind) => Ok(*kind),
            None => Err(name.clone()),
        },
        Type::Array(Array { element_type, size }) => Ok(Kind {
            needs_layout: element_type.is_none(),
            needs_constraints: size.is_none(),
            can_take_constraints: false,
        }),
        Type::Vector(Vector {
            element_type,
            bounds,
        }) => Ok(Kind {
            needs_layout: element_type.is_none(),
            needs_constraints: false,
            can_take_constraints: bounds.is_none(),
        }),
        Type::Str(Str { bounds }) => Ok(Kind {
            needs_layout: false,
            needs_constraints: false,
            can_take_constraints: bounds.is_none(),
        }),
        // TODO: do we want to check that they refer to actual protocols? this isn't really a "kind"
        // check... but it is a check that the type is valid.
        Type::ClientEnd(_) => unimplemented!(),
        Type::ServerEnd(_) => unimplemented!(),
        Type::TypeSubstitution(TypeSubstitution {
            func,
            layout,
            constraint,
        }) => {
            let func_kind = kind_check(func, scope)?;
            // TODO: we return errors here. we may want to distinguish between the 3 error cases
            // (already has layout, already has constraints, already has both)
            if !func_kind.needs_layout && layout.is_some() {
                unimplemented!()
            } else if !(func_kind.needs_constraints || func_kind.can_take_constraints)
                && constraint.is_some()
            {
                unimplemented!()
            } else {
                // NOTE: we error if an argument is provided that is not supported by the func type,
                // but don't if an argument that is "needed" is not provided (so we provide some
                // "currying" like behavior to match fidlc's type constructors)
                Ok(Kind {
                    needs_layout: func_kind.needs_layout && layout.is_none(),
                    needs_constraints: func_kind.needs_constraints && constraint.is_none(),
                    can_take_constraints: func_kind.can_take_constraints && constraint.is_none(),
                })
            }
        }
        _ => Ok(Kind::base_type()),
    }
}

impl TryFrom<IntLiteral> for i64 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        if value.is_negative {
            match value.value.cmp(&(std::i64::MIN as u64)) {
                Ordering::Less => Ok(value.value as i64 * -1),
                Ordering::Equal => Ok(value.value as i64),
                Ordering::Greater => Err(()),
            }
        } else {
            match value.value.cmp(&(std::i64::MAX as u64)) {
                Ordering::Less | Ordering::Equal => Ok(value.value as i64),
                Ordering::Greater => Err(()),
            }
        }
    }
}

impl TryFrom<IntLiteral> for i32 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        i64::try_from(value).and_then(|val| i32::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for i16 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        i64::try_from(value).and_then(|val| i16::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for i8 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        i64::try_from(value).and_then(|val| i8::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for u64 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        if value.is_negative {
            Err(())
        } else {
            Ok(value.value)
        }
    }
}

impl TryFrom<IntLiteral> for u32 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        u64::try_from(value).and_then(|val| u32::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for u16 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        u64::try_from(value).and_then(|val| u16::try_from(val).map_err(|_| ()))
    }
}

impl TryFrom<IntLiteral> for u8 {
    type Error = ();

    fn try_from(value: IntLiteral) -> Result<Self, Self::Error> {
        u64::try_from(value).and_then(|val| u8::try_from(val).map_err(|_| ()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn int_literal_try_from() {
        assert_eq!(
            i64::try_from(IntLiteral {
                value: std::i64::MAX as u64,
                is_negative: false
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i64::try_from(IntLiteral {
                value: std::i64::MIN as u64,
                is_negative: false
            })
            .is_err(),
            true
        );
        assert_eq!(
            i64::try_from(IntLiteral {
                value: std::i64::MIN as u64,
                is_negative: true
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i64::try_from(IntLiteral {
                value: (std::i64::MIN as u64) + 1,
                is_negative: true
            })
            .is_err(),
            true
        );

        assert_eq!(
            i32::try_from(IntLiteral {
                value: std::i32::MIN as u64,
                is_negative: false,
            })
            .is_err(),
            true
        );
        assert_eq!(
            i32::try_from(IntLiteral {
                value: std::i32::MAX as u64,
                is_negative: false,
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i32::try_from(IntLiteral {
                value: std::i32::MIN as u32 as u64,
                is_negative: true,
            })
            .is_ok(),
            true
        );
        assert_eq!(
            i32::try_from(IntLiteral {
                value: (std::i32::MIN as u32 as u64) + 1,
                is_negative: true,
            })
            .is_err(),
            true
        );
    }
}
