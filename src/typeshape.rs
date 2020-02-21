use crate::flat;
use crate::flat::PrimitiveSubtype::*;

#[derive(Debug, Clone)]
pub enum Type {
    Product(Vec<Box<Type>>),
    Sum(Vec<Box<Type>>),
    /// Type the envelope contains, is_nullable, is_flexible
    Envelope(Box<Type>, bool, bool),
    Ptr(Box<Type>),
    Array(Box<Type>, u32),
    // Really, a DynVector is a vector, and a Vector is just a shortcut for
    // a DynVector with only one element type. We need DynVector to be able to
    // represent tables, where each element can be an Envelope of a different type.
    // TODO: does a DynVector have bounds? technically the size if fixed for tables.
    // TODO: if no bounds is specified, then we'd use u32::MAX here, which means
    // certain operations will overflow. need to update to use saturating ops instead
    DynVector(Vec<Box<Type>>, u32),
    // TODO: can a vector be expressed as an envelope<array>? since we only get max
    // out of line
    Vector(Box<Type>, u32),
    Handle,
    Primitive(flat::PrimitiveSubtype),
}

pub fn desugar(ty: &flat::Type) -> Type {
    match ty {
        flat::Type::Struct(val) => Type::Product(
            val.members
                .iter()
                .map(|sp| Box::new(desugar(&*sp.value.ty.value)))
                .collect(),
        ),
        flat::Type::Bits(val) => desugar(val.get_type()),
        flat::Type::Enum(val) => desugar(val.get_type()),
        flat::Type::Table(_) => unimplemented!(),
        flat::Type::Union(_) => unimplemented!(),
        flat::Type::Identifier(_) => unimplemented!(),
        flat::Type::Ptr(ty) => Type::Ptr(Box::new(desugar(&*ty))),
        flat::Type::Array(_) => unimplemented!(),
        flat::Type::Vector(_) => unimplemented!(),
        flat::Type::Str(_) => unimplemented!(),
        flat::Type::Handle(_) | flat::Type::ClientEnd(_) | flat::Type::ServerEnd(_) => Type::Handle,
        flat::Type::Primitive(subtype) => Type::Primitive(*subtype),

        // should panic or return error
        flat::Type::TypeSubstitution(_) | flat::Type::Int => unimplemented!(),
    }
}

#[derive(Debug, Copy, Clone)]
pub enum WireFormat {
    V1,
}

#[derive(Debug, Clone)]
pub struct TypeShape {
    /// The inline size of this type, including padding for the type's minimum alignment. For
    /// example, "struct S { uint32 a; uint16 b; };" will have an inline_size of 8, not 6: the
    /// "packed" size of the struct is 6, but the alignment of its largest member is 4, so 6 is
    /// rounded up to 8.
    pub inline_size: u32,
    /// The minimum alignment required by this type.
    pub alignment: u32,
    /// These values are calculated incorporating both the current TypeShape, and recursively over
    /// all child fields. A value of std::numeric_limits<uint32_t>::max() means that the value is
    /// potentially unbounded, which can happen for self-recursive aggregate objects. For flexible
    /// types, these values is calculated based on the currently-defined members, and does _not_
    /// take potential future members into account.
    pub depth: u32,
    pub max_handles: u32,
    pub max_out_of_line: u32,
    /// `has_padding` is true if this type has _either_ inline or out-of-line padding. For flexible
    /// types, `has_padding` is calculated based on the currently-defined members, and does _not_
    /// take potential future members into account. (If it did, `has_padding` would have to be true
    /// for all flexible types, which doesn't make it very useful.)
    pub has_padding: bool,

    pub has_flexible_envelope: bool,
    /// Whether this type transitively contains a union. If this is false, union/xunion
    /// transformations can be avoided
    pub contains_union: bool,
}

/// `FieldShape` describes the offset and padding information for members that are contained within
/// an aggregate type (e.g. struct/union). TODO(fxb/36337): We can update `FieldShape` to be a
/// simple offset+padding struct, and remove the getter/setter methods since they're purely for
/// backward-compatibility with existing code.
#[derive(Debug, Clone)]
pub struct FieldShape {
    pub offset: u32,
    pub padding: u32,
}

pub fn typeshape(ty: &Type, wire_format: WireFormat) -> TypeShape {
    let unalined_size = unaligned_size(ty, wire_format);
    let alignment = alignment(ty, wire_format);
    TypeShape {
        inline_size: align_to(unalined_size, alignment),
        alignment: alignment,
        depth: depth(ty, wire_format),
        max_handles: max_handles(ty, wire_format),
        max_out_of_line: max_out_of_line(ty, wire_format),
        has_padding: has_padding(ty, wire_format),
        has_flexible_envelope: has_flexible_envelope(ty, wire_format),
        contains_union: contains_union(ty, wire_format),
    }
}

fn unaligned_size(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Product(members) => {
            if members.is_empty() {
                return 1;
            }
            members
                .iter()
                .map(|member| unaligned_size(&*member, wire_format))
                .sum()
        }
        Sum(members) => members
            .iter()
            .map(|member| unaligned_size(&*member, wire_format))
            .max()
            .unwrap_or(1),
        Ptr(_) => 8,
        Envelope(_, _, _) => 16,
        DynVector(_, _) | Vector(_, _) => 16,
        Array(ty, size) => unaligned_size(&*ty, wire_format) * size,
        Handle => 4,
        Primitive(subtype) => match subtype {
            Bool | Int8 | UInt8 => 1,
            Int16 | UInt16 => 2,
            Int32 | UInt32 | Float32 => 4,
            Int64 | UInt64 | Float64 => 8,
        },
    }
}

fn alignment(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Sum(members) | Product(members) => members
            .iter()
            .map(|member| alignment(&*member, wire_format))
            .max()
            .unwrap_or(1),
        Ptr(_) => 8,
        Envelope(_, _, _) => 8,
        DynVector(_, _) | Vector(_, _) => 8,
        Array(ty, _) => alignment(&*ty, wire_format),
        Handle | Primitive(_) => unaligned_size(ty, wire_format),
    }
}

fn depth(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Sum(members) | Product(members) => members
            .iter()
            .map(|member| depth(&*member, wire_format))
            .max()
            .unwrap_or(0),
        DynVector(members, _) => {
            1 + members
                .iter()
                .map(|member| depth(&*member, wire_format))
                .max()
                .unwrap_or(0)
        }
        Envelope(ty, _, _) | Ptr(ty) | Vector(ty, _) | Array(ty, _) => 1 + depth(&*ty, wire_format),
        Handle | Primitive(_) => 0,
    }
}

fn max_handles(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        DynVector(members, _) | Sum(members) | Product(members) => members
            .iter()
            .map(|member| max_handles(&*member, wire_format))
            .sum::<u32>(),
        Envelope(ty, _, _) | Vector(ty, _) | Array(ty, _) | Ptr(ty) => {
            max_handles(&*ty, wire_format)
        }
        Primitive(_) => 0,
        Handle => 1,
    }
}

fn max_out_of_line(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Product(members) => members
            .iter()
            .map(|member| max_out_of_line(&*member, wire_format))
            .sum::<u32>(),
        Sum(members) => members
            .iter()
            .map(|member| max_out_of_line(&*member, wire_format))
            .max()
            .unwrap_or(0),
        Vector(ty, bounds) => {
            object_align(max_out_of_line(&*ty, wire_format)) * bounds
                + object_align(unaligned_size(&*ty, wire_format)) * bounds
        }
        DynVector(members, _) => members
            .iter()
            .map(|member| {
                object_align(
                    max_out_of_line(&*member, wire_format)
                        + object_align(unaligned_size(&*ty, wire_format)),
                )
            })
            .sum::<u32>(),
        Envelope(ty, _, _) | Ptr(ty) => {
            unaligned_size(&*ty, wire_format) + max_out_of_line(&*ty, wire_format)
        }
        Array(ty, size) => max_out_of_line(&*ty, wire_format) * size,
        Handle | Primitive(_) => 0,
    }
}

fn has_padding(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Product(members) => members
            .iter()
            .any(|member| has_padding(&*member, wire_format)),
        // TODO(fxb/36331)
        Sum(_) => true,
        // do envelopes inherently have padding?
        Envelope(ty, _, _) | Array(ty, _) | Ptr(ty) => has_padding(&*ty, wire_format),
        Vector(ty, _) => {
            has_padding(&*ty, wire_format) || unaligned_size(&*ty, wire_format) % 8 != 0
        }
        DynVector(members, _) => members.iter().any(|member| {
            has_padding(&*member, wire_format) || unaligned_size(&*member, wire_format) % 8 != 0
        }),
        Handle | Primitive(_) => false,
    }
}

fn has_flexible_envelope(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        DynVector(members, _) | Sum(members) | Product(members) => members
            .iter()
            .any(|member| has_flexible_envelope(&*member, wire_format)),
        Vector(ty, _) | Array(ty, _) | Ptr(ty) => has_flexible_envelope(&*ty, wire_format),
        Envelope(_, _, true) => true,
        Envelope(ty, _, false) => has_flexible_envelope(&*ty, wire_format),
        Handle | Primitive(_) => false,
    }
}

fn contains_union(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        DynVector(members, _) | Product(members) => members
            .iter()
            .any(|member| contains_union(&*member, wire_format)),
        // currently only a union can desugar to a sum
        Sum(_) => true,
        Envelope(ty, _, _) | Vector(ty, _) | Array(ty, _) | Ptr(ty) => {
            contains_union(&*ty, wire_format)
        }
        Handle | Primitive(_) => false,
    }
}

fn object_align(size: u32) -> u32 {
    align_to(size, 8)
}

fn align_to(size: u32, alignment: u32) -> u32 {
    (size + (alignment - 1)) & !(alignment - 1)
}

// TODO: we do need Type::Identifier, in order to check for recursion here...
// Conceptually FIDL will allow any type that can be expressed in the wire format,
// which means that recursive types are OK if they can be finite. In other words,
// it's OK to have an infinite upper bound, but not OK to have an infinite lower bound.
// In practice, what can be expressed in any binding may be more restrictive.
pub fn can_be_finite(ty: &Type) -> bool {
    use Type::*;
    match ty {
        // add to a set of scene names. if it's already there, return false, otherwise, call
        // can_be_finite recursively
        // Identifier(_name) => unimplemented!(),
        Product(members) | Sum(members) => members.iter().all(|m| can_be_finite(&*m)),
        Array(ty, _) | Envelope(ty, false, _) => can_be_finite(&*ty),
        Ptr(_) | Envelope(_, true, _) | DynVector(_, _) | Vector(_, _) | Handle | Primitive(_) => {
            true
        }
    }
}
