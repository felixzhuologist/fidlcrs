use crate::flat;
use crate::flat::PrimitiveSubtype::*;
use crate::flat::Type;
use crate::raw::Strictness;

#[derive(Debug, Copy, Clone)]
pub enum WireFormat {
    V1,
}

// #[derive(Debug, Clone)]
// pub enum Type {
//     Product(Vec<Box<Type>>);
//     Sum(Vec<Box<Type>>);
//     Envelope(Box<Type>);
//     NNEnvelope(Box<Type>);
//     Ptr(Box<Type>);
//     Array(Box<Type>, u32);
//     Vector(Box<Type> u32);
//     Handle,
//     Primitive(flat::PrimitiveSubtype),
// }

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
        Struct(val) => {
            if val.members.is_empty() {
                return 1;
            }
            val.members
                .iter()
                .map(|member| unaligned_size(&*member.value.ty.value, wire_format))
                .sum()
        }
        Bits(val) => unaligned_size(val.get_type(), wire_format),
        Enum(val) => unaligned_size(val.get_type(), wire_format),
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        Ptr(_) => 8,
        Union(_) => 24,
        Table(_) | Vector(_) | Str(_) => 16,
        Array(flat::Array {
            element_type: _,
            size: _,
        }) => unimplemented!(),
        ClientEnd(_) | ServerEnd(_) | Handle(_) => 4,
        Primitive(subtype) => match subtype {
            Bool | Int8 | UInt8 => 1,
            Int16 | UInt16 => 2,
            Int32 | UInt32 | Float32 => 4,
            Int64 | UInt64 | Float64 => 8,
        },
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn alignment(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Struct(val) => {
            if val.members.is_empty() {
                return 1;
            }
            val.members
                .iter()
                .map(|member| alignment(&*member.value.ty.value, wire_format))
                .max()
                .unwrap()
        }
        Bits(val) => alignment(val.get_type(), wire_format),
        Enum(val) => alignment(val.get_type(), wire_format),
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        Ptr(_) => 8,
        Union(_) => 8,
        Table(_) | Vector(_) | Str(_) => 8,
        Array(flat::Array {
            element_type,
            size: _,
        }) => alignment(element_type.as_ref().unwrap(), wire_format),
        ClientEnd(_) | ServerEnd(_) | Handle(_) => 4,
        Primitive(_) => unaligned_size(ty, wire_format),
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn depth(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    // TODO: handle recursion
    match ty {
        Struct(val) => val
            .members
            .iter()
            .map(|member| depth(&*member.value.ty.value, wire_format))
            .max()
            .unwrap_or(0),
        Table(val) => {
            1 + val
                .members
                .iter()
                .map(|member| match &member.value.inner {
                    flat::TableMemberInner::Reserved => 0,
                    flat::TableMemberInner::Used {
                        ty,
                        name: _,
                        default_value: _,
                    } => depth(&*ty.value, wire_format),
                })
                .max()
                .unwrap_or(0)
        }
        // do we want to model this as an envelope? if so how would that work?
        Union(val) => {
            1 + val
                .members
                .iter()
                .map(|member| match &member.value.inner {
                    flat::UnionMemberInner::Reserved => 0,
                    flat::UnionMemberInner::Used { ty, name: _ } => depth(&*ty.value, wire_format),
                })
                .max()
                .unwrap_or(0)
        }
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        Ptr(ty) => 1 + depth(&*ty, wire_format),
        Str(_) => 1,
        Vector(flat::Vector {
            element_type,
            bounds: _,
        }) => 1 + depth(element_type.as_ref().unwrap(), wire_format),
        Array(flat::Array {
            element_type,
            size: _,
        }) => depth(element_type.as_ref().unwrap(), wire_format),
        Bits(_) | Enum(_) | ClientEnd(_) | ServerEnd(_) | Handle(_) | Primitive(_) => 0,
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn max_handles(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    // TODO: handle recursion
    match ty {
        Struct(val) => val
            .members
            .iter()
            .map(|member| max_handles(&*member.value.ty.value, wire_format))
            .sum::<u32>(),
        Table(val) => val
            .members
            .iter()
            .map(|member| match &member.value.inner {
                flat::TableMemberInner::Reserved => 0,
                flat::TableMemberInner::Used {
                    ty,
                    name: _,
                    default_value: _,
                } => max_handles(&*ty.value, wire_format),
            })
            .sum::<u32>(),
        // do we want to model this as an envelope? if so how would that work?
        Union(val) => val
            .members
            .iter()
            .map(|member| match &member.value.inner {
                flat::UnionMemberInner::Reserved => 0,
                flat::UnionMemberInner::Used { ty, name: _ } => {
                    max_handles(&*ty.value, wire_format)
                }
            })
            .max()
            .unwrap_or(0),
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        Ptr(ty) => max_handles(&*ty, wire_format),
        Str(_) => 0,
        Vector(_) | Array(_) => unimplemented!(),
        ClientEnd(_) | ServerEnd(_) | Handle(_) => 1,
        Bits(_) | Enum(_) | Primitive(_) => 0,
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn max_out_of_line(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    // TODO: handle recursion
    match ty {
        Struct(val) => val
            .members
            .iter()
            .map(|member| max_out_of_line(&*member.value.ty.value, wire_format))
            .sum::<u32>(),
        Table(val) => val
            .members
            .iter()
            .map(|member| match &member.value.inner {
                // TODO(fxb/35773) this excludes reserved table members from the OOL calculations
                // which is incorrect
                flat::TableMemberInner::Reserved => 0,
                flat::TableMemberInner::Used {
                    ty,
                    name: _,
                    default_value: _,
                } => {
                    object_align(unaligned_size(&*ty.value, wire_format))
                        + max_out_of_line(&*ty.value, wire_format)
                        + 16
                }
            })
            .sum::<u32>(),
        // do we want to model this as an envelope? if so how would that work?
        Union(val) => val
            .members
            .iter()
            .map(|member| match &member.value.inner {
                flat::UnionMemberInner::Reserved => 0,
                flat::UnionMemberInner::Used { ty, name: _ } => {
                    object_align(unaligned_size(&*ty.value, wire_format))
                        + max_out_of_line(&*ty.value, wire_format)
                }
            })
            .max()
            .unwrap_or(0),
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        // TODO: do we want to check that some things can't be nullable at this step?
        Ptr(ty) => unaligned_size(&*ty, wire_format) + max_out_of_line(&*ty, wire_format),
        Str(_) => unimplemented!(),
        Vector(_) => unimplemented!(),
        Array(_) => unimplemented!(),
        Bits(_) | Enum(_) | ClientEnd(_) | ServerEnd(_) | Handle(_) | Primitive(_) => 0,
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn has_padding(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Struct(val) => val
            .members
            .iter()
            .any(|member| has_padding(&*member.value.ty.value, wire_format)),
        Table(val) => val.members.iter().any(|member| match &member.value.inner {
            // TODO(fxb/35773) this excludes reserved table members from the OOL calculations
            // which is incorrect
            flat::TableMemberInner::Reserved => false,
            flat::TableMemberInner::Used {
                ty,
                name: _,
                default_value: _,
            } => has_padding(&*ty.value, wire_format),
        }),
        // TODO(fxb/36331)
        Union(_) => true,
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        Ptr(ty) => has_padding(&*ty, wire_format),
        Str(_) => true,
        Vector(_) => unimplemented!(),
        Array(flat::Array {
            element_type,
            size: _,
        }) => has_padding(element_type.as_ref().unwrap(), wire_format),
        Bits(_) | Enum(_) | ClientEnd(_) | ServerEnd(_) | Handle(_) | Primitive(_) => false,
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn has_flexible_envelope(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Struct(val) => val
            .members
            .iter()
            .any(|member| has_flexible_envelope(&*member.value.ty.value, wire_format)),
        Table(val) => {
            val.strictness() == Strictness::Flexible
                || val.members.iter().any(|member| match &member.value.inner {
                    flat::TableMemberInner::Reserved => false,
                    flat::TableMemberInner::Used {
                        ty,
                        name: _,
                        default_value: _,
                    } => has_flexible_envelope(&*ty.value, wire_format),
                })
        }
        Union(val) => {
            val.strictness() == Strictness::Flexible
                || val.members.iter().any(|member| match &member.value.inner {
                    flat::UnionMemberInner::Reserved => false,
                    flat::UnionMemberInner::Used { ty, name: _ } => {
                        has_flexible_envelope(&*ty.value, wire_format)
                    }
                })
        }
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        // ??
        Ptr(ty) => has_flexible_envelope(&*ty, wire_format),
        Vector(flat::Vector {
            element_type,
            bounds: _,
        })
        | Array(flat::Array {
            element_type,
            size: _,
        }) => has_padding(element_type.as_ref().unwrap(), wire_format),
        Bits(_) | Enum(_) | Str(_) | ClientEnd(_) | ServerEnd(_) | Handle(_) | Primitive(_) => {
            false
        }
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn contains_union(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Struct(val) => val
            .members
            .iter()
            .any(|member| contains_union(&*member.value.ty.value, wire_format)),
        Table(val) => val.members.iter().any(|member| match &member.value.inner {
            flat::TableMemberInner::Reserved => false,
            flat::TableMemberInner::Used {
                ty,
                name: _,
                default_value: _,
            } => contains_union(&*ty.value, wire_format),
        }),
        Union(_) => true,
        // TODO: do this first, so we understand what else this function needs
        Identifier(_) => unimplemented!(),
        // ??
        Ptr(ty) => contains_union(&*ty, wire_format),
        Vector(flat::Vector {
            element_type,
            bounds: _,
        })
        | Array(flat::Array {
            element_type,
            size: _,
        }) => contains_union(element_type.as_ref().unwrap(), wire_format),
        Bits(_) | Enum(_) | Str(_) | ClientEnd(_) | ServerEnd(_) | Handle(_) | Primitive(_) => {
            false
        }
        Int => panic!("untyped ints don't have a typeshape"),
        // TODO: is this valid? maybe in the repl
        TypeSubstitution(_) => unimplemented!(),
    }
}

fn object_align(size: u32) -> u32 {
    align_to(size, 8)
}

fn align_to(size: u32, alignment: u32) -> u32 {
    (size + (alignment - 1)) & !(alignment - 1)
}
