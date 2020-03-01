use crate::flat;
use crate::flat::PrimitiveSubtype::*;
use crate::raw::Spanned;
use serde::{Deserialize, Serialize};
use std::cmp;
use std::convert::TryFrom;

pub fn typeshape(ty: &Type, wire_format: WireFormat) -> TypeShape {
    let unalined_size = unaligned_size(ty, wire_format);
    let alignment = alignment(ty, false, wire_format);
    TypeShape {
        inline_size: align_to(unalined_size, alignment),
        alignment: alignment,
        depth: depth(ty, wire_format),
        max_handles: max_handles(ty, wire_format),
        max_out_of_line: max_out_of_line(ty, wire_format),
        has_padding: has_padding(ty, false, wire_format),
        has_flexible_envelope: has_flexible_envelope(ty, wire_format),
        contains_union: contains_union(ty, wire_format),
    }
}

#[derive(Debug, Copy, Clone)]
pub enum WireFormat {
    V1,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeShape {
    /// The inline size of this type, including padding for the type's minimum alignment. For
    /// example, "struct S { uint32 a; uint16 b; };" will have an inline_size of 8, not 6: the
    /// "packed" size of the struct is 6, but the alignment of its largest member is 4, so 6 is
    /// rounded up to 8.
    inline_size: u32,
    /// The minimum alignment required by this type.
    alignment: u32,
    /// These values are calculated incorporating both the current TypeShape, and recursively over
    /// all child fields. A value of std::numeric_limits<uint32_t>::max() means that the value is
    /// potentially unbounded, which can happen for self-recursive aggregate objects. For flexible
    /// types, these values is calculated based on the currently-defined members, and does _not_
    /// take potential future members into account.
    depth: u32,
    max_handles: u32,
    max_out_of_line: u32,
    /// `has_padding` is true if this type has _either_ inline or out-of-line padding. For flexible
    /// types, `has_padding` is calculated based on the currently-defined members, and does _not_
    /// take potential future members into account. (If it did, `has_padding` would have to be true
    /// for all flexible types, which doesn't make it very useful.)
    has_padding: bool,

    has_flexible_envelope: bool,
    /// Whether this type transitively contains a union. If this is false, union/xunion
    /// transformations can be avoided
    contains_union: bool,
}

/// `FieldShape` describes the offset and padding information for members that are contained within
/// an aggregate type (e.g. struct/union). TODO(fxb/36337): We can update `FieldShape` to be a
/// simple offset+padding struct, and remove the getter/setter methods since they're purely for
/// backward-compatibility with existing code.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldShape {
    offset: u32,
    padding: u32,
}

#[derive(Debug, Clone)]
pub enum Type {
    /// The type that contains all of its elements. Second parameter indicates
    /// whether this is a flexible envelope
    Product(Vec<Box<Type>>, bool),
    /// The type that may contain any one of its elements
    Sum(Vec<Box<Type>>),

    /// An Array is just a Product with the same type multiple times
    Array(Box<Type>, u64),

    /// A 64bit pointer that MAY point to out of line content
    Ptr(Box<Type>),
    /// A 64bit pointer that MUST point to out of line content
    Boxed(Box<Type>),

    /// A 32 bit handle
    Handle,
    /// A primitive type
    Primitive(flat::PrimitiveSubtype),
}

// TODO: this should probably take a Spanned<Box<Type>>
pub fn desugar(ty: &flat::Type, scope: &flat::Libraries, nullable: bool) -> Type {
    match ty {
        flat::Type::Struct(val) => {
            let result = Type::Product(
                val.members
                    .iter()
                    .map(|sp| Box::new(desugar(&*sp.value.ty.value, scope, false)))
                    .collect(),
                false,
            );
            if nullable {
                Type::Ptr(Box::new(result))
            } else {
                result
            }
        }
        flat::Type::Bits(val) => desugar(val.get_type(), scope, false),
        flat::Type::Enum(val) => desugar(val.get_type(), scope, false),
        flat::Type::Table(table) => {
            let member_types = table
                .members
                .iter()
                .filter_map(|member| match &member.value.inner {
                    flat::TableMemberInner::Reserved => None,
                    flat::TableMemberInner::Used { ty, .. } => Some(Box::new(envelope(
                        desugar(&*ty.value, scope, false),
                        table.strictness().is_flexible(),
                    ))),
                })
                .collect();
            Type::Product(
                vec![
                    Box::new(Type::Primitive(UInt64)), // num elements
                    Box::new(Type::Boxed(Box::new(Type::Product(member_types, false)))),
                ],
                false,
            )
        }
        flat::Type::Union(union) => {
            let variants = union
                .members
                .iter()
                .filter_map(|member| match &member.value.inner {
                    flat::UnionMemberInner::Reserved => None,
                    flat::UnionMemberInner::Used { ty, .. } => {
                        Some(Box::new(desugar(&*ty.value, scope, false)))
                    }
                })
                .collect();
            let inner = if nullable {
                envelope(Type::Sum(variants), union.strictness().is_flexible())
            } else {
                nn_envelope(Type::Sum(variants), union.strictness().is_flexible())
            };
            Type::Product(
                vec![
                    Box::new(Type::Primitive(UInt64)), // ordinal
                    Box::new(inner),
                ],
                false,
            )
        }
        flat::Type::Identifier(name) => desugar(
            &scope.get_type(flat::dummy_span(name)).unwrap().value,
            scope,
            nullable,
        ),
        flat::Type::Ptr(val) => desugar(&*val.value, scope, true),
        flat::Type::Array(flat::Array { element_type, size }) => Type::Array(
            Box::new(desugar(
                &*element_type.as_ref().unwrap().value,
                scope,
                false,
            )),
            eval_size(size, scope),
        ),
        flat::Type::Vector(flat::Vector {
            element_type,
            bounds,
        }) => {
            let ty = Box::new(desugar(
                &*element_type.as_ref().unwrap().value,
                scope,
                false,
            ));
            let size = eval_size(bounds, scope);
            if nullable {
                vector(ty, size)
            } else {
                nn_vector(ty, size)
            }
        }
        flat::Type::Str(flat::Str { bounds }) => {
            let ty = Box::new(Type::Primitive(UInt8));
            let size = eval_size(bounds, scope);
            if nullable {
                vector(ty, size)
            } else {
                nn_vector(ty, size)
            }
        }
        flat::Type::Handle(_) | flat::Type::ClientEnd(_) | flat::Type::ServerEnd(_) => Type::Handle,
        flat::Type::Primitive(subtype) => Type::Primitive(*subtype),

        // should panic or return error
        flat::Type::Any | flat::Type::TypeSubstitution(_) | flat::Type::Int => unimplemented!(),
    }
}

fn nn_envelope(ty: Type, is_flexible: bool) -> Type {
    Type::Product(
        vec![
            Box::new(Type::Primitive(UInt32)),
            Box::new(Type::Primitive(UInt32)),
            Box::new(Type::Boxed(Box::new(ty))),
        ],
        is_flexible,
    )
}

fn envelope(ty: Type, is_flexible: bool) -> Type {
    Type::Product(
        vec![
            Box::new(Type::Primitive(UInt32)),
            Box::new(Type::Primitive(UInt32)),
            Box::new(Type::Ptr(Box::new(ty))),
        ],
        is_flexible,
    )
}

fn nn_vector(element_type: Box<Type>, bounds: u64) -> Type {
    Type::Product(
        vec![
            Box::new(Type::Primitive(UInt64)), // num elements
            Box::new(Type::Boxed(Box::new(Type::Array(element_type, bounds)))),
        ],
        false,
    )
}

fn vector(element_type: Box<Type>, bounds: u64) -> Type {
    Type::Product(
        vec![
            Box::new(Type::Primitive(UInt64)), // num elements
            Box::new(Type::Ptr(Box::new(Type::Array(element_type, bounds)))),
        ],
        false,
    )
}

fn eval_size(term: &Option<Spanned<Box<flat::Term>>>, scope: &flat::Libraries) -> u64 {
    term.as_ref().map_or(std::u64::MAX, |term| {
        let result = flat::eval_term(term.into(), scope).unwrap();
        if let flat::Term::Int(val) = result.value {
            u64::try_from(*val).unwrap()
        } else {
            panic!("you dun goofed")
        }
    })
}

fn unaligned_size(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Product(members, _) => {
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
        Array(ty, size) => unaligned_size(&*ty, wire_format).saturating_mul(*size as u32),
        Ptr(_) | Boxed(_) => 8,
        Handle => 4,
        Primitive(subtype) => match subtype {
            Bool | Int8 | UInt8 => 1,
            Int16 | UInt16 => 2,
            Int32 | UInt32 | Float32 => 4,
            Int64 | UInt64 | Float64 => 8,
        },
    }
}

fn alignment(ty: &Type, ool: bool, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Sum(members) | Product(members, _) => members
            .iter()
            .map(|member| alignment(&*member, ool, wire_format))
            .max()
            .unwrap_or(1),
        Array(ty, _) => alignment(&*ty, ool, wire_format),
        Ptr(_) | Boxed(_) => 8,
        Handle | Primitive(_) => {
            let min = if ool { 8 } else { 0 };
            cmp::max(min, unaligned_size(ty, wire_format))
        }
    }
}

fn depth(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Sum(members) | Product(members, _) => members
            .iter()
            .map(|member| depth(&*member, wire_format))
            .max()
            .unwrap_or(0),
        Array(ty, _) => depth(&*ty, wire_format),
        Ptr(ty) | Boxed(ty) => 1 + depth(&*ty, wire_format),
        Handle | Primitive(_) => 0,
    }
}

fn max_handles(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Sum(members) | Product(members, _) => members
            .iter()
            .map(|member| max_handles(&*member, wire_format))
            .sum::<u32>(),
        Array(ty, size) => max_handles(&*ty, wire_format).saturating_mul(*size as u32),
        Ptr(ty) | Boxed(ty) => max_handles(&*ty, wire_format),
        Handle => 1,
        Primitive(_) => 0,
    }
}

fn max_out_of_line(ty: &Type, wire_format: WireFormat) -> u32 {
    use Type::*;
    match ty {
        Product(members, _) => members
            .iter()
            .map(|member| max_out_of_line(&*member, wire_format))
            .sum::<u32>(),
        Sum(members) => members
            .iter()
            .map(|member| max_out_of_line(&*member, wire_format))
            .max()
            .unwrap_or(0),
        Array(ty, size) => max_out_of_line(&*ty, wire_format).saturating_mul(*size as u32),
        Ptr(ty) | Boxed(ty) => {
            unaligned_size(&*ty, wire_format) + max_out_of_line(&*ty, wire_format)
        }
        Handle | Primitive(_) => 0,
    }
}

// TODO
fn has_padding(ty: &Type, ool: bool, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Product(members, _) => {
            let fieldshapes = fieldshapes(ty, ool, wire_format).unwrap();
            let inline_padding = fieldshapes.iter().any(|shape| shape.padding > 0);
            let inherent_padding = members
                .iter()
                .any(|member| has_padding(member, ool, wire_format));
            inline_padding || inherent_padding
        }
        // TODO(fxb/36331)
        Sum(_) => true,
        Array(ty, _) => {
            let alignment = alignment(&*ty, ool, wire_format);
            let has_trailing_padding = unaligned_size(&*ty, wire_format) % alignment != 0;
            has_padding(&*ty, ool, wire_format) || has_trailing_padding
        }
        Ptr(ty) | Boxed(ty) => has_padding(&*ty, true, wire_format),
        Handle | Primitive(_) => false,
    }
}

fn has_flexible_envelope(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Product(_, true) => true,
        Sum(members) | Product(members, _) => members
            .iter()
            .any(|member| has_flexible_envelope(&*member, wire_format)),
        Boxed(ty) | Array(ty, _) | Ptr(ty) => has_flexible_envelope(&*ty, wire_format),
        Handle | Primitive(_) => false,
    }
}

fn contains_union(ty: &Type, wire_format: WireFormat) -> bool {
    use Type::*;
    match ty {
        Product(members, _) => members
            .iter()
            .any(|member| contains_union(&*member, wire_format)),
        // currently only a union can desugar to a sum
        Sum(_) => true,
        Array(ty, _) | Ptr(ty) | Boxed(ty) => contains_union(&*ty, wire_format),
        Handle | Primitive(_) => false,
    }
}

pub fn fieldshapes(ty: &Type, ool: bool, wire_format: WireFormat) -> Option<Vec<FieldShape>> {
    use Type::*;
    match ty {
        Product(members, _) => {
            // alignments[i] is the alignment that member[i] needs to pad to;
            // for all members it's the alignment of the next member except for
            // the last member, where it's the alignment of the object
            let mut alignments: Vec<u32> = members
                .iter()
                .skip(1)
                .map(|member| alignment(member, ool, wire_format))
                .collect();
            alignments.push(alignment(ty, ool, wire_format));

            let mut offset = 0;
            Some(
                members
                    .iter()
                    .enumerate()
                    .map(|(i, member)| {
                        let size = unaligned_size(&*member, wire_format);
                        let padding = padding(offset + size, alignments[i]);
                        let ret = FieldShape { offset, padding };
                        offset += size + padding;
                        ret
                    })
                    .collect(),
            )
        }
        _ => None,
    }
}

fn padding(offset: u32, alignment: u32) -> u32 {
    (!offset + 1) & (alignment - 1)
}

// Some things are probably incorrect because the alignment of OOL objects and how it
// works is not clear to me
// fn object_align(size: u32) -> u32 {
//     align_to(size, 8)
// }

fn align_to(size: u32, alignment: u32) -> u32 {
    (size + (alignment - 1)) & !(alignment - 1)
}
