use super::errors::Error;
use super::eval::{eval_term, eval_type};
use super::typecheck::term_can_have_type;
use super::Validator;
use crate::flat::PrimitiveSubtype::*;
use crate::flat::*;
use crate::lexer::Span;
use std::collections::HashMap;
use std::convert::TryFrom;

impl Struct {
    pub fn validate(&self, validator: &mut Validator) {
        for member in &self.members {
            if let Err(err) = member.value.validate(validator) {
                validator.errors.push(err);
            }
        }
    }
}

impl StructMember {
    pub fn validate(&self, validator: &mut Validator) -> Result<(), Error> {
        let kind = validator.kind_check((&self.ty).into());
        if !kind.is_concrete() {
            return Err(Error::NonConcreteType {
                span: self.ty.span,
                missing: kind.missing(),
            });
        }
        validator.validate_type((&self.ty).into());

        if let Some(term) = self.default_value.as_ref() {
            return match validator.type_check(term.into()) {
                Type::Any => Ok(()),
                ref actual => {
                    let term = eval_term(term.into(), validator.scope)?;
                    let ty = eval_type((&self.ty).into(), validator.scope)?;
                    term_can_have_type(term, &ty.value, actual, validator)
                }
            };
        }
        Ok(())
    }
}

impl Bits {
    pub fn validate(&self, validator: &mut Validator) {
        if let Some(ty) = self.ty.as_ref() {
            let kind = validator.kind_check(ty.into());
            if !kind.is_concrete() {
                validator.errors.push(Error::NonConcreteType {
                    span: ty.span,
                    missing: kind.missing(),
                });
                return;
            }
            let evaled = eval_type(ty.into(), validator.scope).unwrap();
            match &evaled.value {
                Type::Primitive(UInt8)
                | Type::Primitive(UInt16)
                | Type::Primitive(UInt32)
                | Type::Primitive(UInt64)
                | Type::Any => (),
                _ => {
                    validator.errors.push(Error::InvalidBitsType(evaled));
                    return;
                }
            };
        }

        let expected = self.get_type();
        let mut member_values: HashMap<u64, Span> = HashMap::new();
        for member in &self.members {
            match member.value.validate(validator, expected) {
                Err(err) => validator.errors.push(err),
                Ok(val) => {
                    if let Some(span) = member_values.get(&val) {
                        validator.errors.push(Error::DuplicateMemberValue {
                            original: *span,
                            dupe: member.span,
                            decl_kind: "bits",
                        });
                    } else {
                        member_values.insert(val, member.span);
                    }
                }
            };
        }
    }
}

impl BitsMember {
    pub fn validate(&self, validator: &mut Validator, expected: &Type) -> Result<u64, Error> {
        let actual = validator.type_check((&self.value).into());
        let value = eval_term((&self.value).into(), validator.scope)?;
        term_can_have_type(value, expected, &actual, validator)?;
        if let Term::Int(val) = value.value {
            Ok(u64::try_from(*val).unwrap())
        } else {
            panic!("should be caught by term_can_have_type")
        }
    }
}

impl Enum {
    pub fn validate(&self, validator: &mut Validator) {
        if let Some(ty) = self.ty.as_ref() {
            let kind = validator.kind_check(ty.into());
            if !kind.is_concrete() {
                validator.errors.push(Error::NonConcreteType {
                    span: ty.span,
                    missing: kind.missing(),
                });
                return;
            }
            let evaled = eval_type(ty.into(), validator.scope).unwrap();
            match &evaled.value {
                Type::Primitive(UInt8)
                | Type::Primitive(UInt16)
                | Type::Primitive(UInt32)
                | Type::Primitive(UInt64)
                | Type::Primitive(Int8)
                | Type::Primitive(Int16)
                | Type::Primitive(Int32)
                | Type::Primitive(Int64)
                | Type::Any => (),
                _ => {
                    validator.errors.push(Error::InvalidEnumType(evaled));
                    return;
                }
            };
        }

        let expected = self.get_type();
        let mut member_values: HashMap<i64, Span> = HashMap::new();
        for member in &self.members {
            match member.value.validate(validator, expected) {
                Err(err) => validator.errors.push(err),
                Ok(val) => {
                    if let Some(span) = member_values.get(&val) {
                        validator.errors.push(Error::DuplicateMemberValue {
                            original: *span,
                            dupe: member.span,
                            decl_kind: "enum",
                        });
                    } else {
                        member_values.insert(val, member.span);
                    }
                }
            };
        }
    }
}

impl EnumMember {
    pub fn validate(&self, validator: &mut Validator, expected: &Type) -> Result<i64, Error> {
        let actual = validator.type_check((&self.value).into());
        let value = eval_term((&self.value).into(), validator.scope)?;
        term_can_have_type(value, expected, &actual, validator)?;
        if let Term::Int(val) = value.value {
            Ok(i64::try_from(*val).unwrap())
        } else {
            panic!("should be caught by term_can_have_type")
        }
    }
}

impl Table {
    pub fn validate(&self, validator: &mut Validator) {
        for member in &self.members {
            if let Err(err) = member.value.validate(validator) {
                validator.errors.push(err);
            }
        }
    }
}

impl TableMember {
    pub fn validate(&self, validator: &mut Validator) -> Result<(), Error> {
        if let TableMemberInner::Used { ty, name: _ } = &self.inner {
            let kind = validator.kind_check(ty.into());
            if !kind.is_concrete() {
                return Err(Error::NonConcreteType {
                    span: ty.span,
                    missing: kind.missing(),
                });
            }
            validator.validate_type(ty.into());

            let evaled = eval_type(ty.into(), validator.scope)?;
            if let Type::Ptr(_) = evaled.value {
                return Err(Error::NullableMember {
                    span: evaled.span,
                    decl_kind: "table",
                });
            }
            // TODO: just remove default_values altogether for tables
        }
        Ok(())
    }
}

impl Union {
    pub fn validate(&self, validator: &mut Validator) {
        for member in &self.members {
            if let Err(err) = member.value.validate(validator) {
                validator.errors.push(err);
            }
        }
    }
}

impl UnionMember {
    pub fn validate(&self, validator: &mut Validator) -> Result<(), Error> {
        if let UnionMemberInner::Used { ty, name: _ } = &self.inner {
            let kind = validator.kind_check(ty.into());
            if !kind.is_concrete() {
                return Err(Error::NonConcreteType {
                    span: ty.span,
                    missing: kind.missing(),
                });
            }
            validator.validate_type(ty.into());

            let evaled = eval_type(ty.into(), validator.scope)?;
            if let Type::Ptr(_) = evaled.value {
                return Err(Error::NullableMember {
                    span: evaled.span,
                    decl_kind: "union",
                });
            }
        }
        Ok(())
    }
}
