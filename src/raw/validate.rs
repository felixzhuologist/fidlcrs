use super::attributes::{AttributeSchema, BUILTIN_SCHEMAS};
use super::errors::Error;
use super::*;
use crate::lexer::Span;
use regex::Regex;
use std::collections::HashMap;
use std::convert::TryFrom;

pub fn validate_file(file: &File) -> Vec<Error> {
    let mut validator = Validator::new();
    validator.validate(file);
    return validator.errors;
}

pub fn validate_attributes(attrs: &Attributes, placement: FidlType) -> Vec<Error> {
    let mut validator = Validator::new();
    validator.validate_attributes(attrs, placement);
    return validator.errors;
}

/// This struct exists mainly to keep track of some internal state (i.e. the
/// list of errors) while doing validation on a File. See raw::errors for the
/// things that are checked at this stage.
pub struct Validator {
    errors: Vec<Error>,
    attribute_schemas: &'static HashMap<String, AttributeSchema>,
}

// TODO: shouldn't have to need to specify FidlTypes explicitly. They should be automatically
// associated with their corresponding raw:: type
impl Validator {
    pub fn new() -> Validator {
        // TODO: construct as not often as possible
        Validator {
            errors: Vec::new(),
            attribute_schemas: &BUILTIN_SCHEMAS,
        }
    }

    pub fn validate(&mut self, file: &File) {
        self.validate_attributes(&file.attributes, FidlType::Library);
        self.validate_library_name(&file.name);
        // self.validate_imports(&file.imports);

        for decl in file.decls.iter() {
            let span = decl.span;
            match &decl.value {
                Decl::Alias(decl) => self.validate_alias(decl),
                Decl::Const(decl) => self.validate_const(&decl),
                Decl::Bits(decl) => self.validate_bits(&decl),
                Decl::Enum(decl) => self.validate_enum(&decl),
                Decl::Struct(decl) => self.validate_struct(&decl),
                Decl::Table(decl) => self.validate_table(&decl, span),
                Decl::Union(decl) => self.validate_union(&decl, span),
                Decl::Protocol(decl) => self.validate_protocol(&decl),
                Decl::Service(decl) => self.validate_service(&decl),
            }
        }
    }

    fn validate_attributes(&mut self, attrs: &Attributes, placement: FidlType) {
        self.check_for_duplicates(attrs, FidlType::Attribute);
        for attr in attrs {
            self.validate_attribute(&attr, placement);
        }
    }

    fn validate_attribute(&mut self, attr: &Spanned<Attribute>, placement: FidlType) {
        if let Some(ref schema) = self.attribute_schemas.get(&attr.value.name.value) {
            if !schema.is_placement_valid(placement) {
                self.errors.push(Error::InvalidAttributePlacement {
                    name: attr.value.name.clone(),
                    placement: placement,
                });
            }
            if !schema.is_value_valid(&attr.value.value) {
                self.errors.push(Error::InvalidAttributeValue {
                    name: attr.value.name.clone(),
                    value: attr.value.value.clone().unwrap_or(Spanned {
                        value: "".to_string(),
                        span: attr.span,
                    }),
                });
            }
        }
    }

    fn validate_library_name(&mut self, name: &LibraryName) {
        let re = Regex::new(r"^[a-z][a-z0-9]*$").unwrap();
        for component in name.iter() {
            if !re.is_match(&component.value) {
                self.errors.push(Error::InvalidLibraryName(component.span));
                // just show the first error
                break;
            }
        }
    }

    fn validate_alias(&mut self, decl: &Alias) {
        self.validate_attributes(&decl.attributes, FidlType::Alias);
    }
    fn validate_const(&mut self, decl: &Const) {
        self.validate_attributes(&decl.attributes, FidlType::ConstDecl);
    }

    fn validate_bits(&mut self, decl: &Bits) {
        self.check_for_duplicates(&decl.members, FidlType::BitsMember);
        self.validate_attributes(&decl.attributes, FidlType::BitsDecl);
        for member in &decl.members {
            self.validate_attributes(&member.value.attributes, FidlType::BitsMember);
        }
    }

    fn validate_enum(&mut self, decl: &Enum) {
        self.check_for_duplicates(&decl.members, FidlType::EnumMember);
        self.validate_attributes(&decl.attributes, FidlType::EnumDecl);
        for member in &decl.members {
            self.validate_attributes(&member.value.attributes, FidlType::EnumMember);
        }
    }

    fn validate_struct(&mut self, decl: &Struct) {
        self.check_for_duplicates(&decl.members, FidlType::StructMember);
        self.validate_attributes(&decl.attributes, FidlType::StructDecl);
        for member in &decl.members {
            self.validate_attributes(&member.value.attributes, FidlType::StructMember);
        }
    }

    fn validate_table(&mut self, decl: &Table, span: Span) {
        self.check_for_duplicates(&decl.members, FidlType::TableMember);
        self.validate_attributes(&decl.attributes, FidlType::TableDecl);
        for member in &decl.members {
            self.validate_attributes(&member.value.attributes, FidlType::TableMember);
        }
        self.check_ordinals(&decl.members, span, decl.name.span);
    }

    fn validate_union(&mut self, decl: &Union, span: Span) {
        self.validate_attributes(&decl.attributes, FidlType::UnionDecl);
        self.check_for_duplicates(&decl.members, FidlType::UnionMember);
        for member in &decl.members {
            self.validate_attributes(&member.value.attributes, FidlType::UnionMember);
        }
        self.check_ordinals(&decl.members, span, decl.name.span);
    }

    fn validate_protocol(&mut self, decl: &Protocol) {
        self.validate_attributes(&decl.attributes, FidlType::ProtocolDecl);
        // Note: we check for duplicate compose statements after name resolution, once we
        // know which protocols each name points to.
        self.check_for_duplicates(&decl.methods, FidlType::ProtocolDecl);
        for method in &decl.methods {
            self.validate_attributes(&method.value.attributes, FidlType::Method);
            if let Some(request) = &method.value.request {
                self.check_for_duplicates(request, FidlType::Parameter);
                for param in request {
                    self.validate_attributes(&param.value.attributes, FidlType::Parameter);
                }
            }
            if let Some(response) = &method.value.response {
                self.check_for_duplicates(response, FidlType::Parameter);
                for param in response {
                    self.validate_attributes(&param.value.attributes, FidlType::Parameter);
                }
            }
        }
    }

    fn validate_service(&mut self, decl: &Service) {
        self.validate_attributes(&decl.attributes, FidlType::ServiceDecl);
        for member in &decl.members {
            self.validate_attributes(&member.value.attributes, FidlType::ServiceMember);
        }
    }

    // TODO: spans are not completely correct here
    fn check_for_duplicates<T>(&mut self, values: &Vec<Spanned<T>>, decltype: FidlType)
    where
        T: Nameable,
    {
        let mut seen: HashMap<String, usize> = HashMap::new();
        for (i, val) in values.iter().enumerate() {
            match val.name() {
                None => continue,
                Some(ref name) => {
                    if let Some(index) = seen.get(*name) {
                        self.errors.push(Error::DuplicateDefinition {
                            original: values[*index].span,
                            duplicate: val.span,
                            decl_type: decltype,
                            decl_name: name.to_string(),
                        });
                    } else {
                        seen.insert(name.to_string(), i);
                    }
                }
            };
        }
    }

    /// Checks that a set of ordinal members are valid:
    ///   - at least one non reserved member
    ///   - not out of bounds ordinals (i.e. they all fall in [1, 0xffff_ffff])
    ///   - they form a dense space starting at 1
    fn check_ordinals<T>(&mut self, values: &Vec<Spanned<T>>, decl_span: Span, decl_name_span: Span)
    where
        T: HasOrdinal,
    {
        let is_empty = values
            .iter()
            .filter(|&member| !member.value.is_reserved())
            .peekable()
            .peek()
            .is_none();
        if is_empty {
            self.errors.push(Error::EmptyTableOrUnion(decl_span));
        }

        let (ordinals, oob_ordinals): (Vec<_>, Vec<_>) = values
            .iter()
            .map(|member| {
                let IntLiteral { value, is_negative } = member.value.ordinal().value;
                let ord_val = u32::try_from(value);
                if is_negative || value == 0 || ord_val.is_err() {
                    Err(member.span)
                } else {
                    Ok(ord_val.unwrap())
                }
            })
            .partition(Result::is_ok);
        if !oob_ordinals.is_empty() {
            self.errors.push(Error::OobOrdinals {
                decl_span,
                ordinal_spans: oob_ordinals.into_iter().map(Result::unwrap_err).collect(),
            });
        }
        let mut ordinals: Vec<u32> = ordinals.into_iter().map(Result::unwrap).collect();
        ordinals.sort();
        let mut missing_ranges: Vec<(u32, u32)> = Vec::new();
        let mut next_expected = 1;
        for ord in ordinals {
            if ord != next_expected {
                missing_ranges.push((next_expected, ord - 1));
            }
            next_expected = ord + 1;
        }
        if !missing_ranges.is_empty() {
            self.errors.push(Error::NonDenseOrdinals {
                decl_span,
                name_span: decl_name_span,
                missing_ranges: missing_ranges,
            });
        }
    }
}

trait Nameable {
    fn name(&self) -> Option<&String>;
}

impl<T> Nameable for Spanned<T>
where
    T: Nameable,
{
    fn name(&self) -> Option<&String> {
        self.value.name()
    }
}

impl Nameable for Attribute {
    fn name(&self) -> Option<&String> {
        Some(&self.name.value)
    }
}

impl Nameable for StructMember {
    fn name(&self) -> Option<&String> {
        Some(&self.name.value)
    }
}

impl Nameable for BitsMember {
    fn name(&self) -> Option<&String> {
        Some(&self.name.value)
    }
}

impl Nameable for EnumMember {
    fn name(&self) -> Option<&String> {
        Some(&self.name.value)
    }
}

impl Nameable for TableMember {
    fn name(&self) -> Option<&String> {
        match &self.inner {
            TableMemberInner::Reserved => None,
            TableMemberInner::Used { ty: _, name } => Some(&name.value),
        }
    }
}

impl Nameable for UnionMember {
    fn name(&self) -> Option<&String> {
        match &self.inner {
            UnionMemberInner::Reserved => None,
            UnionMemberInner::Used { ty: _, name } => Some(&name.value),
        }
    }
}

impl Nameable for Method {
    fn name(&self) -> Option<&String> {
        Some(&self.name.value)
    }
}

impl Nameable for Parameter {
    fn name(&self) -> Option<&String> {
        Some(&self.name.value)
    }
}

trait HasOrdinal {
    fn is_reserved(&self) -> bool;
    fn ordinal(&self) -> Spanned<IntLiteral>;
}

impl HasOrdinal for TableMember {
    fn is_reserved(&self) -> bool {
        match &self.inner {
            TableMemberInner::Reserved => true,
            _ => false,
        }
    }
    fn ordinal(&self) -> Spanned<IntLiteral> {
        self.ordinal
    }
}

impl HasOrdinal for UnionMember {
    fn is_reserved(&self) -> bool {
        match &self.inner {
            UnionMemberInner::Reserved => true,
            _ => false,
        }
    }
    fn ordinal(&self) -> Spanned<IntLiteral> {
        self.ordinal
    }
}
