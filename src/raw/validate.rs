use super::attributes::{AttributeSchema, BUILTIN_SCHEMAS};
use super::errors::Error;
use super::*;
use regex::Regex;
use std::collections::HashMap;
use std::convert::TryFrom;
// use std::slice::Iter;

pub fn validate_file(file: &File) -> Vec<Error> {
    let mut validator = Validator::new();
    validator.validate(file);
    return validator.errors;
}

pub struct Validator {
    pub errors: Vec<Error>,
    pub attribute_schemas: &'static HashMap<String, AttributeSchema>,
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

        // self.validate_aliases(&file.aliases);
        // self.validate_consts(&file.consts);
        self.validate_bits(&file.bits);
        self.validate_enums(&file.enums);
        self.validate_structs(&file.structs);
        self.validate_tables(&file.tables);
        self.validate_unions(&file.unions);

        // self.validate_protocols(&file.protocols);
        // self.validate_services(&file.services);
    }

    fn validate_attributes(&mut self, attrs: &Vec<Spanned<Attribute>>, placement: FidlType) {
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

    fn validate_library_name(&mut self, name: &CompoundIdentifier) {
        let re = Regex::new(r"^[a-z][a-z0-9]*$").unwrap();
        for component in name.iter() {
            if !re.is_match(&component.value) {
                self.errors.push(Error::InvalidLibraryName(component.span));
                // just show the first error
                break;
            }
        }
    }

    // Note: fidlc allows two ways to reference an import with an alias:
    // either using the alias, or the full path. (the docs also say that using
    // the name is valid but fidlc doesn't actually do that - bug?). for simplicity,
    // right now if an alias is used, the library must be referenced by that alias
    // fn validate_imports(&mut self, imports: &Vec<Spanned<Import>>) {
    //     for import in imports.iter() {
    //         if !import.attributes.is_empty() {
    //             unimplemented!();
    //         }
    //         // the actual name the must be used to reference this import
    //         let reference_name = match import.value.alias {
    //             Some(alias) => alias.value.clone(),
    //             None => {
    //                 let mut components: Vector<String> = import.name.iter().map(|c| c.value.clone());
    //                 components.join(".")
    //             }
    //         }
    //         if !self.imported_libraries.insert(reference_name) {

    //         }
    //     }
    // }

    fn validate_bits(&mut self, bits: &Vec<Spanned<Bits>>) {
        for decl in bits {
            self.check_for_duplicates(&decl.value.members, FidlType::BitsMember);
            self.validate_attributes(&decl.value.attributes, FidlType::BitsDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, FidlType::BitsMember);
            }
        }
    }

    fn validate_enums(&mut self, enums: &Vec<Spanned<Enum>>) {
        for decl in enums {
            self.check_for_duplicates(&decl.value.members, FidlType::EnumMember);
            self.validate_attributes(&decl.value.attributes, FidlType::EnumDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, FidlType::EnumMember);
            }
        }
    }

    fn validate_structs(&mut self, structs: &Vec<Spanned<Struct>>) {
        for decl in structs {
            self.check_for_duplicates(&decl.value.members, FidlType::StructMember);
            self.validate_attributes(&decl.value.attributes, FidlType::StructDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, FidlType::StructMember);
            }
        }
    }

    fn validate_tables(&mut self, tables: &Vec<Spanned<Table>>) {
        for decl in tables {
            self.check_for_duplicates(&decl.value.members, FidlType::TableMember);
            self.validate_attributes(&decl.value.attributes, FidlType::TableDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, FidlType::TableMember);
            }

            let is_empty = decl
                .value
                .members
                .iter()
                .filter(|&member| member.value.is_used())
                .peekable()
                .peek()
                .is_none();
            if is_empty {
                self.errors
                    .push(Error::EmptyTableOrUnion(decl.span, FidlType::TableDecl));
            }

            let (ordinals, oob_ordinals): (Vec<_>, Vec<_>) = decl
                .value
                .members
                .iter()
                .map(|member| {
                    let IntLiteral { value, is_negative } = member.value.ordinal.value;
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
                    decl_span: decl.span,
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
                    decl_span: decl.span,
                    name_span: decl.value.name.span,
                    missing_ranges: missing_ranges,
                });
            }
        }
    }

    fn validate_unions(&mut self, unions: &Vec<Spanned<Union>>) {
        for decl in unions {
            self.validate_attributes(&decl.value.attributes, FidlType::UnionDecl);
            self.check_for_duplicates(&decl.value.members, FidlType::UnionMember);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, FidlType::UnionMember);
            }
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
            TableMemberInner::Used {
                ty: _,
                name,
                default_value: _,
            } => Some(&name.value),
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

impl TableMember {
    fn is_used(&self) -> bool {
        match &self.inner {
            TableMemberInner::Reserved => false,
            _ => true,
        }
    }
}
