use super::attributes::{AttributeSchema, Placement, BUILTIN_SCHEMAS};
use super::errors::Error;
use super::*;
use regex::Regex;
use std::collections::HashMap;

pub fn validate_file(file: &File) -> Vec<Error> {
    let mut validator = Validator::new();
    validator.validate(file);
    return validator.errors;
}

pub struct Validator {
    pub errors: Vec<Error>,
    pub attribute_schemas: &'static HashMap<String, AttributeSchema>,
}

impl Validator {
    pub fn new() -> Validator {
        // TODO: construct as not often as possible
        Validator {
            errors: Vec::new(),
            attribute_schemas: &BUILTIN_SCHEMAS,
        }
    }

    pub fn validate(&mut self, file: &File) {
        self.validate_attributes(&file.attributes, Placement::Library);
        self.validate_name(&file.name);

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

    fn validate_attributes(&mut self, attrs: &Vec<Spanned<Attribute>>, placement: Placement) {
        let mut seen: HashMap<String, usize> = HashMap::new();
        for (i, attr) in attrs.iter().enumerate() {
            self.validate_attribute(&attr, placement);
            if let Some(index) = seen.get(&attr.value.name.value) {
                self.errors.push(Error::DuplicateAttributes {
                    original: attrs[*index].clone(),
                    duplicate: attr.clone(),
                });
            } else {
                seen.insert(attr.value.name.value.clone(), i);
            }
        }
    }

    fn validate_attribute(&mut self, attr: &Spanned<Attribute>, placement: Placement) {
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

    fn validate_name(&mut self, name: &CompoundIdentifier) {
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
            self.validate_attributes(&decl.value.attributes, Placement::BitsDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, Placement::BitsMember);
            }
        }
    }

    fn validate_enums(&mut self, enums: &Vec<Spanned<Enum>>) {
        for decl in enums {
            self.validate_attributes(&decl.value.attributes, Placement::BitsDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, Placement::BitsMember);
            }
        }
    }

    fn validate_structs(&mut self, structs: &Vec<Spanned<Struct>>) {
        for decl in structs {
            self.validate_attributes(&decl.value.attributes, Placement::BitsDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, Placement::BitsMember);
            }
        }
    }

    fn validate_tables(&mut self, tables: &Vec<Spanned<Table>>) {
        for decl in tables {
            self.validate_attributes(&decl.value.attributes, Placement::BitsDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, Placement::BitsMember);
            }
        }
    }

    fn validate_unions(&mut self, unions: &Vec<Spanned<Union>>) {
        for decl in unions {
            self.validate_attributes(&decl.value.attributes, Placement::BitsDecl);
            for member in &decl.value.members {
                self.validate_attributes(&member.value.attributes, Placement::BitsMember);
            }
        }
    }
}
