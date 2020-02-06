use std::collections::HashMap;
use super::attributes::{AttributeSchema, BUILTIN_SCHEMAS, Placement};
use super::errors::Error;
use super::*;

pub fn validate_file(file: &File) -> Vec<Error> {
    let mut validator = Validator::new();
    validator.validate(file);
    return validator.errors
}

pub struct Validator {
    pub errors: Vec<Error>,
    pub attribute_schemas: &'static HashMap<String, AttributeSchema>,
}

impl Validator {
    pub fn new() -> Validator {
        // TODO: construct as not often as possible
        Validator { errors: Vec::new(), attribute_schemas: &BUILTIN_SCHEMAS, }
    }

    pub fn validate(&mut self, file: &File) {
        self.validate_attributes(&file.attributes, Placement::Library);
    }

    fn validate_attributes(&mut self, attrs: &Vec<Spanned<Attribute>>, placement: Placement) {
        let mut seen: HashMap<String, usize> = HashMap::new();
        for (i, attr) in attrs.iter().enumerate() {
            self.validate_attribute(&attr, placement);
            if let Some(index) = seen.get(&attr.value.name.value) {
                self.errors.push(Error::DuplicateAttributes{
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
                self.errors.push(Error::InvalidAttributePlacement{
                    name: attr.value.name.clone(),
                    placement: placement,
                });
            }
            if !schema.is_value_valid(&attr.value.value) {
                self.errors.push(Error::InvalidAttributeValue{
                    name: attr.value.name.clone(),
                    value: attr.value.value.clone().unwrap(),
                });
            }
        }
    }
}
