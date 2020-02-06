use super::errors::Error;
use crate::raw::Attribute;
use crate::raw::Spanned;
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTIN_SCHEMAS: HashMap<String, AttributeSchema> = {
        let mut builtins = HashMap::new();
        builtins.insert(
            "Discoverable".to_string(),
            AttributeSchema { placements: vec![Placement::ProtocolDecl], allowed_value: None },
        );
        builtins.insert(
            "Doc".to_string(),
            AttributeSchema { placements: Vec::new(), allowed_value: None },
        );
        builtins.insert(
            "FragileBase".to_string(),
            AttributeSchema {
                placements: vec![Placement::ProtocolDecl],
                allowed_value: Some("Simple".to_string()),
            },
        );
        builtins.insert(
            "MaxBytes".to_string(),
            AttributeSchema {
                placements: vec![
                    Placement::ProtocolDecl,
                    Placement::Method,
                    Placement::StructDecl,
                    Placement::TableDecl,
                    Placement::UnionDecl,
                ],
                allowed_value: None,
            },
        );
        builtins.insert(
            "MaxHandles".to_string(),
            AttributeSchema {
                placements: vec![
                    Placement::ProtocolDecl,
                    Placement::Method,
                    Placement::StructDecl,
                    Placement::TableDecl,
                    Placement::UnionDecl,
                ],
                allowed_value: None,
            },
        );
        builtins.insert(
            "Result".to_string(),
            AttributeSchema {
                placements: vec![Placement::UnionDecl],
                allowed_value: Some("".to_string()),
            },
        );
        builtins.insert(
            "Selector".to_string(),
            AttributeSchema { placements: vec![Placement::Method], allowed_value: None },
        );
        builtins.insert(
            "Transport".to_string(),
            AttributeSchema { placements: vec![Placement::ProtocolDecl], allowed_value: None },
        );
        builtins
    };
}

pub struct AttributeSchema {
    placements: Vec<Placement>,
    allowed_value: Option<String>,
    // TODO: check_constraint: fn() -> bool
}

impl AttributeSchema {
    pub fn is_placement_valid(&self, placement: Placement) -> bool {
        self.placements.is_empty() || self.placements.contains(&placement)
    }

    pub fn is_value_valid(&self, value: &Option<Spanned<String>>) -> bool {
        match self.allowed_value {
            None => true,
            // TODO: should Attribute.Value just be a String instead of
            // Option<String> if it's treated the same as an empty string?
            Some(ref allowed) => match value {
                None => allowed == "",
                Some(ref value) => allowed == &value.value,
            },
        }
    }
}

// TODO: this shouldn't need to exist. or, at least it shouldn't need to be
// specified explicitly in the function call
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Placement {
    BitsDecl,
    BitsMember,
    ConstDecl,
    EnumDecl,
    EnumMember,
    ProtocolDecl,
    Library,
    Method,
    ServiceDecl,
    ServiceMember,
    StructDecl,
    StructMember,
    TableDecl,
    TableMember,
    TypeAliasDecl,
    UnionDecl,
    UnionMember,
}
