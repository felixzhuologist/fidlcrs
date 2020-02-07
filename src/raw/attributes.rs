use crate::raw::{FidlType, Spanned};
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTIN_SCHEMAS: HashMap<String, AttributeSchema> = {
        let mut builtins = HashMap::new();
        builtins.insert(
            "Discoverable".to_string(),
            AttributeSchema {
                placements: vec![FidlType::ProtocolDecl],
                allowed_value: None,
            },
        );
        builtins.insert(
            "Doc".to_string(),
            AttributeSchema {
                placements: Vec::new(),
                allowed_value: None,
            },
        );
        builtins.insert(
            "FragileBase".to_string(),
            AttributeSchema {
                placements: vec![FidlType::ProtocolDecl],
                allowed_value: Some("Simple".to_string()),
            },
        );
        builtins.insert(
            "MaxBytes".to_string(),
            AttributeSchema {
                placements: vec![
                    FidlType::ProtocolDecl,
                    FidlType::Method,
                    FidlType::StructDecl,
                    FidlType::TableDecl,
                    FidlType::UnionDecl,
                ],
                allowed_value: None,
            },
        );
        builtins.insert(
            "MaxHandles".to_string(),
            AttributeSchema {
                placements: vec![
                    FidlType::ProtocolDecl,
                    FidlType::Method,
                    FidlType::StructDecl,
                    FidlType::TableDecl,
                    FidlType::UnionDecl,
                ],
                allowed_value: None,
            },
        );
        builtins.insert(
            "Result".to_string(),
            AttributeSchema {
                placements: vec![FidlType::UnionDecl],
                allowed_value: Some("".to_string()),
            },
        );
        builtins.insert(
            "Selector".to_string(),
            AttributeSchema {
                placements: vec![FidlType::Method],
                allowed_value: None,
            },
        );
        builtins.insert(
            "Transport".to_string(),
            AttributeSchema {
                placements: vec![FidlType::ProtocolDecl],
                allowed_value: None,
            },
        );
        builtins
    };
}

pub struct AttributeSchema {
    placements: Vec<FidlType>,
    allowed_value: Option<String>,
    // TODO: check_constraint: fn() -> bool
}

impl AttributeSchema {
    pub fn is_placement_valid(&self, placement: FidlType) -> bool {
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
