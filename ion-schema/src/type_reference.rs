use crate::ion_path::IonPath;
use crate::isl::isl_type_reference::NullabilityModifier;
use crate::isl::ranges::UsizeRange;
use crate::result::ValidationResult;
use crate::system::{TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::{IonSchemaElement, IonSchemaElementType};
use ion_rs::IonType;

/// Provides reference to a type definition.
/// This will be used by constraints to store type references.
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct TypeReference {
    type_id: TypeId,
    type_modifier: NullabilityModifier,
}

impl TypeReference {
    pub fn new(type_id: TypeId, type_modifier: NullabilityModifier) -> Self {
        Self {
            type_id,
            type_modifier,
        }
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }
}

impl TypeValidator for TypeReference {
    fn is_valid(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> bool {
        let violation = self.validate(value, type_store, ion_path);
        violation.is_ok()
    }

    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        use crate::isl::isl_type_reference::NullabilityModifier::*;
        let type_def = type_store.get_type_by_id(self.type_id()).unwrap();
        match self.type_modifier {
            Nullable => {
                if let Some(element) = value.as_element() {
                    if element.is_null()
                        && (element.ion_type() == IonType::Null
                            || type_def
                                .is_valid_for_base_nullable_type(value, type_store, ion_path))
                    {
                        return Ok(());
                    }
                }
            }
            NullOr => {
                if value.ion_schema_type() == IonSchemaElementType::Null {
                    return Ok(());
                }
            }
            Nothing => {}
        }
        type_def.validate(value, type_store, ion_path)
    }
}

/// Provides reference to a type definition that has an `occurs` field.
/// This will be used by `ordered_elements` and `fields` constraints to store variably occurring type references.
#[derive(Debug, Clone, PartialEq)]
pub struct VariablyOccurringTypeRef {
    type_ref: TypeReference,
    occurs_range: UsizeRange, // represents the range provided by `occurs` field for given type reference
}

impl VariablyOccurringTypeRef {
    pub fn new(type_ref: TypeReference, occurs_range: UsizeRange) -> Self {
        Self {
            type_ref,
            occurs_range,
        }
    }

    pub fn type_ref(&self) -> TypeReference {
        self.type_ref
    }

    pub fn occurs_range(&self) -> &UsizeRange {
        &self.occurs_range
    }
}
