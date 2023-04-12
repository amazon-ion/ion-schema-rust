use crate::ion_path::IonPath;
use crate::isl::isl_type_reference::IslTypeRefModifier;
use crate::result::ValidationResult;
use crate::system::{TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::IonSchemaElement;
use ion_rs::IonType;

/// Provides an internal representation of a schema type reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeReference {
    type_id: TypeId,
    type_modifier: IslTypeRefModifier,
}

impl TypeReference {
    pub fn new(type_id: TypeId, type_modifier: IslTypeRefModifier) -> Self {
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
        use crate::isl::isl_type_reference::IslTypeRefModifier::*;
        let type_def = type_store.get_type_by_id(self.type_id()).unwrap();
        match self.type_modifier {
            Nullable => match value {
                IonSchemaElement::SingleElement(element) => {
                    if element.is_null()
                        && (element.ion_type() == IonType::Null
                            || type_def
                                .is_valid_for_base_nullable_type(value, type_store, ion_path))
                    {
                        return Ok(());
                    }
                }
                IonSchemaElement::Document(_) => {}
            },
            NullOr => {
                if let IonSchemaElement::SingleElement(element) = value {
                    if element.ion_type() == IonType::Null {
                        return Ok(());
                    }
                }
            }
            Nothing => {}
        }
        type_def.validate(value, type_store, ion_path)
    }
}
