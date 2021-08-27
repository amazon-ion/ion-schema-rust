use crate::result::{invalid_schema_error_raw, IonSchemaResult};
use crate::system::{SharedTypeCache, TypeId};
use crate::types::TypeRef;
use crate::violation::Violations;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence};
use ion_rs::IonType;

/// Provides validation for schema Constraint
pub trait ConstraintValidator {
    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to issues
    /// if the constraint is violated.
    fn validate(&self, value: OwnedElement, issues: &mut Violations);
}

/// Defines schema Constraints
#[derive(Debug, Clone)]
// TODO: add other constraints
pub enum Constraint {
    AllOf(AllOfConstraint),
    Type(TypeConstraint),
}

/// Implements an `all_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#all_of
#[derive(Debug, Clone)]
pub struct AllOfConstraint {
    type_references: Vec<TypeId>,
}

impl AllOfConstraint {
    pub fn new(types: Vec<TypeId>) -> Self {
        Self {
            type_references: types,
        }
    }

    /// Tries to create an [AllOf] constraint from the given OwnedElement
    pub fn parse_from_ion_element(
        ion: &OwnedElement,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Self> {
        if ion.ion_type() != IonType::List {
            return Err(invalid_schema_error_raw(format!(
                "all_of constraint was a {:?} instead of a list",
                ion.ion_type()
            )));
        }
        let types: Vec<TypeRef> = ion
            .as_sequence()
            .unwrap()
            .iter()
            .map(|e| TypeRef::parse_from_ion_element(e, type_cache))
            .collect::<IonSchemaResult<Vec<TypeRef>>>()?;

        let resolved_types: Vec<TypeId> = types
            .iter()
            .map(|t| TypeRef::resolve_type_reference(t, type_cache))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(AllOfConstraint::new(resolved_types.to_owned()))
    }
}

impl ConstraintValidator for AllOfConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

/// Implements a `type` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#type
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    type_reference: TypeId,
}

impl TypeConstraint {
    pub fn new(type_reference: TypeId) -> Self {
        Self { type_reference }
    }

    /// Tries to create a [Type] constraint from the given OwnedElement
    pub fn parse_from_ion_element(
        ion: &OwnedElement,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Self> {
        if ion.ion_type() != IonType::Symbol && ion.ion_type() != IonType::Struct {
            return Err(invalid_schema_error_raw(format!(
                "type constraint was a {:?} instead of a symbol/struct",
                ion.ion_type()
            )));
        }
        let type_reference: TypeRef = TypeRef::parse_from_ion_element(ion, type_cache)?;
        let type_id = TypeRef::resolve_type_reference(&type_reference, type_cache)?;
        Ok(TypeConstraint::new(type_id))
    }
}

impl ConstraintValidator for TypeConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}
