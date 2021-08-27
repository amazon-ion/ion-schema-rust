use crate::result::{invalid_schema_error_raw, IonSchemaResult};
use crate::system::{SharedTypeStore, TypeId};
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
    type_ids: Vec<TypeId>,
}

impl AllOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [AllOf] constraint from the given OwnedElement
    pub fn parse_from_ion_element(
        ion: &OwnedElement,
        type_store: &SharedTypeStore,
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
            .map(|e| TypeRef::parse_from_ion_element(e, type_store))
            .collect::<IonSchemaResult<Vec<TypeRef>>>()?;

        let resolved_types: Vec<TypeId> = types
            .iter()
            .map(|t| TypeRef::resolve_type_reference(t, type_store))
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
    type_id: TypeId,
}

impl TypeConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create a [Type] constraint from the given OwnedElement
    pub fn parse_from_ion_element(
        ion: &OwnedElement,
        type_store: &SharedTypeStore,
    ) -> IonSchemaResult<Self> {
        if ion.ion_type() != IonType::Symbol && ion.ion_type() != IonType::Struct {
            return Err(invalid_schema_error_raw(format!(
                "type constraint was a {:?} instead of a symbol/struct",
                ion.ion_type()
            )));
        }
        let type_reference: TypeRef = TypeRef::parse_from_ion_element(ion, type_store)?;
        let type_id = TypeRef::resolve_type_reference(&type_reference, type_store)?;
        Ok(TypeConstraint::new(type_id))
    }
}

impl ConstraintValidator for TypeConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}
