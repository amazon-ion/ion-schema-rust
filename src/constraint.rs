use crate::isl::IslTypeRef;
use crate::result::IonSchemaResult;
use crate::system::{SharedPendingTypes, SharedTypeStore, TypeId};
use crate::violation::Violations;
use ion_rs::value::owned::OwnedElement;

/// Provides validation for schema Constraint
pub trait ConstraintValidator {
    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to issues
    /// if the constraint is violated.
    fn validate(&self, value: OwnedElement, issues: &mut Violations);
}

/// Defines schema Constraints
#[derive(Debug, Clone, PartialEq)]
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
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &SharedTypeStore,
        context: &SharedPendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, context))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(AllOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for AllOfConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

impl PartialEq for AllOfConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_ids == other.type_ids
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
    pub fn resolve_from_isl_constraint(
        type_reference: &IslTypeRef,
        type_store: &SharedTypeStore,
        context: &SharedPendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id = IslTypeRef::resolve_type_reference(type_reference, type_store, context)?;
        Ok(TypeConstraint::new(type_id))
    }
}

impl ConstraintValidator for TypeConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

impl PartialEq for TypeConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}
