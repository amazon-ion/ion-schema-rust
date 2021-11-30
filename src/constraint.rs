use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::result::IonSchemaResult;
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::violation::{Violation, ViolationLeaf};
use ion_rs::value::owned::OwnedElement;

/// Provides validation for schema Constraint
pub trait ConstraintValidator {
    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to issues
    /// if the constraint is violated.
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore);
}

/// Defines schema Constraints
#[derive(Debug, Clone, PartialEq)]
// TODO: add other constraints
pub enum Constraint {
    AllOf(AllOfConstraint),
    AnyOf(AnyOfConstraint),
    Not(NotConstraint),
    OneOf(OneOfConstraint),
    Type(TypeConstraint),
}

impl Constraint {
    /// Creates a [Constraint::Type] referring to the type represented by the provided [TypeId].
    pub fn type_constraint(type_id: TypeId) -> Constraint {
        Constraint::Type(TypeConstraint::new(type_id))
    }

    /// Creates a [Constraint::AllOf] referring to the types represented by the provided [TypeId]s.
    pub fn all_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::AllOf(AllOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::AnyOf] referring to the types represented by the provided [TypeId]s.
    pub fn any_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::AnyOf(AnyOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::OneOf] referring to the types represented by the provided [TypeId]s.
    pub fn one_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::OneOf(OneOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::Not] referring to the type represented by the provided [TypeId].
    pub fn not(type_id: TypeId) -> Constraint {
        Constraint::Not(NotConstraint::new(type_id))
    }

    /// Parse an [IslConstraint] to a [Constraint]
    pub fn resolve_from_isl_constraint(
        isl_constraint: &IslConstraint,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Constraint> {
        // TODO: add more constraints below
        match isl_constraint {
            IslConstraint::AllOf(type_references) => {
                let all_of: AllOfConstraint = AllOfConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::AllOf(all_of))
            }
            IslConstraint::AnyOf(type_references) => {
                let any_of: AnyOfConstraint = AnyOfConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::AnyOf(any_of))
            }
            IslConstraint::OneOf(type_references) => {
                let one_of: OneOfConstraint = OneOfConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::OneOf(one_of))
            }
            IslConstraint::Not(type_references) => {
                let not: NotConstraint = NotConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::Not(not))
            }
            IslConstraint::Type(type_reference) => {
                let type_constraint: TypeConstraint = TypeConstraint::resolve_from_isl_constraint(
                    type_reference,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::Type(type_constraint))
            }
        }
    }

    pub fn validate(
        &self,
        value: &OwnedElement,
        issues: &mut impl Violation,
        type_store: &TypeStore,
    ) {
        match self {
            Constraint::AllOf(all_of) => all_of.validate(value, issues, type_store),
            Constraint::AnyOf(any_of) => any_of.validate(value, issues, type_store),
            Constraint::Not(not) => not.validate(value, issues, type_store),
            Constraint::OneOf(one_of) => one_of.validate(value, issues, type_store),
            Constraint::Type(type_constraint) => {
                type_constraint.validate(value, issues, type_store)
            }
        }
    }
}

/// Implements an `all_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#all_of
#[derive(Debug, Clone, PartialEq)]
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
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(AllOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for AllOfConstraint {
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        let mut valid_types = vec![];
        let mut all_of_violation = ViolationLeaf::new("all_of", "all_types_not_matched", "");
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            let checkpoint = all_of_violation.checkpoint();
            type_def.validate(value, &mut all_of_violation, type_store);
            if checkpoint.is_valid(&all_of_violation) {
                valid_types.push(type_id);
            }
        }
        if valid_types.len() != self.type_ids.len() {
            all_of_violation.message = format!(
                "value matches {} types, expected {}",
                valid_types.len(),
                self.type_ids.len()
            );
            issues.add_violation(all_of_violation)
        }
    }
}

/// Implements an `any_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#any_of
#[derive(Debug, Clone, PartialEq)]
pub struct AnyOfConstraint {
    type_ids: Vec<TypeId>,
}

impl AnyOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [AnyOf] constraint from the given OwnedElement
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(AnyOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for AnyOfConstraint {
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        let mut valid_types = vec![];
        let mut any_of_violation = ViolationLeaf::new(
            "any_of",
            "no_types_matched",
            "value matches none of the types",
        );
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            let checkpoint = any_of_violation.checkpoint();
            type_def.validate(value, &mut any_of_violation, type_store);
            if checkpoint.is_valid(&any_of_violation) {
                valid_types.push(type_id);
            }
        }
        if valid_types.len() == 0 {
            issues.add_violation(any_of_violation)
        }
    }
}

/// Implements an `one_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#one_of
#[derive(Debug, Clone, PartialEq)]
pub struct OneOfConstraint {
    type_ids: Vec<TypeId>,
}

impl OneOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [OneOf] constraint from the given OwnedElement
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(OneOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for OneOfConstraint {
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        let mut valid_types = vec![];
        let mut one_of_violation = ViolationLeaf::new("one_of", "", "");
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            let checkpoint = one_of_violation.checkpoint();
            type_def.validate(value, &mut one_of_violation, type_store);
            if checkpoint.is_valid(&one_of_violation) {
                valid_types.push(type_id);
            }
        }
        if valid_types.len() != 1 {
            if valid_types.len() == 0 {
                one_of_violation.code = "no_types_matched".to_owned();
                one_of_violation.message = "value matches none of the types".to_owned();
            } else if valid_types.len() > 1 {
                one_of_violation.code = "more_than_one_type_matched".to_owned();
                one_of_violation.message =
                    format!("value matches {} types, expected 1", valid_types.len());
            }
            issues.add_violation(one_of_violation)
        }
    }
}

/// Implements a `not` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#not
#[derive(Debug, Clone, PartialEq)]
pub struct NotConstraint {
    type_id: TypeId,
}

impl NotConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create a [Not] constraint from the given OwnedElement
    pub fn resolve_from_isl_constraint(
        type_reference: &IslTypeRef,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id =
            IslTypeRef::resolve_type_reference(type_reference, type_store, pending_types)?;
        Ok(NotConstraint::new(type_id))
    }
}

impl ConstraintValidator for NotConstraint {
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();
        let mut not_violation =
            ViolationLeaf::new("not", "type_matched", "value unexpectedly matches type");
        type_def.validate(value, &mut not_violation, type_store);
        if not_violation.is_valid() {
            issues.add_violation(not_violation);
        }
    }
}

/// Implements a `type` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#type
#[derive(Debug, Clone, PartialEq)]
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
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id =
            IslTypeRef::resolve_type_reference(type_reference, type_store, pending_types)?;
        Ok(TypeConstraint::new(type_id))
    }
}

impl ConstraintValidator for TypeConstraint {
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();
        type_def.validate(value, issues, type_store);
    }
}
