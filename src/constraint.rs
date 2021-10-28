use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::result::IonSchemaResult;
use crate::system::{PendingTypes, TypeId, TypeStore};
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
    AnyOf(AnyOfConstraint),
    OneOf(OneOfConstraint),
    Not(NotConstraint),
    Type(TypeConstraint),
}

impl Constraint {
    /// Creates a [Constraint::Type] referring to the type represented by the provided [TypeId].
    pub fn type_constraint(type_id: TypeId) -> Constraint {
        Constraint::Type(TypeConstraint::new(type_id))
    }

    /// Creates a [Constraint::AllOf] referring to the type represented by the provided [TypeId].
    pub fn all_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::AllOf(AllOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::AnyOf] referring to the type represented by the provided [TypeId].
    pub fn any_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::AnyOf(AnyOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::OneOf] referring to the type represented by the provided [TypeId].
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
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

impl PartialEq for AllOfConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_ids == other.type_ids
    }
}

/// Implements an `any_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#any_of
#[derive(Debug, Clone)]
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
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

impl PartialEq for AnyOfConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_ids == other.type_ids
    }
}

/// Implements an `one_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#one_of
#[derive(Debug, Clone)]
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
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

impl PartialEq for OneOfConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_ids == other.type_ids
    }
}

/// Implements a `not` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#not
#[derive(Debug, Clone)]
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
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

impl PartialEq for NotConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
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
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id =
            IslTypeRef::resolve_type_reference(type_reference, type_store, pending_types)?;
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
