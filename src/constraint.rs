use crate::result::{invalid_schema_error_raw, IonSchemaError, IonSchemaResult};
use crate::types::TypeRef;
use crate::violation::Violations;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence};
use ion_rs::IonType;
use std::convert::{TryFrom, TryInto};

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
    type_references: Vec<TypeRef>,
}

impl AllOfConstraint {
    pub fn new(types: Vec<TypeRef>) -> Self {
        Self {
            type_references: types,
        }
    }

    /// Returns type references that are not yet resolved (alias type reference or anonymous type reference)
    pub fn deferred_type_references(&self) -> Vec<TypeRef> {
        self.type_references
            .iter()
            .filter(|t| match t {
                TypeRef::ISLCoreType(_) => false,
                _ => true,
            })
            .map(|t| t.to_owned())
            .collect()
    }
}

impl ConstraintValidator for AllOfConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

/// Tries to create an [AllOf] constraint from the given OwnedElement
impl TryFrom<&OwnedElement> for AllOfConstraint {
    type Error = IonSchemaError;

    fn try_from(ion: &OwnedElement) -> Result<Self, Self::Error> {
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
            .map(|e| e.try_into())
            .collect::<IonSchemaResult<Vec<TypeRef>>>()?;
        Ok(AllOfConstraint::new(types))
    }
}

/// Implements a `type` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#type
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    type_reference: TypeRef,
}

impl TypeConstraint {
    pub fn new(type_reference: TypeRef) -> Self {
        Self { type_reference }
    }

    /// Returns type references that are not yet resolved (alias type reference or anonymous type reference)
    pub fn deferred_type_reference(&self) -> Option<&TypeRef> {
        match self.type_reference {
            TypeRef::ISLCoreType(_) => Some(&self.type_reference),
            _ => None,
        }
    }
}

impl ConstraintValidator for TypeConstraint {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

/// Tries to create a [Type] constraint from the given OwnedElement
impl TryFrom<&OwnedElement> for TypeConstraint {
    type Error = IonSchemaError;

    fn try_from(ion: &OwnedElement) -> Result<Self, Self::Error> {
        if ion.ion_type() != IonType::Symbol && ion.ion_type() != IonType::Struct {
            return Err(invalid_schema_error_raw(format!(
                "type constraint was a {:?} instead of a symbol/struct",
                ion.ion_type()
            )));
        }
        let type_reference: TypeRef = ion.try_into()?;
        Ok(TypeConstraint::new(type_reference.to_owned()))
    }
}
