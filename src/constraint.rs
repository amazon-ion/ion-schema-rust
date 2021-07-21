use crate::result::{invalid_schema_error_raw, IonSchemaError};
use crate::types::Type;
use crate::violation::Violations;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence};
use ion_rs::IonType;
use std::convert::{TryFrom, TryInto};

/// Defines a schema Constraint and provides validation for it
pub trait Constraint {
    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to issues
    /// if the constraint is violated.
    fn validate(&self, value: OwnedElement, issues: &mut Violations);
}

#[derive(Debug, Clone)]
// TODO: add other constraints
pub enum Constraints {
    AllOf(AllOf),
}

/// Implements an `all_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#all_of
#[derive(Debug, Clone)]
pub struct AllOf {
    types: Vec<Type>,
}

impl AllOf {
    pub fn new(types: Vec<Type>) -> Self {
        Self { types }
    }
}

impl Constraint for AllOf {
    fn validate(&self, value: OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

/// Tries to create an [AllOf] constraint from the given OwnedElement
impl TryFrom<&OwnedElement> for AllOf {
    type Error = IonSchemaError;

    fn try_from(ion: &OwnedElement) -> Result<Self, Self::Error> {
        if ion.ion_type() != IonType::List {
            return Err(invalid_schema_error_raw(format!(
                "all_of constraint was a {:?} instead of a list",
                ion.ion_type()
            )));
        }
        let types: Vec<Type> = ion
            .as_sequence()
            .unwrap()
            .iter()
            .map(|e| {
                e.as_struct()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "all_of constraint must be a struct with type references inside it",
                        )
                    })
                    .and_then(|type_reference| type_reference.try_into())
            })
            .collect::<Result<Vec<Type>, IonSchemaError>>()?;
        Ok(AllOf::new(types))
    }
}
