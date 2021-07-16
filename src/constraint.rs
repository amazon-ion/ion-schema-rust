use ion_rs::value::owned::OwnedElement;
use crate::violation::Violations;
use crate::types::Type;
use std::convert::{TryFrom, TryInto};
use crate::result::{IonSchemaError, unresolvable_schema_error_raw};
use ion_rs::value::{Element, Sequence};

/// Defines a schema Constraint and provides validation for it
pub trait Constraint {

    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to issues
    /// if the constraint is violated.
    fn validate(&self, value: OwnedElement, issues: Violations);
}

#[derive(Debug, Clone)]
// TODO: add other constraints
pub enum Constraints {
    AllOf(AllOf)
}

/// Implements an `all_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#all_of
#[derive(Debug, Clone)]
pub struct AllOf {
    types: Vec<Type>,
}

impl AllOf {
    pub fn new(types: Vec<Type>) -> Self {
        Self{
            types
        }
    }

    // helper method for validate to validate all the types inside `all_of` constraint
    pub fn validate_types(self, value: OwnedElement, issues: Violations) -> Vec<Type> {
        todo!()
    }
}

impl Constraint for AllOf {
    fn validate(&self, value: OwnedElement, mut issues: Violations) {
        todo!()
    }
}

/// Parse the constraints into an [AllOf] schema constraint
impl TryFrom<OwnedElement> for AllOf {
    type Error = IonSchemaError;

    fn try_from(ion: OwnedElement) -> Result<Self, Self::Error> {
        let mut types = vec![];
        let type_list = match ion.as_sequence() {
            Some(ion) => ion,
            None => { return Err(unresolvable_schema_error_raw("All Of constraint is not a list")) }
        };
        for _type in type_list.iter() {
            match _type.as_struct() {
                Some(type_struct) => match type_struct.to_owned().try_into() {
                    Ok(type_reference) => types.push(type_reference),
                    Err(_) => { return Err(unresolvable_schema_error_raw("Type reference is not resolvable for All Of constraint")) }
                },
                None => { return Err(unresolvable_schema_error_raw("Type reference is not a struct for All Of constraint")) }
            }
        }
        Ok(AllOf::new(types))
    }
}