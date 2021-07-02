use ion_rs::value::owned::OwnedElement;
use crate::schema::{SchemaImpl};
use crate::violations::{Violations};
use crate::types::{TypeImpl};

// TODO: fill the trait
pub trait Constraint {
    fn validate(&self, value: OwnedElement, issues: Violations);
}

#[derive(Debug, Clone)]
pub struct ConstraintImpl<'a> {
    ion: OwnedElement,
    name: String,
    constraint: Constraints<'a>,
}

impl<'a> Constraint for ConstraintImpl<'a> {
    fn validate(&self, value: OwnedElement, issues: Violations) {
        todo!()
    }
}

#[derive(Debug, Clone)]
// TODO: add other constraints
pub enum Constraints<'a> {
    AllOf(OwnedAllOf<'a>)
}

#[derive(Debug, Clone)]
pub struct OwnedAllOf<'a> {
    ion: OwnedElement,
    schema: SchemaImpl<'a>,
    types: Vec<TypeImpl<'a>>,
}

impl<'a> OwnedAllOf<'a> {
    pub fn new(ion: OwnedElement, schema: SchemaImpl<'a>) -> Self {
        todo!()
    }

    pub fn validate_types(self, value: OwnedElement, issues: Violations) -> Vec<&'a TypeImpl<'a>> {
        todo!()
    }
}

impl<'a> Constraint for OwnedAllOf<'a> {
    fn validate(&self, value: OwnedElement, mut issues: Violations) {
        todo!()
    }
}