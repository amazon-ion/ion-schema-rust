use ion_rs::value::owned::OwnedElement;
use crate::violation::Violations;
use crate::types::Type;
use crate::schema::Schema;

pub trait Constraint {
    fn validate(&self, value: OwnedElement, issues: Violations);
}

#[derive(Debug, Clone)]
// TODO: add other constraints
pub enum Constraints {
    AllOf(OwnedAllOf)
}

#[derive(Debug, Clone)]
pub struct OwnedAllOf {
    ion: OwnedElement,
    schema: Schema,
    types: Vec<Type>,
}

impl OwnedAllOf {
    pub fn new(ion: OwnedElement, schema: Schema) -> Self {
        todo!()
    }

    pub fn validate_types(self, value: OwnedElement, issues: Violations) -> Vec<Type> {
        todo!()
    }
}

impl Constraint for OwnedAllOf {
    fn validate(&self, value: OwnedElement, mut issues: Violations) {
        todo!()
    }
}
