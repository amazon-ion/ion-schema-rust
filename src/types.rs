use crate::schema::Schema;
use ion_rs::value::owned::{OwnedStruct, OwnedElement};
use crate::constraint::Constraints;
use crate::violation::Violations;

/// A Type consists of an optional name and zero or more constraints.
///
/// Unless otherwise specified, the constraint `type: any` is automatically applied.
#[derive(Debug, Clone)]
pub struct Type {
    schema: Schema,
    ion_struct: OwnedStruct,
    add_default_type_constraints: bool,
    constraints: Vec<Constraints>
}

impl Type {
    pub fn new(schema: Schema, ion_struct: OwnedStruct, add_default_type_constraints: bool) -> Self {
        let constraints = vec![];
        Self {
            schema,
            ion_struct,
            add_default_type_constraints,
            constraints
        }
    }

    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: OwnedElement) -> bool {
        todo!()
    }

    /// Returns a Violations object indicating whether the specified value
    /// is valid for this type, and if not, provides details as to which
    /// constraints were violated.
    fn validate(&self, value: OwnedElement) -> Violations {
        todo!()
    }
}
