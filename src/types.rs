use std::fmt::Debug;
use ion_rs::value::{Struct, Element};
use crate::violations::Violations;
use crate::schema::SchemaImpl;
use ion_rs::value::owned::{OwnedStruct, OwnedElement};
use crate::constraints::Constraints;

/// A Type consists of an optional name and zero or more constraints.
pub trait Type: Debug + Clone + From<String> + From<OwnedStruct> {
    type Struct: Struct;
    type Element: Element;

    ///If the specified value violates one or more of this type's constraints,
    ///returns `false`, otherwise `true`
    fn is_valid(&self, value: Self::Element) -> bool;

    ///Returns a Violations object indicating whether the specified value
    ///is valid for this type, and if not, provides details as to which
    ///constraints were violated.
    fn validate(&self, value: Self::Element) -> Violations;
}

// TODO: Fill the struct
#[derive(Debug, Clone)]
pub struct TypeImpl<'a> {
    schema: SchemaImpl<'a>,
    ion_struct: &'a OwnedStruct,
    add_default_type_constraints: bool,
    constraints: Vec<Constraints<'a>>
}

impl<'a> TypeImpl<'a> {
    pub fn new(schema: SchemaImpl<'a>, ion_struct: &'a OwnedStruct, add_default_type_constraints: bool) -> Self {
        let constraints = vec![];
        Self {
            schema,
            ion_struct,
            add_default_type_constraints,
            constraints
        }
    }
}

impl<'a> Type for TypeImpl<'a> {
    type Struct = OwnedStruct;
    type Element = OwnedElement;

    fn is_valid(&self, value: Self::Element) -> bool {
        todo!()
    }

    fn validate(&self, value: Self::Element) -> Violations {
        todo!()
    }
}

impl<'a> From<String> for TypeImpl<'a> {
    fn from(isl: String) -> Self {
        todo!()
    }
}

impl<'a> From<OwnedStruct> for TypeImpl<'a> {
    fn from(isl: OwnedStruct) -> Self {
        todo!()
    }
}