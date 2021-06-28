use ion_rs::value::{Element, Struct};
use std::fmt::Debug;
use crate::system::SchemaSystem;
use std::any::Any;
use std::iter::Map;

/// A Schema is a collection of zero or more [Type]s.
///
/// Each type may refer to other types within the same schema,
/// or types imported into this schema from other schemas.
/// To instantiate a Schema, see [SchemaSystem].
///
/// Classes that implement this interface are expected to be
/// immutable.  This avoids surprising behavior, for instance:
/// if a particular type in a schema were allowed to be replaced,
/// a value that was once valid for the type may no longer be valid.
/// Instead, any methods that would mutate a Schema are expected
/// to return a new Schema instance with the mutation applied
/// (see [plus_type] as an example of this).

pub trait Schema: Debug + Clone + From<String> + From<dyn Iterator<Item = Self::Element>> {
    type Import: Import;
    type Type: Type;
    type SchemaSystem: SchemaSystem;

    /// Returns an Import representing all the types imported from
    /// the specified schema [id].
    fn get_import(&self, id: String) -> Option<Self::Import>;

    /// Returns an iterator over the imports of this Schema.  Note that
    /// multiple ISL imports referencing the same schema id (to import
    /// individual types from the same schema id, for example) are
    /// represented by a single Import object.
    fn get_imports(&self) -> dyn Iterator<Item = Self::Import>;

    /// Returns the requested type, if present in this schema;
    /// otherwise returns null.
    fn get_type(&self, name: String) -> Option<Self::Type>;

    /// Returns an iterator over the types in this schema.
    fn get_types(&self) -> dyn Iterator<Item = Self::Type>;

    /// Returns the IonSchemaSystem this schema was created by.
    fn get_schema_system(&self) -> Self::SchemaSystem;

    /// Returns a new Schema instance containing all the types of this
    /// instance plus the provided type.  Note that the added type
    /// in the returned instance will hide a type of the same name
    /// from this instance.
    fn plus_type(&self, schema_type: Self::Type) -> Self;
}

/// A Type consists of an optional name and zero or more constraints.
///
/// Unless otherwise specified, the constraint `type: any` is automatically applied.
pub trait Type: Debug + Clone + From<String> + From<Self::Struct> {
    type Name: str;
    type Struct: Struct;
    type Element: Element;

    ///If the specified value violates one or more of this type's constraints,
    ///returns `false`, otherwise `true`
    fn is_valid(&self, value: Self::Element) -> Boolean;

    ///Returns a Violations object indicating whether the specified value
    ///is valid for this type, and if not, provides details as to which
    ///constraints were violated.
    fn validate(&self, value: Self::Element) -> Violations;
}

#[derive(Debug, Clone)]
pub struct Violations {
    violations: Vec<Violation>
}

impl Violations {
    fn add_violation(violation: Violation) {
        this.violations.unshift(violation);
    }
}

// TODO: Fill the struct
#[derive(Debug, Clone)]
pub struct Violation {}

// TODO: Fill the trait
pub trait Authority: Debug + Clone {}

// TODO: Fill the trait
pub trait Import: Debug + Clone {}