use std::rc::Rc;
use crate::types::Type;
use ion_rs::result::IonResult;
use ion_rs::value::owned::{OwnedElement, OwnedStruct};
use std::collections::HashMap;
use crate::import::Import;
use crate::system::SchemaSystem;

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
#[derive(Debug, Clone)]
pub struct Schema {
    id: String,
    imports: Vec<Rc<Schema>>, //TODO: Use HashMap for imports and types
    types: Vec<Type>,
    content: Vec<OwnedElement>,
}

impl Schema {
    pub fn new<A: AsRef<str>>(id: A, content: Vec<OwnedElement>) -> Self {
        Self {
            id: id.as_ref().parse().unwrap(),
            imports: vec![],
            types: vec![],
            content
        }
    }

    fn validate_type(self, schema_type: Type) {
        todo!()
    }

    fn add_type(type_map: HashMap<String, Type>, schema_type: Type) {
        todo!()
    }

    /// Returns the id for this Schema
    pub fn get_id(&self) -> &String {
        &self.id
    }

    /// Returns the content of the Schema as a vector of OwnedElement
    pub fn get_content(&self) -> &Vec<OwnedElement> {
        &self.content
    }

    /// Returns an Import representing all the types imported from
    /// the specified schema [id].
    fn get_import(&self, id: String) -> Option<Import> {
        todo!()
    }

    /// Returns an iterator over the imports of this Schema.  Note that
    /// multiple ISL imports referencing the same schema id (to import
    /// individual types from the same schema id, for example) are
    /// represented by a single Import object.
    fn get_imports(&self) -> Box<dyn Iterator<Item=Import>> {
        todo!()
    }

    /// Returns the requested type, if present in this schema;
    /// otherwise returns null.
    fn get_type(&self, name: String) -> Option<Type> {
        todo!()
    }

    /// Returns an iterator over the types in this schema.
    fn get_types(&self) -> Box<dyn Iterator<Item=Type>> {
        todo!()
    }

    /// Returns the SchemaSystem this schema was created by.
    fn get_schema_system(&self) -> SchemaSystem {
        todo!()
    }

    /// Returns a new Schema instance containing all the types of this
    /// instance plus the provided type.  Note that the added type
    /// in the returned instance will hide a type of the same name
    /// from this instance.
    fn plus_type(&self, schema_type: Type) -> Self {
        todo!()
    }
}