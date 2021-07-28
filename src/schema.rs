use crate::import::Import;
use crate::types::Type;
use ion_rs::value::owned::OwnedElement;
use std::rc::Rc;

/// A Schema is a collection of zero or more [Type]s.
///
/// Each type may refer to other types within the same schema,
/// or types imported into this schema from other schemas.
/// To instantiate a Schema, see [SchemaSystem].
#[derive(Debug, Clone)]
pub struct Schema {
    id: String,
    imports: Vec<Rc<Schema>>, //TODO: Use HashMap for imports and types
    types: Vec<Type>,
    content: Vec<OwnedElement>, //TODO: remove this and instead pass a Vec<Type> directly to Schema constructor
}

impl Schema {
    pub fn new<A: AsRef<str>>(id: A, content: Vec<OwnedElement>) -> Self {
        Self {
            id: id.as_ref().to_owned(),
            imports: vec![],
            types: vec![],
            content,
        }
    }

    /// Returns the id for this Schema
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Returns an [Import] representing all the types imported from
    /// the specified schema [id].
    fn import(&self, id: String) -> Option<Import> {
        todo!()
    }

    /// Returns an iterator over the imports of this [Schema].  Note that
    /// multiple ISL imports referencing the same schema id (to import
    /// individual types from the same schema id, for example) are
    /// represented by a single Import object.
    fn imports(&self) -> Box<dyn Iterator<Item = Import>> {
        todo!()
    }

    /// Returns the requested type, if present in this schema;
    /// otherwise returns None.
    // TODO: could not name the method as type because its a rust keyword
    fn schema_type(&self, name: String) -> Option<Type> {
        todo!()
    }

    /// Returns an iterator over the types in this schema.
    // TODO: can be changed to return &impl Iterator<Item=Type> based on what is decided for implementation of the method
    fn types(&self) -> &[Type] {
        todo!()
    }

    /// Returns a new [Schema] instance containing all the types of this
    /// instance plus the provided type.  Note that the added type
    /// in the returned instance will hide a type of the same name
    /// from this instance.
    fn plus_type(&self, schema_type: Type) -> Self {
        todo!()
    }
}
