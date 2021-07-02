use std::fmt::Debug;
use crate::system::{SchemaSystem, SchemaSystemImpl};
use std::iter::{FromIterator};
use ion_rs::value::owned::{OwnedElement, OwnedStruct};
use crate::types::{Type, TypeImpl};
use crate::imports::{ImportImpl, Import};
use std::collections::HashMap;
use ion_rs::result::IonResult;

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

pub trait Schema: Debug + Clone + From<String> {
    type Import: Import;
    type Type: Type;
    type SchemaSystem: SchemaSystem;

    /// Returns an Import representing all the types imported from
    /// the specified schema [id].
    fn get_import(&self, id: String) -> Option<&Self::Import>;

    /// Returns an iterator over the imports of this Schema.  Note that
    /// multiple ISL imports referencing the same schema id (to import
    /// individual types from the same schema id, for example) are
    /// represented by a single Import object.
    fn get_imports(&self) -> Box<dyn Iterator<Item = Self::Import>>;

    /// Returns the requested type, if present in this schema;
    /// otherwise returns None.
    fn get_type(&self, name: String) -> Option<&Self::Type>;

    /// Returns an iterator over the types in this schema.
    fn get_types(&self) -> Box<dyn Iterator<Item=Self::Type>>;

    /// Returns the SchemaSystem this schema was created by.
    fn get_schema_system(&self) -> Self::SchemaSystem;

    /// Returns a new Schema instance containing all the types of this
    /// instance plus the provided type.  Note that the added type
    /// in the returned instance will hide a type of the same name
    /// from this instance.
    fn plus_type(&self, schema_type: Self::Type) -> Self;
}

#[derive(Debug, Clone)]
pub struct SchemaImpl<'a> {
    system: SchemaSystemImpl<'a>,
    content: Vec<IonResult<OwnedElement>>,
    id: String,
    imports: HashMap<String, ImportImpl>,
    types: HashMap<String, TypeImpl<'a>>
}

impl<'a> SchemaImpl<'a> {
    pub fn new(system: SchemaSystemImpl<'a>,
               content: Vec<IonResult<OwnedElement>>,
               id: String) -> Self {
        let imports = HashMap::new();
        let types = HashMap::new();
        Self {
            system,
            content,
            id,
            imports,
            types
        }
    }

    fn load_header(self, header: OwnedStruct) -> HashMap<String, ImportImpl> {
        todo!()
    }

    fn validate_type(self, schema_type: TypeImpl) {
        todo!()
    }

    fn add_type(type_map: HashMap<String, TypeImpl>, schema_type: TypeImpl) {
        todo!()
    }

    pub fn get_id(&self) -> &String {
        &self.id
    }

    pub fn get_content(&self) -> &Vec<IonResult<OwnedElement>> {
        &self.content
    }
}


pub struct SchemaAndTypeImports<'a> {
    id: String,
    schema: SchemaImpl<'a>,
    types: HashMap<String, TypeImpl<'a>>
}

impl<'a> SchemaAndTypeImports<'a> {
    pub fn new(id: String, schema: SchemaImpl<'a>) -> Self {
        Self {
            id,
            schema,
            types: HashMap::new()
        }
    }

    fn add_type(self, name: String, schema_type: TypeImpl<'a>) {
        todo!()
    }
}

impl<'a> Schema for SchemaImpl<'a> {
    type Import = ImportImpl;
    type Type = TypeImpl<'a>;
    type SchemaSystem = SchemaSystemImpl<'a>;

    fn get_import(&self, id: String) -> Option<&Self::Import> {
        self.imports.get(id.as_str())
    }

    fn get_imports(&self) -> Box<dyn Iterator<Item=Self::Import>> {
        todo!()
    }

    fn get_type(&self, name: String) -> Option<&Self::Type> {
        self.types.get(name.as_str())
    }

    fn get_types(&self) -> Box<dyn Iterator<Item=Self::Type>> {
        todo!()
    }

    fn get_schema_system(&self) -> Self::SchemaSystem {
        self.system.clone()
    }

    fn plus_type(&self, schema_type: Self::Type) -> Self {
        todo!()
    }
}

impl<'a> From<String> for SchemaImpl<'a> {
    fn from(id: String) -> Self {
        todo!()
    }
}

impl<'a> FromIterator<OwnedElement> for SchemaImpl<'a> {
    fn from_iter<I: IntoIterator<Item=OwnedElement>>(iter: I) -> Self {
        todo!()
    }
}