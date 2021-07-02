use std::path::Path;
use crate::system::{SchemaSystemImpl, SchemaSystem};
use ion_rs::value::owned::OwnedElement;
use std::fmt::Debug;
use ion_rs::value::Element;
use ion_rs::result::IonResult;

/// An Authority is responsible for resolving a particular class of schema identifiers.
/// The structure of a schema identifier string is defined by the
/// Authority responsible for the schema/type(s) being imported.
/// Runtime resolution of a schema over a network presents availability and security risks, and should thereby be avoided.
pub trait Authority: Debug + Clone {
    type SchemaSystem: SchemaSystem;
    type Element: Element;

    /// Provides an Iterator<Element> for the requested schema identifier.
    /// If an error condition is encountered while attempting to resolve the schema
    /// identifier, this method should throw an exception.  If no error conditions
    /// were encountered, but the schema identifier can't be resolved, this method
    /// should return [EMPTY_ITERATOR].
    fn iterator_for(self, iss: &Self::SchemaSystem, id: String) -> Box<IonResult<Box<dyn Iterator<Item=IonResult<Self::Element>>>>>;
}

// TODO: Fill the struct
#[derive(Debug, Clone)]
pub struct FileSystemAuthority<'a> {
    base_path: &'a Path,
}

impl<'a> FileSystemAuthority <'a>{
    pub fn new(base_path: &'a Path) -> Self {
        Self{
            base_path,
        }
    }

    pub fn get_base_path(&self) -> &'a Path {
        self.base_path
    }
}

impl<'a> Authority for FileSystemAuthority<'a> {
    type SchemaSystem = SchemaSystemImpl<'a>;
    type Element = OwnedElement;

    fn iterator_for(self, iss: &Self::SchemaSystem, id: String) -> Box<IonResult<Box<dyn Iterator<Item=IonResult<Self::Element>>>>> {
        todo!()
    }
}