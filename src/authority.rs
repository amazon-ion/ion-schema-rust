use crate::result::{unresolvable_schema_error_raw, IonSchemaResult};
use ion_rs::result::IonError;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::reader::{element_reader, ElementReader};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs;
use std::path::{Path, PathBuf};

/// An [Authority] is responsible for resolving a particular class of
/// schema identifiers.
///
/// The structure of a schema identifier string is defined by the
/// Authority responsible for the schema/type(s) being imported.
pub trait DocumentAuthority: Debug {
    fn elements(&self, id: &str) -> IonSchemaResult<Vec<OwnedElement>>;
}

/// An [Authority] implementation that attempts to resolve schema ids to files
/// relative to a basePath.
#[derive(Debug, Clone)]
pub struct FileSystemDocumentAuthority {
    base_path: PathBuf,
}

impl FileSystemDocumentAuthority {
    pub fn new(base_path: &Path) -> Self {
        Self {
            base_path: base_path.to_path_buf(),
        }
    }

    /// Returns the base path for this [FileSystemAuthority]
    pub fn base_path(&self) -> &Path {
        self.base_path.as_path()
    }
}

impl DocumentAuthority for FileSystemDocumentAuthority {
    /// Returns a vector of [OwnedElement]s based on given schema id
    fn elements(&self, id: &str) -> IonSchemaResult<Vec<OwnedElement>> {
        let absolute_path = self.base_path().join(id);
        // if absolute_path exists for the given id then load schema with file contents
        let ion_content = fs::read(absolute_path)?;
        let iterator = element_reader().iterate_over(&ion_content)?;
        let schema_content = iterator.collect::<Result<Vec<OwnedElement>, IonError>>()?;
        Ok(schema_content)
    }
}

/// An [Authority] implementation that attempts to resolve schema ids to ion elements using the map.
#[derive(Debug, Clone)]
pub struct MapDocumentAuthority {
    ion_content_by_id: HashMap<String, String>, // This map represents (id, ion content) which can used to resolve schema ids to Vec<OwnedElement>
}

impl MapDocumentAuthority {
    pub fn new<'a, I: IntoIterator<Item = (&'a str, &'a str)>>(ion_content_by_id: I) -> Self {
        Self {
            ion_content_by_id: ion_content_by_id
                .into_iter()
                .map(|(id, ion)| (id.to_owned(), ion.to_owned()))
                .collect(),
        }
    }
}

impl DocumentAuthority for MapDocumentAuthority {
    /// Returns a vector of [OwnedElement]s based on given schema id using ion_content_by_id map
    fn elements(&self, id: &str) -> IonSchemaResult<Vec<OwnedElement>> {
        // if ion content exists for the given id  in the map then return ion content as OwnedElements
        let ion_content = self
            .ion_content_by_id
            .get(id)
            .ok_or(unresolvable_schema_error_raw(format!(
                "MapDocumentAuthority does not contain schema with id: {}",
                id
            )))?;
        let iterator = element_reader().iterate_over(ion_content.as_bytes())?;
        let schema_content = iterator.collect::<Result<Vec<OwnedElement>, IonError>>()?;
        Ok(schema_content)
    }
}
