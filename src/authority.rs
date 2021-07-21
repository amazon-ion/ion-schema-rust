use crate::result::IonSchemaResult;
use crate::schema::Schema;
use ion_rs::result::IonError;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::reader::{element_reader, ElementReader};
use std::fmt::Debug;
use std::fs;
use std::path::{Path, PathBuf};

/// An [Authority] is responsible for resolving a particular class of
/// schema identifiers.
///
/// The structure of a schema identifier string is defined by the
/// Authority responsible for the schema/type(s) being imported.
pub trait Authority: Debug {
    fn resolve(&self, id: &str) -> IonSchemaResult<Schema>;
}

/// An [Authority] implementation that attempts to resolve schema ids to files
/// relative to a basePath.
#[derive(Debug, Clone)]
pub struct FileSystemAuthority {
    base_path: PathBuf,
}

impl FileSystemAuthority {
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

impl Authority for FileSystemAuthority {
    /// Returns a resolved [Schema] based on given schema id
    fn resolve(&self, id: &str) -> IonSchemaResult<Schema> {
        let absolute_path = self.base_path().join(id);
        // if absolute_path exists for the given id then load schema with file contents
        let ion_content = fs::read(absolute_path)?;
        let mut iterator = element_reader().iterate_over(&ion_content)?;
        let schema_content = iterator.collect::<Result<Vec<OwnedElement>, IonError>>()?;
        Ok(Schema::new(id, schema_content))
    }
}
