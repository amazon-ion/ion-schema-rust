use std::fmt::Debug;
use crate::result::{IonSchemaResult, unresolvable_schema_error};
use crate::schema::Schema;
use std::path::{Path, PathBuf};
use ion_rs::value::reader::{element_reader, ElementReader};
use std::fs;

/// An [Authority] is responsible for resolving a particular class of
/// schema identifiers.
///
/// The structure of a schema identifier string is defined by the
/// Authority responsible for the schema/type(s) being imported.
pub trait Authority: Debug {
    fn resolve(&self, id: String) -> IonSchemaResult<Schema>;
    fn get_base_path(&self) -> &Path;
}

/// An [Authority] implementation that attempts to resolve schema ids to files
/// relative to a basePath.
#[derive(Debug, Clone)]
pub struct FileSystemAuthority {
    base_path: PathBuf,
}

impl FileSystemAuthority {
    pub fn new(base_path: &Path) -> Self {
        let mut root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        Self{
            base_path: root_path.join(base_path),
        }
    }
}

impl Authority for FileSystemAuthority {
    /// Returns a resolved [Schema] based on given schema id
    fn resolve(&self, id: String) -> IonSchemaResult<Schema> {
        let absolute_path = self.get_base_path().join(&id);
        // if absolute_path exists for the given id then load schema with file contents
        let ion_data = fs::read_to_string(absolute_path).expect("Unable to read file!");
        let mut iterator = element_reader().iterate_over(ion_data.as_ref()).unwrap();
        let mut peekable_iterator = &mut iterator.peekable();
        if peekable_iterator.peek().is_some() {
            let schema_content = peekable_iterator.into_iter().flat_map(|e| e.ok()).collect();
            return Ok(Schema::new(id, schema_content))
        }
        unresolvable_schema_error("The schema: ".to_owned() + id.as_str() + " is unresolvable")
    }

    /// Returns the base path for this [Authority]
    fn get_base_path(&self) -> &Path {
        self.base_path.as_path()
    }
}