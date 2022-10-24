//! Provides a way to construct [`DocumentAuthority`].
//!
//! A [`DocumentAuthority`] is responsible for resolving a particular class of
//! schema identifiers as per *[Ion Schema spec]*.
//!
//! There are two types of a [`DocumentAuthority`] as defined below.
//! * [`FileSystemDocumentAuthority`] : Attempts to resolve schema ids to files relative to a basePath.
//! * [`MapDocumentAuthority`] : Attempts to resolve schema ids to ion elements using the map of (id, ion content).
//!
//! [Ion Schema spec]: https://amzn.github.io/ion-schema/docs/spec.html#schema-authorities
//!
//! ## Example usage of `authority` module to create a [`DocumentAuthority`]:
//! ```
//! use ion_schema::authority::{MapDocumentAuthority, FileSystemDocumentAuthority};
//! use std::path::Path;
//!
//! // map with (id, ion content) to represent `sample_number` schema
//! let map_authority = [
//!     (
//!         "sample_number.isl",
//!         r#"
//!             schema_header::{
//!                 imports: [{ id: "sample_decimal.isl", type: my_decimal, as: other_decimal }],
//!             }
//!             type::{
//!                 name: my_int,
//!                 type: int,
//!             }
//!             type::{
//!                 name: my_number,
//!                 all_of: [
//!                     my_int,
//!                     other_decimal,
//!                  ],
//!             }
//!             schema_footer::{
//!            }
//!         "#,
//!   ),
//!   (
//!         "sample_decimal.isl",
//!         r#"
//!             schema_header::{
//!                 imports: [],
//!             }
//!             type::{
//!                 name: my_decimal,
//!                 type: decimal,
//!              }
//!              schema_footer::{
//!              }
//!         "#,
//!    ),
//! ];
//!
//! let map_document_authority = MapDocumentAuthority::new(map_authority);
//!
//! let file_system_document_authority = FileSystemDocumentAuthority::new(Path::new(
//!     "sample_schemas",
//! ));
//! ```

use crate::result::{unresolvable_schema_error_raw, IonSchemaResult};
use ion_rs::result::IonError;
use ion_rs::value::owned::Element;
use ion_rs::value::reader::{element_reader, ElementReader};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs;
use std::path::{Path, PathBuf};

/// An [`DocumentAuthority`] is responsible for resolving a particular class of
/// schema identifiers.
///
/// The structure of a schema identifier string is defined by the
/// Authority responsible for the schema/type(s) being imported.
pub trait DocumentAuthority: Debug {
    fn elements(&self, id: &str) -> IonSchemaResult<Vec<Element>>;
}

/// An [`DocumentAuthority`] implementation that attempts to resolve schema ids to files
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

    /// Returns the base path for this [`FileSystemDocumentAuthority`]
    pub fn base_path(&self) -> &Path {
        self.base_path.as_path()
    }
}

impl DocumentAuthority for FileSystemDocumentAuthority {
    /// Returns a vector of [`Element`]s based on given schema id
    fn elements(&self, id: &str) -> IonSchemaResult<Vec<Element>> {
        let absolute_path = self.base_path().join(id);
        // if absolute_path exists for the given id then load schema with file contents
        let ion_content = fs::read(absolute_path)?;
        let iterator = element_reader().iterate_over(&ion_content)?;
        let schema_content = iterator.collect::<Result<Vec<Element>, IonError>>()?;
        Ok(schema_content)
    }
}

/// An [`DocumentAuthority`] implementation that attempts to resolve schema ids to ion elements using the map.
#[derive(Debug, Clone)]
pub struct MapDocumentAuthority {
    ion_content_by_id: HashMap<String, String>, // This map represents (id, ion content) which can used to resolve schema ids to Vec<Element>
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
    /// Returns a vector of [`Element`]s based on given schema id using ion_content_by_id map
    fn elements(&self, id: &str) -> IonSchemaResult<Vec<Element>> {
        // if ion content exists for the given id  in the map then return ion content as Elements
        let ion_content = self.ion_content_by_id.get(id).ok_or_else(|| {
            unresolvable_schema_error_raw(format!(
                "MapDocumentAuthority does not contain schema with id: {}",
                id
            ))
        })?;
        let iterator = element_reader().iterate_over(ion_content.as_bytes())?;
        let schema_content = iterator.collect::<Result<Vec<Element>, IonError>>()?;
        Ok(schema_content)
    }
}
