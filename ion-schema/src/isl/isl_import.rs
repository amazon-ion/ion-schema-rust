use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::element::Element;

/// Represents an import in an ISL schema.
/// For more information: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IslImport {
    Schema(String),
    Type(IslImportType),
    TypeAlias(IslImportType),
}

impl IslImport {
    pub fn id(&self) -> &String {
        match self {
            IslImport::Schema(id) => id,
            IslImport::Type(isl_import_impl) => isl_import_impl.id(),
            IslImport::TypeAlias(isl_import_impl) => isl_import_impl.id(),
        }
    }

    /// Parse constraints inside an [Element] to an [IslImport]
    pub fn from_ion_element(value: &Element) -> IonSchemaResult<IslImport> {
        let import = try_to!(value.as_struct());
        let id = match import.get("id") {
            Some(import_id) => try_to!(import_id.as_string()),
            None => {
                return Err(invalid_schema_error_raw(
                    "import must have an id field in its definition",
                ))
            }
        };

        let type_name = match import.get("type") {
            Some(type_name) => try_to!(type_name.as_text()),
            None => return Ok(IslImport::Schema(id.to_owned())),
        };

        let alias = match import.get("as") {
            Some(alias) => alias.as_text().map(|a| a.to_owned()),
            None => {
                return Ok(IslImport::Type(IslImportType::new(
                    id.to_owned(),
                    type_name.to_owned(),
                    None,
                )))
            }
        };

        Ok(IslImport::TypeAlias(IslImportType::new(
            id.to_owned(),
            type_name.to_owned(),
            alias,
        )))
    }
}

/// Represents typed and type aliased [IslImport]s
/// Typed import grammar: `{ id: <ID>, type: <TYPE_NAME> }`
/// Type aliased import grammar: `{ id: <ID>, type: <TYPE_NAME>, as: <TYPE_ALIAS> }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IslImportType {
    id: String,
    type_name: String,
    alias: Option<String>,
}

impl IslImportType {
    pub fn new(id: String, type_name: String, alias: Option<String>) -> Self {
        Self {
            id,
            type_name,
            alias,
        }
    }

    pub fn id(&self) -> &String {
        &self.id
    }

    pub fn type_name(&self) -> &String {
        &self.type_name
    }

    pub fn alias(&self) -> &Option<String> {
        &self.alias
    }
}
