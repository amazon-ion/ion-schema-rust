use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Struct};

/// Represents an import in an ISL schema.
/// For more information: https://amzn.github.io/ion-schema/docs/spec.html#imports
#[derive(Debug, Clone, PartialEq)]
pub enum IslImport {
    Schema(String),
    Type(IslImportImpl),
    TypeAlias(IslImportImpl),
}

impl IslImport {
    pub fn id(&self) -> &String {
        match self {
            IslImport::Schema(id) => id,
            IslImport::Type(isl_import_impl) => isl_import_impl.id(),
            IslImport::TypeAlias(isl_import_impl) => isl_import_impl.id(),
        }
    }
    /// Parse constraints inside an [OwnedElement] to an [IslImport]
    pub fn parse_from_ion_element(value: &OwnedElement) -> IonSchemaResult<IslImport> {
        let import = try_to!(value.as_struct());
        let id = match import.get("id") {
            Some(import_id) => try_to!(import_id.as_str()),
            None => {
                return Err(invalid_schema_error_raw(
                    "import must have an id field in its definition",
                ))
            }
        };

        let type_name = match import.get("type") {
            Some(type_name) => try_to!(type_name.as_str()),
            None => return Ok(IslImport::Schema(id.to_owned())),
        };

        let alias = import
            .get("as")
            .and_then(|alias| alias.as_str().and_then(|a| Some(a.to_owned())));

        Ok(IslImport::TypeAlias(IslImportImpl::new(
            id.to_owned(),
            type_name.to_owned(),
            alias,
        )))
    }
}

/// Represents typed and type aliased [IslImport]s
/// Typed import grammar: `{ id: <ID>, type: <TYPE_NAME> }`
/// Type aliased import grammar: `{ id: <ID>, type: <TYPE_NAME>, as: <TYPE_ALIAS> }`
#[derive(Debug, Clone, PartialEq)]
pub struct IslImportImpl {
    id: String,
    type_name: String,
    alias: Option<String>,
}

impl IslImportImpl {
    pub fn new(id: String, type_name: String, alias: Option<String>) -> Self {
        Self {
            id,
            type_name,
            alias,
        }
    }

    fn id(&self) -> &String {
        &self.id
    }
}
