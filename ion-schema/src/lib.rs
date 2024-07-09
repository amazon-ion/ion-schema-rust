// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

use crate::isl::isl_constraint::IslConstraintValue;
use crate::isl::isl_type::IslType;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::{
    Element, IonResult, Struct, StructWriter, Symbol, ValueWriter, WriteAsIon,
};
use regex::Regex;
use std::sync::OnceLock;

/// A `try`-like macro to work around the [`Option`]/[`Result`] nested APIs.
/// These API require checking the type and then calling the appropriate getter function
/// (which returns a None if you got it wrong). This macro turns the `None` into
/// an `IonSchemaError` which cannot be currently done with `?`.
macro_rules! try_to {
    ($getter:expr) => {
        match $getter {
            Some(value) => value,
            None => invalid_schema_error(format!("Missing a value: {}", stringify!($getter)))?,
        }
    };
}

// TODO: consider changing some of these modules to public if required
pub mod authority;
mod constraint;
mod import;
pub(crate) mod ion_extension;
mod ion_path;
mod ion_schema_element;
pub mod isl;
mod nfa;
pub mod result;
pub mod schema;
pub mod system;
mod type_reference;
pub mod types;
pub mod violation;

pub use ion_schema_element::*;

static ISL_VERSION_MARKER_REGEX: OnceLock<Regex> = OnceLock::new();
static RESERVED_WORD_REGEX: OnceLock<Regex> = OnceLock::new();

/// Checks if a value is an ISL version marker.
fn is_isl_version_marker(text: &str) -> bool {
    ISL_VERSION_MARKER_REGEX
        .get_or_init(|| Regex::new(r"^\$ion_schema_\d.*$").unwrap())
        .is_match(text)
}

/// Checks is a value is reserved keyword ISL version maker.
fn is_reserved_word(text: &str) -> bool {
    RESERVED_WORD_REGEX
        .get_or_init(|| Regex::new(r"^(\$ion_schema(_.*)?|[a-z][a-z0-9]*(_[a-z0-9]+)*)$").unwrap())
        .is_match(text)
}

const ISL_2_0_KEYWORDS: [&str; 28] = [
    "all_of",
    "annotations",
    "any_of",
    "as",
    "byte_length",
    "codepoint_length",
    "container_length",
    "contains",
    "element",
    "exponent",
    "field_names",
    "fields",
    "id",
    "imports",
    "name",
    "not",
    "occurs",
    "one_of",
    "ordered_elements",
    "precision",
    "regex",
    "schema_footer",
    "schema_header",
    "timestamp_precision",
    "type",
    "user_reserved_fields",
    "utf8_byte_length",
    "valid_values",
];

// helper function to be used by schema tests
fn load(text: &str) -> Vec<Element> {
    Element::read_all(text.as_bytes())
        .expect("parsing failed unexpectedly")
        .into_iter()
        .collect()
}

// TODO: Move this to a more sensible location
#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserReservedFields {
    schema_header_fields: Vec<String>,
    schema_footer_fields: Vec<String>,
    type_fields: Vec<String>,
}

impl UserReservedFields {
    pub(crate) fn is_empty(&self) -> bool {
        self.type_fields.is_empty()
            && self.schema_header_fields.is_empty()
            && self.schema_footer_fields.is_empty()
    }

    /// Parse use reserved fields inside a [Struct]
    pub(crate) fn from_ion_elements(user_reserved_fields: &Struct) -> IonSchemaResult<Self> {
        if user_reserved_fields.fields().any(|(f, v)| {
            f.text() != Some("schema_header")
                && f.text() != Some("schema_footer")
                && f.text() != Some("type")
        }) {
            return invalid_schema_error(
                "User reserved fields can only have schema_header, schema_footer or type as the field names",
            );
        }
        Ok(Self {
            schema_header_fields: UserReservedFields::field_names_from_ion_elements(
                "schema_header",
                user_reserved_fields,
            )?,
            schema_footer_fields: UserReservedFields::field_names_from_ion_elements(
                "schema_footer",
                user_reserved_fields,
            )?,
            type_fields: UserReservedFields::field_names_from_ion_elements(
                "type",
                user_reserved_fields,
            )?,
        })
    }

    fn field_names_from_ion_elements(
        user_reserved_fields_type: &str,
        user_reserved_fields: &Struct,
    ) -> IonSchemaResult<Vec<String>> {
        let user_reserved_elements: Vec<&Element> = user_reserved_fields
            .get(user_reserved_fields_type)
            .and_then(|it| it.as_sequence().map(|s| s.elements().collect()))
            .ok_or(invalid_schema_error_raw(
                "User reserved fields mut be non null",
            ))?;

        let user_reserved_fields = user_reserved_elements
            .iter()
            .filter(|e| e.annotations().is_empty() && !e.is_null())
            .map(|e| e.as_text().map(|s| s.to_owned()))
            .collect::<Option<Vec<String>>>()
            .unwrap_or(vec![]);

        if user_reserved_fields.len() != user_reserved_elements.len() {
            return invalid_schema_error("User reserved fields mut be unannotated");
        }

        if user_reserved_fields
            .iter()
            .any(|f| is_reserved_word(f) || ISL_2_0_KEYWORDS.binary_search(&f.as_str()).is_ok())
        {
            return invalid_schema_error(
                "ISl 2.0 keywords may not be declared as user reserved fields",
            );
        }

        Ok(user_reserved_fields)
    }

    pub(crate) fn validate_field_names_in_header(
        &self,
        schema_header: &Struct,
    ) -> IonSchemaResult<()> {
        let unexpected_fields: Vec<(&Symbol, &Element)> = schema_header
            .fields()
            .filter(|(f, v)| {
                !self
                    .schema_header_fields
                    .contains(&f.text().unwrap().to_owned())
                    && f.text().unwrap() != "user_reserved_fields"
                    && f.text().unwrap() != "imports"
            })
            .collect();

        if !unexpected_fields.is_empty() {
            // for unexpected fields return invalid schema error
            return invalid_schema_error(format!(
                "schema header contains unexpected fields: {unexpected_fields:?}"
            ));
        }

        Ok(())
    }

    pub(crate) fn validate_field_names_in_type(&self, isl_type: &IslType) -> IonSchemaResult<()> {
        let unexpected_fields: &Vec<&String> = &isl_type
            .constraints()
            .iter()
            .filter(|c| matches!(c.constraint_value, IslConstraintValue::Unknown(_, _)))
            .map(|c| match &c.constraint_value {
                IslConstraintValue::Unknown(f, v) => f,
                _ => {
                    unreachable!("we have already filtered all other constraints")
                }
            })
            .filter(|f| !self.type_fields.contains(f))
            .collect();

        if !unexpected_fields.is_empty() {
            // for unexpected fields return invalid schema error
            return invalid_schema_error(format!(
                "schema type contains unexpected fields: {unexpected_fields:?}"
            ));
        }

        Ok(())
    }

    pub(crate) fn validate_field_names_in_footer(
        &self,
        schema_footer: &Struct,
    ) -> IonSchemaResult<()> {
        let unexpected_fields: Vec<(&Symbol, &Element)> = schema_footer
            .fields()
            .filter(|(f, v)| {
                !self
                    .schema_footer_fields
                    .contains(&f.text().unwrap().to_owned())
            })
            .collect();

        if !unexpected_fields.is_empty() {
            // for unexpected fields return invalid schema error
            return invalid_schema_error(format!(
                "schema footer contains unexpected fields: {unexpected_fields:?}"
            ));
        }
        Ok(())
    }
}

impl WriteAsIon for UserReservedFields {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        let mut struct_writer = writer.struct_writer()?;
        struct_writer
            .field_writer("schema_header")
            .write(&self.schema_header_fields)?;
        struct_writer
            .field_writer("type")
            .write(&self.type_fields)?;
        struct_writer
            .field_writer("schema_footer")
            .write(&self.schema_footer_fields)?;
        struct_writer.close()
    }
}
