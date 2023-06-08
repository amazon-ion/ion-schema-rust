// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

use crate::external::ion_rs::IonType;
use crate::ion_path::IonPath;
use crate::isl::isl_constraint::IslConstraintImpl;
use crate::isl::isl_type::IslTypeImpl;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use crate::violation::{Violation, ViolationCode};
use ion_rs::element::{Element, Struct};
use ion_rs::Symbol;
use regex::Regex;
use std::fmt::{Display, Formatter};
use std::sync::OnceLock;
/// A [`try`]-like macro to workaround the [`Option`]/[`Result`] nested APIs.
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
mod ion_path;
pub mod isl;
mod nfa;
pub mod result;
pub mod schema;
pub mod system;
mod type_reference;
pub mod types;
pub mod violation;

/// Re-export of the ion-rs dependency that is part of our public API.
pub mod external {
    pub use ion_rs;
}

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

/// Provide an Ion schema Element which includes all Elements and a document type
///
/// ## Example:
/// In general `TypeRef` `validate()` takes in IonSchemaElement as the value to be validated.
/// In order to create an `IonSchemaElement`:
///
/// ```
/// use ion_rs::element::Element;
/// use ion_schema::IonSchemaElement;
///
/// // create an IonSchemaElement from an Element
/// let owned_element: Element = 4.into();
/// let ion_schema_element: IonSchemaElement = (&owned_element).into();
///
/// // create an IonSchemaElement for document type based on vector of owned elements
/// let document: IonSchemaElement = IonSchemaElement::Document(vec![owned_element]);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum IonSchemaElement {
    SingleElement(Element),
    Document(Vec<Element>),
}

impl IonSchemaElement {
    pub fn as_element(&self) -> Option<&Element> {
        match self {
            IonSchemaElement::SingleElement(element) => Some(element),
            IonSchemaElement::Document(_) => None,
        }
    }

    pub fn as_document(&self) -> Option<&Vec<Element>> {
        match self {
            IonSchemaElement::SingleElement(_) => None,
            IonSchemaElement::Document(document) => Some(document),
        }
    }

    fn expect_element_of_type(
        &self,
        types: &[IonType],
        constraint_name: &str,
        ion_path: &mut IonPath,
    ) -> Result<&Element, Violation> {
        match self {
            IonSchemaElement::SingleElement(element) => {
                if !types.contains(&element.ion_type()) || element.is_null() {
                    // If it's an Element but the type isn't one of `types`,
                    // return a Violation with the constraint name.
                    return Err(Violation::new(
                        constraint_name,
                        ViolationCode::TypeMismatched,
                        format!("expected {:?} but found {}", types, element.ion_type()),
                        ion_path,
                    ));
                }
                // If it's an Element of an expected type, return a ref to it.
                Ok(element)
            }
            IonSchemaElement::Document(_) => {
                // If it's a Document, return a Violation with the constraint name
                Err(Violation::new(
                    constraint_name,
                    ViolationCode::TypeMismatched,
                    format!("expected {types:?} but found document"),
                    ion_path,
                ))
            }
        }
    }
}

impl Display for IonSchemaElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            IonSchemaElement::SingleElement(element) => {
                write!(f, "{element}")
            }
            IonSchemaElement::Document(document) => {
                write!(f, "/* Ion document */ ")?;
                for value in document {
                    write!(f, "{value} ")?;
                }
                write!(f, "/* end */")
            }
        }
    }
}

impl From<&Element> for IonSchemaElement {
    fn from(value: &Element) -> Self {
        if value.annotations().contains("document") {
            let sequence = match value.ion_type() {
                IonType::String => load(value.as_string().unwrap()),
                IonType::List | IonType::SExp => {
                    let ion_elements: Vec<Element> = value
                        .as_sequence()
                        .unwrap()
                        .elements()
                        .map(|oe| oe.to_owned())
                        .collect();
                    ion_elements
                }
                _ => {
                    panic!("invalid document")
                }
            };
            return IonSchemaElement::Document(sequence);
        }
        IonSchemaElement::SingleElement(value.to_owned())
    }
}

impl From<&Vec<Element>> for IonSchemaElement {
    fn from(value: &Vec<Element>) -> Self {
        IonSchemaElement::Document(value.to_owned())
    }
}

// helper function to be used by schema tests
fn load(text: &str) -> Vec<Element> {
    Element::read_all(text.as_bytes()).expect("parsing failed unexpectedly")
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserReservedFields {
    schema_header_fields: Vec<String>,
    schema_footer_fields: Vec<String>,
    type_fields: Vec<String>,
}

impl UserReservedFields {
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

    pub(crate) fn validate_field_names_in_type(
        &self,
        isl_type: &IslTypeImpl,
    ) -> IonSchemaResult<()> {
        let unexpected_fields: &Vec<&String> = &isl_type
            .constraints()
            .iter()
            .filter(|c| matches!(c, IslConstraintImpl::Unknown(_, _)))
            .map(|c| match c {
                IslConstraintImpl::Unknown(f, v) => f,
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
