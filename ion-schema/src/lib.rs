// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

use crate::external::ion_rs::IonType;
use crate::ion_path::IonPath;
use crate::isl::isl_constraint::IslConstraintImpl;
use crate::isl::isl_type::IslTypeImpl;
use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::violation::{Violation, ViolationCode};
use ion_rs::value::owned::{Element, Struct};
use ion_rs::value::reader::{element_reader, ElementReader};
use ion_rs::value::{IonElement, IonSequence, IonStruct};
use ion_rs::Symbol;
use std::fmt::{Display, Formatter};
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
pub mod types;
pub mod violation;

/// Re-export of the ion-rs dependency that is part of our public API.
pub mod external {
    pub use ion_rs;
}

/// Provide an Ion schema Element which includes all Elements and a document type
///
/// ## Example:
/// In general `TypeRef` `validate()` takes in IonSchemaElement as the value to be validated.
/// In order to create an `IonSchemaElement`:
///
/// ```
/// use ion_rs::value::owned::Element;
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
        if value.annotations().any(|a| a.text() == Some("document")) {
            let sequence = match value.ion_type() {
                IonType::String => load(value.as_str().unwrap()),
                IonType::List | IonType::SExpression => {
                    let ion_elements: Vec<Element> = value
                        .as_sequence()
                        .unwrap()
                        .iter()
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
    element_reader()
        .read_all(text.as_bytes())
        .expect("parsing failed unexpectedly")
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserReservedFields {
    schema_header_fields: Vec<String>,
    schema_footer_fields: Vec<String>,
    type_fields: Vec<String>,
}

impl UserReservedFields {
    /// Parse use reserved fields inside a [Struct]
    pub(crate) fn from_ion_elements(user_reserved_fields: &Struct) -> Self {
        Self {
            schema_header_fields: UserReservedFields::field_names_from_ion_elements(
                "schema_header",
                user_reserved_fields,
            ),
            schema_footer_fields: UserReservedFields::field_names_from_ion_elements(
                "schema_footer",
                user_reserved_fields,
            ),
            type_fields: UserReservedFields::field_names_from_ion_elements(
                "type",
                user_reserved_fields,
            ),
        }
    }

    fn field_names_from_ion_elements(
        user_reserved_fields_type: &str,
        user_reserved_fields: &Struct,
    ) -> Vec<String> {
        let user_reserved_elements: Vec<&Element> = user_reserved_fields
            .get(user_reserved_fields_type)
            .and_then(|it| it.as_sequence().map(|s| s.iter().collect()))
            .unwrap_or(vec![]);

        user_reserved_elements
            .iter()
            .map(|e| e.as_str().map(|s| s.to_owned()))
            .collect::<Option<Vec<String>>>()
            .unwrap_or(vec![])
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
