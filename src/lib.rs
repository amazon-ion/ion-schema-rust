// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

use crate::external::ion_rs::IonType;
use crate::violation::{Violation, ViolationCode};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::reader::{element_reader, ElementReader};
use ion_rs::value::{Element, Sequence, SymbolToken};
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
pub mod isl;
pub mod result;
pub mod schema;
pub mod system;
pub mod types;
mod violation;

/// Re-export of the ion-rs dependency that is part of our public API.
pub mod external {
    pub use ion_rs;
}

/// Provide an Ion schema element which includes all OwnedElements and a document type
///
/// ## Example:
/// In general `TypeRef` `validate()` takes in IonSchemaElement as the value to be validated.
/// In order to create an `IonSchemaElement`:
///
/// ```
/// use ion_rs::value::owned::OwnedElement;
/// use ion_schema::IonSchemaElement;
///
/// // create an IonSchemaElement from an OwnedElement
/// let owned_element: OwnedElement = 4.into();
/// let ion_schema_element: IonSchemaElement = (&owned_element).into();
///
/// // create an IonSchemaElement for document type based on vector of owned elements
/// let document: IonSchemaElement = IonSchemaElement::Document(vec![owned_element]);
/// ```
#[derive(Debug, Clone)]
pub enum IonSchemaElement {
    SingleElement(OwnedElement),
    Document(Vec<OwnedElement>),
}

impl IonSchemaElement {
    pub fn as_element(&self) -> Option<&OwnedElement> {
        match self {
            IonSchemaElement::SingleElement(element) => Some(element),
            IonSchemaElement::Document(_) => None,
        }
    }

    pub fn as_document(&self) -> Option<&Vec<OwnedElement>> {
        match self {
            IonSchemaElement::SingleElement(_) => None,
            IonSchemaElement::Document(document) => Some(document),
        }
    }

    fn expect_element_of_type(
        &self,
        types: &[IonType],
        constraint_name: &str,
    ) -> Result<&OwnedElement, Violation> {
        match self {
            IonSchemaElement::SingleElement(element) => {
                if !types.contains(&element.ion_type()) || element.is_null() {
                    // If it's an OwnedElement but the type isn't one of `types`,
                    // return a Violation with the constraint name.
                    return Err(Violation::new(
                        constraint_name,
                        ViolationCode::TypeMismatched,
                        &format!("expected {:?} but found {}", types, element.ion_type()),
                    ));
                }
                // If it's an OwnedElement of an expected type, return a ref to it.
                Ok(element)
            }
            IonSchemaElement::Document(_) => {
                // If it's a Document, return a Violation with the constraint name
                Err(Violation::new(
                    constraint_name,
                    ViolationCode::TypeMismatched,
                    &format!("expected {:?} but found document", types),
                ))
            }
        }
    }
}

impl Display for IonSchemaElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            IonSchemaElement::SingleElement(element) => {
                write!(f, "{}", element)
            }
            IonSchemaElement::Document(document) => {
                write!(f, "/* Ion document */ ")?;
                for value in document {
                    write!(f, "{} ", value)?;
                }
                write!(f, "/* end */")
            }
        }
    }
}

impl From<&OwnedElement> for IonSchemaElement {
    fn from(value: &OwnedElement) -> Self {
        if value.annotations().any(|a| a.text() == Some("document")) {
            let sequence = match value.ion_type() {
                IonType::String => load(value.as_str().unwrap()),
                IonType::List | IonType::SExpression => {
                    let elements: Vec<OwnedElement> = value
                        .as_sequence()
                        .unwrap()
                        .iter()
                        .map(|oe| oe.to_owned())
                        .collect();
                    elements
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

impl From<&Vec<OwnedElement>> for IonSchemaElement {
    fn from(value: &Vec<OwnedElement>) -> Self {
        IonSchemaElement::Document(value.to_owned())
    }
}

// helper function to be used by schema tests
fn load(text: &str) -> Vec<OwnedElement> {
    element_reader()
        .read_all(text.as_bytes())
        .expect("parsing failed unexpectedly")
}
