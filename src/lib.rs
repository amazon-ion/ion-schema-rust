// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

use crate::external::ion_rs::IonType;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::reader::{element_reader, ElementReader};
use ion_rs::value::{Element, Sequence, SymbolToken};
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
pub enum IonSchemaElement {
    Element(OwnedElement),
    Document(Vec<OwnedElement>),
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
        IonSchemaElement::Element(value.to_owned())
    }
}

// helper function to be used by schema tests
fn load(text: &str) -> Vec<OwnedElement> {
    element_reader()
        .read_all(text.as_bytes())
        .expect("parsing failed unexpectedly")
}
