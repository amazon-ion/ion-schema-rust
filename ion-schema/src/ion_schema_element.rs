use crate::ion_path::IonPath;
use crate::violation::{Violation, ViolationCode};
use ion_rs::{Element, IonType, Struct};
use std::fmt::{Display, Formatter};

/// Extends [IonType] by adding a "Document" variant for Ion Schema.
#[derive(Debug, Clone, PartialEq, Copy)]
pub(crate) enum IonSchemaElementType {
    Null,
    Bool,
    Int,
    Float,
    Decimal,
    Timestamp,
    Symbol,
    String,
    Clob,
    Blob,
    List,
    SExp,
    Struct,
    Document,
}

impl Display for IonSchemaElementType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            IonSchemaElementType::Null => "null",
            IonSchemaElementType::Bool => "bool",
            IonSchemaElementType::Int => "int",
            IonSchemaElementType::Float => "float",
            IonSchemaElementType::Decimal => "decimal",
            IonSchemaElementType::Timestamp => "timestamp",
            IonSchemaElementType::Symbol => "symbol",
            IonSchemaElementType::String => "string",
            IonSchemaElementType::Clob => "clob",
            IonSchemaElementType::Blob => "blob",
            IonSchemaElementType::List => "list",
            IonSchemaElementType::SExp => "sexp",
            IonSchemaElementType::Struct => "struct",
            IonSchemaElementType::Document => "document",
        };
        f.write_str(text)
    }
}

impl From<IonType> for IonSchemaElementType {
    fn from(value: IonType) -> Self {
        match value {
            IonType::Null => IonSchemaElementType::Null,
            IonType::Bool => IonSchemaElementType::Bool,
            IonType::Int => IonSchemaElementType::Int,
            IonType::Float => IonSchemaElementType::Float,
            IonType::Decimal => IonSchemaElementType::Decimal,
            IonType::Timestamp => IonSchemaElementType::Timestamp,
            IonType::Symbol => IonSchemaElementType::Symbol,
            IonType::String => IonSchemaElementType::String,
            IonType::Clob => IonSchemaElementType::Clob,
            IonType::Blob => IonSchemaElementType::Blob,
            IonType::List => IonSchemaElementType::List,
            IonType::SExp => IonSchemaElementType::SExp,
            IonType::Struct => IonSchemaElementType::Struct,
        }
    }
}

/// Internal-only backing representation for [IonSchemaElement].
#[derive(Debug, Clone, PartialEq)]
enum IonSchemaElementKind<'a> {
    SingleElement(&'a Element),
    Document(&'a [Element]),
}

/// Represents a value that can be validated by Ion Schema.
///
/// An [IonSchemaElement] can be constructed from [Element] to represent a single Ion value, or
/// from [Document] to represent the Ion Schema document type.
///
/// In general, users do not need to construct this directly. Ion Schema APIs accept
/// `Into<IonSchemaElement>` rather than directly accepting `IonSchemaElement`.
///
/// See [TypeDefinition::validate] for examples of use.
#[derive(Debug, Clone, PartialEq)]
pub struct IonSchemaElement<'a> {
    content: IonSchemaElementKind<'a>,
}

impl<'a> IonSchemaElement<'a>
where
    Self: 'a,
{
    pub(crate) fn as_sequence_iter(&'a self) -> Option<impl Iterator<Item = &'a Element>> {
        match &self.content {
            IonSchemaElementKind::SingleElement(e) => e
                .as_sequence()
                .map(|s| AsRef::<[Element]>::as_ref(s).iter()),
            IonSchemaElementKind::Document(d) => Some(d.iter()),
        }
    }

    pub(crate) fn as_struct(&'a self) -> Option<&'a Struct> {
        match self.content {
            IonSchemaElementKind::SingleElement(e) => e.as_struct(),
            _ => None,
        }
    }

    pub(crate) fn as_element(&'a self) -> Option<&'a Element> {
        match self.content {
            IonSchemaElementKind::SingleElement(element) => Some(element),
            _ => None,
        }
    }

    pub(crate) fn as_document(&'a self) -> Option<impl Iterator<Item = &'a Element>> {
        match &self.content {
            IonSchemaElementKind::Document(d) => Some(d.iter()),
            _ => None,
        }
    }

    pub(crate) fn ion_schema_type(&self) -> IonSchemaElementType {
        match self.as_element() {
            Some(e) => e.ion_type().into(),
            _ => IonSchemaElementType::Document,
        }
    }

    pub(crate) fn is_null(&self) -> bool {
        match self.as_element() {
            Some(e) => e.is_null(),
            _ => false,
        }
    }

    pub(crate) fn expect_element_of_type(
        &self,
        types: &[IonType],
        constraint_name: &str,
        ion_path: &mut IonPath,
    ) -> Result<&Element, Violation> {
        match self.as_element() {
            Some(element) => {
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
            None => {
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

impl<'a> Display for IonSchemaElement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.content {
            IonSchemaElementKind::SingleElement(element) => {
                write!(f, "{element}")
            }
            IonSchemaElementKind::Document(document) => {
                write!(f, "/* Ion document */ ")?;
                for value in document.iter() {
                    write!(f, "{value} ")?;
                }
                write!(f, "/* end */")
            }
        }
    }
}

impl<'a> From<&'a Element> for IonSchemaElement<'a> {
    fn from(value: &'a Element) -> Self {
        IonSchemaElement {
            content: IonSchemaElementKind::SingleElement(value),
        }
    }
}

/// Marker type to indicate that a sequence of [Element] should be validated as a `document`.
pub struct DocumentHint<'a>(&'a [Element]);

pub trait AsDocumentHint<'a> {
    fn as_document(&'a self) -> DocumentHint<'a>;
}

impl<'a, T> AsDocumentHint<'a> for T
where
    T: AsRef<[Element]> + 'a,
{
    fn as_document(&'a self) -> DocumentHint<'a> {
        DocumentHint(self.as_ref())
    }
}

impl<'a> From<DocumentHint<'a>> for IonSchemaElement<'a> {
    fn from(value: DocumentHint<'a>) -> Self {
        let content: IonSchemaElementKind = IonSchemaElementKind::Document(value.0);
        IonSchemaElement { content }
    }
}
