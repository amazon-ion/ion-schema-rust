use ion_rs::value::owned::Element;
use ion_rs::value::Builder;
use ion_rs::Symbol;
use std::fmt;
use std::fmt::{Debug, Formatter};

/// Represents a single element in Ion Path which is either an index value or a field name depending on its parent container type
#[derive(Clone, PartialEq, PartialOrd)]
pub enum IonPathElement {
    Index(usize),
    Field(String),
}

// TODO: This can be removed once we have a complete Display for violation
// This is currently used by schema sandbox to print nested violations
impl fmt::Debug for IonPathElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            IonPathElement::Index(index) => {
                write!(f, "{}", Element::from(*index as i64))
            }
            IonPathElement::Field(name) => {
                write!(f, "{}", Element::from(Symbol::from(name.as_str())))
            }
        }
    }
}

impl fmt::Display for IonPathElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self, f)
    }
}

/// Represents an IonPath associated with given Ion value for which the violation occurred.
/// IonPath consists of IonPathElements where each element could either be an index value or a field name.
///
/// Note: IonPath is Displayed as an SExpression where each element of SExpression is either an index value or a field name.
///
/// Example of IonPath for an Ion value is as following:
/// ```ion
/// {
///     greetings: [ "hello", "hi", "hey" ]
/// }
/// ```
///
/// For an Ion value given as above if the violation occurred for Ion value "hi" then the associated
/// IonPath would be as below:
/// ```ion
/// ( greetings 1 ) // here 1 represents the index of "hi" in the Ion list value
/// ```
///
/// For the same Ion value the IonPath associated with list value ["hello", "hi", "hey"]  would be as following:
/// ```ion
/// ( greetings ) // where `greetings` represents the field name for the list value
/// ```
#[derive(Default, Clone, PartialEq, PartialOrd)]
pub struct IonPath {
    ion_path_elements: Vec<IonPathElement>,
}

impl IonPath {
    pub fn push(&mut self, parent: IonPathElement) {
        self.ion_path_elements.push(parent);
    }

    pub fn pop(&mut self) -> Option<IonPathElement> {
        self.ion_path_elements.pop()
    }
}

impl From<IonPath> for Element {
    fn from(value: IonPath) -> Element {
        let mut ion_path = vec![];
        for parent in &value.ion_path_elements {
            let element = match parent {
                IonPathElement::Index(index) => Element::from(*index as i64),
                IonPathElement::Field(name) => Element::from(Symbol::from(name.as_str())),
            };
            ion_path.push(element);
        }
        Element::new_sexp(ion_path.into_iter())
    }
}

// TODO: This can be removed once we have a complete Display for violation
// This is currently used by schema sandbox to print nested violations
impl fmt::Debug for IonPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ion_path_sexp: Element = self.to_owned().into();
        write!(f, "{ion_path_sexp}")
    }
}

impl fmt::Display for IonPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self, f)
    }
}
