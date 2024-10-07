use ion_rs::Symbol;
use ion_rs::{Element, SExp, Sequence};
use std::fmt;
use std::fmt::{Debug, Formatter};

/// Represents a single element in Ion Path which is either an index value or a field name depending on its parent container type
#[derive(Clone, PartialEq, PartialOrd, Eq, Hash)]
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

/// Represents the path to the nested Ion value for which the violation occurred.
/// An [IonPath] consists of [IonPathElement]s where each element could either be an index integer or a field name string. [IonPath]s are serialized as s-expressions.
///
/// Here are some examples of serialized paths::
/// ```ion
/// {
///     greetings: [ "hello", "hi", "hey" ]
/// }
/// ```
///
/// For an Ion value given as above if the violation occurred at the top level for given struct then
/// the associated Ion Path would be as following:
/// ```ion
/// () // this empty Ion Path suggests violation occurred at the top level
/// ```
///
/// For the same Ion value the Ion Path associated with list value ["hello", "hi", "hey"]  would be as following:
/// ```ion
/// ( greetings ) // this represents violation occurred at field with `greetings` as the field name for the list value
/// ```
///
/// For an Ion value given as above if the violation occurred for Ion value "hi" then the associated
/// Ion Path would be as below:
/// ```ion
/// ( greetings 1 ) // here 1 represents the index of "hi" in the Ion list value
/// ```
#[derive(Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
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

    pub(crate) fn new(ion_path_elements: Vec<IonPathElement>) -> Self {
        Self { ion_path_elements }
    }
}

impl From<IonPath> for Element {
    fn from(value: IonPath) -> Element {
        let mut ion_path_elements = vec![];
        for parent in &value.ion_path_elements {
            let element = match parent {
                IonPathElement::Index(index) => Element::from(*index as i64),
                IonPathElement::Field(name) => Element::from(Symbol::from(name.as_str())),
            };
            ion_path_elements.push(element);
        }
        SExp::from(Sequence::new(ion_path_elements)).into()
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
