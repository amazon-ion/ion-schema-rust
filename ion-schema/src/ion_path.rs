use ion_rs::value::owned::Element;
use ion_rs::value::Builder;
use ion_rs::Symbol;
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, PartialEq)]
pub enum IonPathElement {
    IndexedElement { index: usize },
    Field { name: String },
}

// TODO: This can be removed once we have a complete Display for violation
// This is currently used by schema sandbox to print nested violations
impl fmt::Debug for IonPathElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            IonPathElement::IndexedElement { index } => {
                write!(f, "{}", index)
            }
            IonPathElement::Field { name } => {
                write!(f, ".{}", name)
            }
        }
    }
}

impl fmt::Display for IonPathElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            IonPathElement::IndexedElement { index } => {
                write!(f, "[{}]", index)
            }
            IonPathElement::Field { name } => {
                write!(f, ".{}", name)
            }
        }
    }
}

#[derive(Default, Clone, PartialEq)]
pub struct IonPath {
    parents: Vec<IonPathElement>,
}

impl IonPath {
    pub fn add_parent(&mut self, parent: IonPathElement) {
        self.parents.push(parent);
    }

    pub fn remove_last_parent(&mut self) -> Option<IonPathElement> {
        self.parents.pop()
    }
}

impl Into<Element> for IonPath {
    fn into(self) -> Element {
        let mut ion_path = vec![];
        for parent in &self.parents {
            let element = match parent {
                IonPathElement::IndexedElement { index } => Element::from(*index as i64),
                IonPathElement::Field { name } => Element::from(Symbol::from(name.as_str())),
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
        write!(f, "./",)?;
        for parent in &self.parents {
            write!(f, "{}", parent)?;
        }
        Ok(())
    }
}

impl fmt::Display for IonPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "./",)?;
        for parent in &self.parents {
            write!(f, "{}", parent)?;
        }
        Ok(())
    }
}
