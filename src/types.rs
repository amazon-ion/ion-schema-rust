use crate::constraint::Constraints;
use crate::result::{invalid_schema_error_raw, IonSchemaError};
use crate::violation::Violations;
use ion_rs::value::owned::{OwnedElement, OwnedStruct};
use ion_rs::value::{Element, Struct, SymbolToken};
use std::convert::{TryFrom, TryInto};

/// A Type consists of an optional name and zero or more constraints.
///
/// Unless otherwise specified, the constraint `type: any` is automatically applied.
#[derive(Debug, Clone)]
pub struct Type {
    name: String,
    constraints: Vec<Constraints>,
}

impl Type {
    pub fn new(name: String, constraints: Vec<Constraints>) -> Self {
        Self { name, constraints }
    }

    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: OwnedElement) -> bool {
        todo!()
    }

    /// Returns a Violations object indicating whether the specified value
    /// is valid for this type, and if not, provides details as to which
    /// constraints were violated.
    fn validate(&self, value: OwnedElement) -> Violations {
        todo!()
    }
}

/// Parse constraints inside an [OwnedStruct] to a schema [Type]
impl TryFrom<&OwnedStruct> for Type {
    type Error = IonSchemaError;

    fn try_from(ion_struct: &OwnedStruct) -> Result<Self, Self::Error> {
        let mut constraints = vec![];

        // parses the name of the type specified by schema
        let type_name=  match ion_struct.get("name") {
            Some(name_element) => match name_element.as_str() {
                Some(name) => name.to_owned(),
                None => { return Err(invalid_schema_error_raw("A type name is not string/symbol, if the value is any null, or the text of the symbol is not defined.")) }
            },
            None => "".to_owned()
        };

        // parses all the constraints inside a Type
        for (field_name, value) in ion_struct.iter() {
            let constraint_name = match field_name.text() {
                Some("name") => continue, // if the field_name is "name" then it's the type name not a constraint
                Some(name) => name,
                None => {
                    return Err(invalid_schema_error_raw(
                        "A type name symbol token does not have any text",
                    ))
                }
            };
            // TODO: add more constraints to match below
            let constraint = match constraint_name {
                "all_of" => {
                    if let Ok(all_of) = value.try_into() {
                        Constraints::AllOf(all_of)
                    } else {
                        return Err(invalid_schema_error_raw(
                            "Can not read all_of constraint in Ion",
                        ));
                    }
                }
                _ => {
                    return Err(invalid_schema_error_raw(
                        "Type".to_owned()
                            + &type_name
                            + "can not be built as constraint "
                            + constraint_name
                            + " does not exist",
                    ))
                }
            };
            constraints.push(constraint);
        }
        Ok(Type::new(type_name, constraints))
    }
}
