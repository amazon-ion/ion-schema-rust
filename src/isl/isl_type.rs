use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence, Struct, SymbolToken};
use ion_rs::IonType;
use std::convert::TryFrom;

/// Represents a public facing schema type [IslType] which can be converted to a solid TypeRef using TypeStore
#[derive(Debug, Clone)]
pub struct IslType {
    name: Option<String>,
    constraints: Vec<IslConstraint>,
}

impl IslType {
    pub fn new(name: Option<String>, constraints: Vec<IslConstraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn constraints(&self) -> &[IslConstraint] {
        &self.constraints
    }
}

/// Provides an implementation of PartialEq to compare two IslTypes
impl PartialEq for IslType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.constraints.eq(&other.constraints)
    }
}

/// Parse constraints inside an [OwnedStruct] to an [IslType]
impl TryFrom<&OwnedElement> for IslType {
    type Error = IonSchemaError;

    fn try_from(ion: &OwnedElement) -> Result<Self, Self::Error> {
        let mut constraints = vec![];

        let ion_struct = try_to!(ion.as_struct());

        // parses the name of the type specified by schema
        let type_name: Option<String> = match ion_struct.get("name") {
            Some(name_element) => match name_element.as_str() {
                Some(name) => Some(name.to_owned()),
                None => {
                    return Err(invalid_schema_error_raw(
                        "type names must be a string or a symbol with defined text",
                    ))
                }
            },
            None => None, // If the type is UNNAMED_TYPE_DEFINITION/ AnonymousType then set name as None
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
                    //TODO: create a method/macro for this ion type check which can be reused
                    if value.ion_type() != IonType::List {
                        return Err(invalid_schema_error_raw(format!(
                            "all_of constraint was a {:?} instead of a list",
                            value.ion_type()
                        )));
                    }
                    let types: Vec<IslTypeRef> = value
                        .as_sequence()
                        .unwrap()
                        .iter()
                        .map(|e| IslTypeRef::parse_from_ion_element(e))
                        .collect::<IonSchemaResult<Vec<IslTypeRef>>>()?;
                    IslConstraint::AllOf(types)
                }
                "type" => {
                    if value.ion_type() != IonType::Symbol && value.ion_type() != IonType::Struct {
                        return Err(invalid_schema_error_raw(format!(
                            "type constraint was a {:?} instead of a symbol/struct",
                            value.ion_type()
                        )));
                    }
                    let type_reference: IslTypeRef = IslTypeRef::parse_from_ion_element(value)?;
                    IslConstraint::Type(type_reference)
                }
                _ => {
                    return Err(invalid_schema_error_raw(
                        "Type: ".to_owned()
                            + &type_name.unwrap()
                            + " can not be built as constraint: "
                            + constraint_name
                            + " does not exist",
                    ))
                }
            };
            constraints.push(constraint);
        }
        Ok(IslType::new(type_name, constraints))
    }
}
