use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::{Element, Sequence, Struct, SymbolToken};
use ion_rs::IonType;

/// Represents the public facing schema [IslType] which converts given ion content in the schema file
/// into an internal representation of ISL types(not-yet-resolved types).
#[derive(Debug, Clone, PartialEq)]
pub enum IslType {
    NamedIslType(NamedIslType),
    AnonymousIslType(AnonymousIslType),
}

/// Represents a public facing schema type [AnonymousIslType] which can be converted to a solid [AnonymousTypeDefinition] using TypeStore
/// anonymous ISL type grammar: `{ <CONSTRAINT>... }`
#[derive(Debug, Clone)]
pub struct AnonymousIslType {
    constraints: Vec<IslConstraint>,
}

impl AnonymousIslType {
    pub fn new(constraints: Vec<IslConstraint>) -> Self {
        Self { constraints }
    }

    pub fn constraints(&self) -> &[IslConstraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedElement] to an [AnonymousIslType]
    pub fn parse_from_owned_element(ion: &OwnedElement) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        let ion_struct = try_to!(ion.as_struct());

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
                    return Err(invalid_schema_error_raw(format!(
                        "Type: {:?}
                            can not be built as constraint: {}
                            does not exist",
                        ion_struct, constraint_name
                    )))
                }
            };
            constraints.push(constraint);
        }
        Ok(AnonymousIslType::new(constraints))
    }
}

/// Provides an implementation of PartialEq to compare two IslTypes
impl PartialEq for AnonymousIslType {
    fn eq(&self, other: &Self) -> bool {
        self.constraints.eq(&other.constraints)
    }
}

/// Represents a public facing schema type [NamedIslType] which can be converted to a solid [NamedTypeDefinition] using TypeStore
/// named ISL type grammar: `type:: { name: <NAME>, <CONSTRAINT>...}`
#[derive(Debug, Clone)]
pub struct NamedIslType {
    name: String,
    constraints: Vec<IslConstraint>,
}

impl NamedIslType {
    pub fn new(name: String, constraints: Vec<IslConstraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn constraints(&self) -> &[IslConstraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedElement] to an [NamedIslType]
    pub fn parse_from_owned_element(ion: &OwnedElement) -> IonSchemaResult<Self> {
        let mut constraints = vec![];
        let annotations: Vec<&OwnedSymbolToken> = ion.annotations().collect();

        // For named types check if it has the `type::` annotation
        if !annotations.contains(&&text_token("type")) {
            return Err(invalid_schema_error_raw(
                "Named types must have `type::` annotation in their definition",
            ));
        }

        let ion_struct = try_to!(ion.as_struct());

        // parses the name of the type specified by schema
        let type_name: String = match ion_struct.get("name") {
            Some(name_element) => match name_element.as_str() {
                Some(name) => name.to_owned(),
                None => {
                    return Err(invalid_schema_error_raw(
                        "type names must be a string or a symbol with defined text",
                    ))
                }
            },
            None => {
                return Err(invalid_schema_error_raw(
                    "Named type must have a name field in its definition",
                ))
            } // If a named type doesn't have name field throw an error
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
                            + &type_name
                            + " can not be built as constraint: "
                            + constraint_name
                            + " does not exist",
                    ))
                }
            };
            constraints.push(constraint);
        }
        Ok(NamedIslType::new(type_name, constraints))
    }
}

/// Provides an implementation of PartialEq to compare two IslTypes
impl PartialEq for NamedIslType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.constraints.eq(&other.constraints)
    }
}
