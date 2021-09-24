use crate::isl::isl_constraint::IslConstraint;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::{Element, Struct, SymbolToken};

/// Represents a type in an ISL schema.
#[derive(Debug, Clone, PartialEq)]
pub enum IslType {
    Named(IslTypeImpl),
    Anonymous(IslTypeImpl),
}

impl IslType {
    /// Creates a [IslType::Named] using the [IslConstraint] defined within it
    pub fn named(name: String, constraints: Vec<IslConstraint>) -> IslType {
        IslType::Named(IslTypeImpl::new(Some(name), constraints))
    }

    /// Creates a [IslType::Anonymous] using the [IslConstraint] defined within it
    pub fn anonymous(constraints: Vec<IslConstraint>) -> IslType {
        IslType::Anonymous(IslTypeImpl::new(None, constraints))
    }

    /// Provides the underlying constraints of [IslTypeImpl]
    pub fn constraints(&self) -> &[IslConstraint] {
        match &self {
            IslType::Named(named_type) => named_type.constraints(),
            IslType::Anonymous(anonymous_type) => anonymous_type.constraints(),
        }
    }
}

/// Represents both named and anonymous [IslType] which can be converted to a solid [TypeDefinition] using TypeStore
/// named ISL type grammar: `type:: { name: <NAME>, <CONSTRAINT>...}`
/// anonymous ISL type grammar: `{ <CONSTRAINT>... }`
#[derive(Debug, Clone, PartialEq)]
pub struct IslTypeImpl {
    name: Option<String>,
    constraints: Vec<IslConstraint>,
}

impl IslTypeImpl {
    pub fn new(name: Option<String>, constraints: Vec<IslConstraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn constraints(&self) -> &[IslConstraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedElement] to an [NamedIslType]
    pub fn parse_from_owned_element(ion: &OwnedElement) -> IonSchemaResult<Self> {
        let mut constraints = vec![];
        let annotations: Vec<&OwnedSymbolToken> = ion.annotations().collect();

        let contains_annotations = annotations.contains(&&text_token("type"));

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
            None => None, // If there is no name field then it is an anonymous type
        };

        if contains_annotations && type_name.is_none() {
            // If a named type doesn't have name field throw an error
            return Err(invalid_schema_error_raw(
                "Top level types must have a name field in its definition",
            ));
        } else if !contains_annotations && type_name.is_some() {
            // For named types if it does not have the `type::` annotation throw an error
            return Err(invalid_schema_error_raw(
                "Top level types must have `type::` annotation in their definition",
            ));
        }

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

            // set the isl type name for any error that is thrown while parsing its constraints
            let mut isl_type_name = format!("{:?}", ion_struct);
            if type_name.is_some() {
                isl_type_name = type_name.to_owned().unwrap();
            }

            let constraint =
                IslConstraint::parse_from_ion_element(constraint_name, value, &isl_type_name)?;
            constraints.push(constraint);
        }
        Ok(IslTypeImpl::new(type_name, constraints))
    }
}
