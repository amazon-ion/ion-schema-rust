use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaError,
    IonSchemaResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::TypeDefinition;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence, Struct, SymbolToken};
use ion_rs::IonType;
use std::convert::{TryFrom, TryInto};

// TODO: create a module for isl file and modularize different isl* structs defined below
// TODO: add a module level documentation for isl module
/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint {
    AllOf(Vec<IslTypeRef>),
    Type(IslTypeRef),
}

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

/// Provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]
/// [Ion Schema spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
#[derive(Debug, Clone, PartialEq)]
pub enum IslTypeRef {
    /// Represents a reference to one of ISL's built-in core types:
    CoreIslType(IonType),
    /// Represents a reference to a named type (including aliases)
    NamedType(String),
    /// Represents a type reference defined as an inlined import type from another schema
    // TODO: add ImportType(Import) where ImportType could either point to a schema represented by an id with all the types or a single type from inside it
    /// represents an unnamed type definition reference
    AnonymousType(IslType),
}

// TODO: add a check for nullable type reference
impl IslTypeRef {
    /// Tries to create an [IslTypeRef] from the given OwnedElement
    pub fn parse_from_ion_element(value: &OwnedElement) -> IonSchemaResult<Self> {
        match value.ion_type() {
            IonType::Symbol => {
                value.as_sym().unwrap()
                    .text()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "a base or alias type reference symbol doesn't have text",
                        )
                    })
                    .and_then(|type_name| {
                        let ion_type = match type_name {
                            "int" => IslTypeRef::CoreIslType(IonType::Integer),
                            "float" => IslTypeRef::CoreIslType(IonType::Float),
                            "decimal" => IslTypeRef::CoreIslType(IonType::Decimal),
                            "timestamp" => IslTypeRef::CoreIslType(IonType::Timestamp),
                            "string" => IslTypeRef::CoreIslType(IonType::String),
                            "symbol" => IslTypeRef::CoreIslType(IonType::Symbol),
                            "bool" => IslTypeRef::CoreIslType(IonType::Boolean),
                            "blob" => IslTypeRef::CoreIslType(IonType::Blob),
                            "clob" => IslTypeRef::CoreIslType(IonType::Clob),
                            "sexp" => IslTypeRef::CoreIslType(IonType::SExpression),
                            "list" => IslTypeRef::CoreIslType(IonType::List),
                            "struct" => IslTypeRef::CoreIslType(IonType::Struct),
                            // TODO: add a match for other core types like: lob, text, number, document, any
                            _ => IslTypeRef::NamedType(type_name.to_owned()),
                        };
                        Ok(ion_type)
                    })
            }
            IonType::Struct =>
                Ok(IslTypeRef::AnonymousType(value.try_into()?)),
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    // TODO: break match arms into helper methods as we add more constraints
    /// Resolves a type_reference into a [TypeId] using the type_store
    pub fn resolve_type_reference(
        type_reference: &IslTypeRef,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<TypeId> {
        match type_reference {
            IslTypeRef::CoreIslType(ion_type) => {
                // TODO: create CoreType struct for storing ISLCoreType type definition instead of Type
                // inserts ISLCoreType as a Type into type_store
                Ok(pending_types.add_named_type(
                    &format!("{:?}", ion_type),
                    TypeDefinition::new(Some(format!("{:?}", ion_type)), vec![]),
                    type_store,
                ))
            }
            IslTypeRef::NamedType(alias) => {
                // verify if the AliasType actually exists in the type_store or throw an error
                match pending_types.get_type_id_by_name(alias, type_store) {
                    Some(type_id) => Ok(type_id.to_owned()),
                    None => match pending_types.get_parent() {
                        Some(parent) => {
                            // if it is a self referencing type resolve it using parent information from type_store
                            if parent.0.eq(alias) {
                                Ok(parent.1)
                            } else {
                                unresolvable_schema_error(format!(
                                    "Could not resolve type reference: {:?} does not exist",
                                    alias
                                ))
                            }
                        }
                        None => unresolvable_schema_error(format!(
                            "Could not resolve type reference: {:?} does not exist",
                            alias
                        )),
                    },
                }
            }
            IslTypeRef::AnonymousType(isl_type) => {
                let type_id = pending_types.get_total_types();
                let type_def = TypeDefinition::parse_from_isl_type_and_update_type_store(
                    isl_type,
                    type_store,
                    pending_types,
                )?;
                // get the last added anonymous type's type_id for given anonymous type
                Ok(type_id)
            } //TODO: add a check for ImportType type reference here
        }
    }
}

#[cfg(test)]
mod isl_tests {
    use super::*;
    use ion_rs::value::reader::element_reader;
    use ion_rs::value::reader::ElementReader;
    use rstest::*;

    // helper function to be used by isl tests
    fn load(text: &str) -> IslType {
        (&element_reader()
            .read_one(text.as_bytes())
            .expect("parsing failed unexpectedly"))
            .try_into()
            .unwrap()
    }

    #[rstest(
    isl_type1,isl_type2,
    case::type_constraint_with_anonymous_type(
        load(r#" // For a schema with single anonymous type
            {type: int}
        "#),
        IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])
    ),
    case::type_constraint_with_named_type(
        load(r#" // For a schema with named type 
            { name: my_int, type: int }
        "#),
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])
    ),
    case::type_constraint_with_self_reference_type(
        load(r#" // For a schema with self reference type
            { name: my_int, type: my_int }
        "#),
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])
    ),
    case::type_constraint_with_nested_self_reference_type(
        load(r#" // For a schema with nested self reference type
            { name: my_int, type: { type: my_int } }
        "#),
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))])
    ),
    case::type_constraint_with_nested_type(
        load(r#" // For a schema with nested types
            { name: my_int, type: { type: int } }
        "#),
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])))])
    ),
    case::type_constraint_with_nested_multiple_types(
        load(r#" // For a schema with nested multiple types
            { name: my_int, type: { type: int }, type: { type: my_int } }
        "#),
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))), IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))])
    ),
    case::all_of_constraint(
        load(r#" // For a schema with all_of type as below: 
            { all_of: [{ type: int }] }
        "#),
    IslType::new(None, vec![IslConstraint::AllOf(vec![IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))])])
    ),
    )]
    fn owned_struct_to_isl_type(isl_type1: IslType, isl_type2: IslType) {
        // assert if both the IslType are same in terms of constraints and name
        assert_eq!(isl_type1, isl_type2);
    }
}
