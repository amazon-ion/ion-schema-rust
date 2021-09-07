use crate::result::{
    invalid_schema_error_raw, unresolvable_schema_error, IonSchemaError, IonSchemaResult,
};
use crate::system::{SharedTypeStore, TypeId};
use crate::types::TypeDefinition;
use ion_rs::value::owned::{OwnedElement, OwnedStruct};
use ion_rs::value::{Element, Sequence, Struct, SymbolToken};
use ion_rs::IonType;
use std::convert::{TryFrom, TryInto};

/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone)]
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

/// Parse constraints inside an [OwnedStruct] to an [IslType]
impl TryFrom<&OwnedStruct> for IslType {
    type Error = IonSchemaError;

    fn try_from(ion_struct: &OwnedStruct) -> Result<Self, Self::Error> {
        let mut constraints = vec![];

        // parses the name of the type specified by schema
        let type_name: Option<String>=  match ion_struct.get("name") {
            Some(name_element) => match name_element.as_str() {
                Some(name) => Some(name.to_owned()),
                None => { return Err(invalid_schema_error_raw("A type name is not string/symbol, if the value is any null, or the text of the symbol is not defined.")) }
            },
            None => None // If the type is UNNAMED_TYPE_DEFINITION/ AnonymousType then set name as None
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
        Ok(IslType::new(type_name.to_owned(), constraints))
    }
}

/// Provides an internal representation of schema type reference.
/// Type reference grammar is defined in [Ion Schema Spec]
/// [Ion Schema spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
#[derive(Debug, Clone)]
pub enum IslTypeRef {
    /// represents core ion type reference
    IslCoreType(IonType),
    /// represents a type reference which represents a type imported from another schema
    AliasType(String),
    /// represents a type reference defined as an inlined import type from another schema
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
                    .and_then(|type_reference| {
                        let ion_type = match type_reference {
                            "int" => IslTypeRef::IslCoreType(IonType::Integer),
                            "float" => IslTypeRef::IslCoreType(IonType::Float),
                            "decimal" => IslTypeRef::IslCoreType(IonType::Decimal),
                            "timestamp" => IslTypeRef::IslCoreType(IonType::Timestamp),
                            "string" => IslTypeRef::IslCoreType(IonType::String),
                            "symbol" => IslTypeRef::IslCoreType(IonType::Symbol),
                            "bool" => IslTypeRef::IslCoreType(IonType::Boolean),
                            "blob" => IslTypeRef::IslCoreType(IonType::Blob),
                            "clob" => IslTypeRef::IslCoreType(IonType::Clob),
                            "sexp" => IslTypeRef::IslCoreType(IonType::SExpression),
                            "list" => IslTypeRef::IslCoreType(IonType::List),
                            "struct" => IslTypeRef::IslCoreType(IonType::Struct),
                            // TODO: add a match for other core types like: lob, text, number, document, any
                            _ => IslTypeRef::AliasType(type_reference.to_owned()),
                        };
                        Ok(ion_type)
                    })
            }
            IonType::Struct =>
                Ok(IslTypeRef::AnonymousType(value.as_struct().unwrap().try_into()?)),
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    /// Resolves a type_reference into a [TypeId] using the type_store
    pub fn resolve_type_reference(
        type_reference: &IslTypeRef,
        type_store: &SharedTypeStore,
    ) -> IonSchemaResult<TypeId> {
        match type_reference {
            IslTypeRef::IslCoreType(ion_type) => {
                // TODO: create CoreType struct for storing ISLCoreType type definition instead of Type
                // inserts ISLCoreType as a Type into type_store
                Ok(type_store.borrow_mut().add_named_type(
                    &format!("{:?}", ion_type),
                    TypeDefinition::new(Some(format!("{:?}", ion_type)), vec![]),
                ))
            }
            IslTypeRef::AliasType(alias) => {
                let borrow_type_store = type_store.borrow_mut();
                // verify if the AliasType actually exists in the type_store or throw an error
                match borrow_type_store.get_type_id_by_name(alias) {
                    Some(type_id) => Ok(type_id.to_owned()),
                    None => match borrow_type_store.get_parent() {
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
                let type_def = TypeDefinition::parse_from_isl_type_and_update_type_store(
                    isl_type, type_store,
                )?;
                Ok(type_store.borrow_mut().add_anonymous_type(type_def))
            } //TODO: add a check for ImportType type reference here
        }
    }
}
