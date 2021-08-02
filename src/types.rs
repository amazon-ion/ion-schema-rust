use crate::constraint::{AllOf, Constraint, TypeConstraint};
use crate::result::{invalid_schema_error_raw, IonSchemaError};
use crate::violation::Violations;
use ion_rs::value::owned::{OwnedElement, OwnedStruct};
use ion_rs::value::{Element, Struct, SymbolToken};
use ion_rs::IonType;
use std::convert::{TryFrom, TryInto};

/// Provides validation for Type
pub trait TypeValidator {
    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: &OwnedElement) -> bool;

    /// Returns a Violations object indicating whether the specified value
    /// is valid for this type, and if not, provides details as to which
    /// constraints were violated.
    fn validate(&self, value: &OwnedElement, issues: &mut Violations);
}

/// A Type consists of an optional name and zero or more constraints.
///
/// Unless otherwise specified, the constraint `type: any` is automatically applied.
#[derive(Debug, Clone)]
pub struct Type {
    name: String,
    constraints: Vec<Constraint>,
    deferred_type_references: Vec<TypeRef>,
}

impl Type {
    pub fn new(
        name: String,
        constraints: Vec<Constraint>,
        deferred_type_references: Vec<TypeRef>,
    ) -> Self {
        Self {
            name,
            constraints,
            deferred_type_references,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Returns type references that are not yet resolved (alias type reference or anonymous type reference)
    pub fn deferred_type_references(&self) -> &[TypeRef] {
        &self.deferred_type_references
    }
}

impl TypeValidator for Type {
    fn is_valid(&self, value: &OwnedElement) -> bool {
        todo!()
    }

    fn validate(&self, value: &OwnedElement, issues: &mut Violations) {
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
            None => format!("{:?}", ion_struct) // If the type is UNNAMED_TYPE_DEFINITION/ AnonymousType then add the entire struct as the name of type
        };

        let mut deferred_type_references: Vec<TypeRef> = vec![];
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
                    let all_of: AllOf = value.try_into()?;
                    deferred_type_references
                        .append(all_of.deferred_type_references().to_vec().as_mut());
                    Constraint::AllOf(all_of)
                }
                "type" => {
                    let type_constraint: TypeConstraint = value.try_into()?;
                    if type_constraint.deferred_type_reference().is_some() {
                        deferred_type_references.push(
                            type_constraint
                                .deferred_type_reference()
                                .unwrap()
                                .to_owned(),
                        );
                    }
                    Constraint::TypeConstraint(type_constraint)
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
        Ok(Type::new(type_name, constraints, deferred_type_references))
    }
}

/// Provides an internal representation of schema type reference.
/// Type reference grammar is defined in [Ion Schema Spec]
/// [Ion Schema spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
#[derive(Debug, Clone)]
pub enum TypeRef {
    /// represents core ion type reference
    BaseType(IonType),
    /// represents a type reference which represents a type imported from another schema
    AliasType(String),
    /// represents a type reference defined as an inlined import type from another schema
    // TODO: add ImportType(Import) where ImportType could either point to a schema represented by an id with all the types or a single type from inside it
    /// represents an unnamed type definition reference
    AnonymousType(Type),
}

/// Tries to create a schema type reference from the given OwnedElement
// TODO: add a check for nullable type reference
impl TryFrom<&OwnedElement> for TypeRef {
    type Error = IonSchemaError;

    fn try_from(value: &OwnedElement) -> Result<Self, Self::Error> {
        match value.ion_type() {
            IonType::Symbol => {
                value.as_sym()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "a base or alias type reference must be a symbol",
                        )
                    })?
                    .text()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "a base or alias type reference symbol doesn't have text",
                        )
                    })
                    .and_then(|type_reference| {
                        let ion_type = match type_reference {
                            "null" => TypeRef::BaseType(IonType::Null),
                            "int" => TypeRef::BaseType(IonType::Integer),
                            "float" => TypeRef::BaseType(IonType::Float),
                            "decimal" => TypeRef::BaseType(IonType::Decimal),
                            "timestamp" => TypeRef::BaseType(IonType::Timestamp),
                            "string" => TypeRef::BaseType(IonType::String),
                            "symbol" => TypeRef::BaseType(IonType::Symbol),
                            "bool" => TypeRef::BaseType(IonType::Boolean),
                            "blob" => TypeRef::BaseType(IonType::Blob),
                            "clob" => TypeRef::BaseType(IonType::Clob),
                            "sexp" => TypeRef::BaseType(IonType::SExpression),
                            "list" => TypeRef::BaseType(IonType::List),
                            "struct" => TypeRef::BaseType(IonType::Struct),
                            // TODO: add a match for other core types like: lob, text, number, document, any
                            _ => TypeRef::AliasType(type_reference.to_owned()),
                        };
                        Ok(ion_type)
                    })
            }
            IonType::Struct => value
                .as_struct()
                .ok_or_else(|| {
                    invalid_schema_error_raw("anonymous type reference must be a struct")
                })
                .and_then(|type_reference| Ok(TypeRef::AnonymousType(type_reference.try_into()?))),
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }
}
