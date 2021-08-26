use crate::constraint::{AllOfConstraint, Constraint, TypeConstraint};
use crate::result::{invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult};
use crate::system::SharedTypeCache;
use crate::violation::Violations;
use ion_rs::value::owned::{OwnedElement, OwnedStruct};
use ion_rs::value::{Element, Struct, SymbolToken};
use ion_rs::IonType;

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
}

impl Type {
    pub fn new(name: String, constraints: Vec<Constraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedStruct] to a schema [Type]
    pub fn parse_from_ion_element(
        ion_struct: &OwnedStruct,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses the name of the type specified by schema
        let type_name=  match ion_struct.get("name") {
            Some(name_element) => match name_element.as_str() {
                Some(name) => name.to_owned(),
                None => { return Err(invalid_schema_error_raw("A type name is not string/symbol, if the value is any null, or the text of the symbol is not defined.")) }
            },
            None => format!("{:?}", ion_struct) // If the type is UNNAMED_TYPE_DEFINITION/ AnonymousType then add the entire struct as the name of type
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
                    let all_of: AllOfConstraint =
                        AllOfConstraint::parse_from_ion_element(value, type_cache)?;
                    Constraint::AllOf(all_of)
                }
                "type" => {
                    let type_constraint: TypeConstraint =
                        TypeConstraint::parse_from_ion_element(value, type_cache)?;
                    Constraint::Type(type_constraint)
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
        let type_def = Type::new(type_name.to_owned(), constraints);
        type_cache
            .borrow_mut()
            .insert(type_name, type_def.to_owned());
        Ok(type_def.to_owned())
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

/// Provides an internal representation of schema type reference.
/// Type reference grammar is defined in [Ion Schema Spec]
/// [Ion Schema spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
#[derive(Debug, Clone)]
pub enum TypeRef {
    /// represents core ion type reference
    IslCoreType(IonType),
    /// represents a type reference which represents a type imported from another schema
    AliasType(String),
    /// represents a type reference defined as an inlined import type from another schema
    // TODO: add ImportType(Import) where ImportType could either point to a schema represented by an id with all the types or a single type from inside it
    /// represents an unnamed type definition reference
    AnonymousType(Type),
}

// TODO: add a check for nullable type reference
impl TypeRef {
    /// Tries to create a schema type reference from the given OwnedElement
    pub fn parse_from_ion_element(
        value: &OwnedElement,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Self> {
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
                            "int" => TypeRef::IslCoreType(IonType::Integer),
                            "float" => TypeRef::IslCoreType(IonType::Float),
                            "decimal" => TypeRef::IslCoreType(IonType::Decimal),
                            "timestamp" => TypeRef::IslCoreType(IonType::Timestamp),
                            "string" => TypeRef::IslCoreType(IonType::String),
                            "symbol" => TypeRef::IslCoreType(IonType::Symbol),
                            "bool" => TypeRef::IslCoreType(IonType::Boolean),
                            "blob" => TypeRef::IslCoreType(IonType::Blob),
                            "clob" => TypeRef::IslCoreType(IonType::Clob),
                            "sexp" => TypeRef::IslCoreType(IonType::SExpression),
                            "list" => TypeRef::IslCoreType(IonType::List),
                            "struct" => TypeRef::IslCoreType(IonType::Struct),
                            // TODO: add a match for other core types like: lob, text, number, document, any
                            _ => TypeRef::AliasType(type_reference.to_owned()),
                        };
                        Ok(ion_type)
                    })
            }
            IonType::Struct =>
                Ok(TypeRef::AnonymousType(Type::parse_from_ion_element(value
                                           .as_struct()
                                           .unwrap(), type_cache)?)),
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    /// Resolves a type_reference into a [Type] that can be using the type_cache
    pub fn resolve_type_reference(
        type_reference: &TypeRef,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Type> {
        match type_reference {
            TypeRef::IslCoreType(ion_type) => {
                // TODO: create CoreType struct for storing ISLCoreType type definition instead of Type
                // inserts ISLCoreType as a Type into type_cache
                type_cache.borrow_mut().insert(
                    format!("{:?}", ion_type),
                    Type::new(format!("{:?}", ion_type), vec![]),
                );
                Ok(type_cache
                    .borrow_mut()
                    .get(&format!("{:?}", ion_type))
                    .unwrap()
                    .to_owned())
            }
            TypeRef::AliasType(alias) => {
                // verify if the AliasType actually exists in the type_cache or throw an error
                match type_cache.borrow_mut().get(alias) {
                    Some(type_def) => Ok(type_def.to_owned()),
                    None => unresolvable_schema_error(format!(
                        "Could not resolve type reference: {:?} does not exist",
                        alias
                    )),
                }
            }
            TypeRef::AnonymousType(type_def) => Ok(type_def.to_owned()),
            //TODO: add a check for ImportType type reference here
        }
    }
}
