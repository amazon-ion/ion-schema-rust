use crate::isl::isl_type::IslType;
use crate::result::{invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::TypeDefinition;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, SymbolToken};
use ion_rs::IonType;
use std::convert::TryInto;

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
