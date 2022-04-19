use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslTypeImpl;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::TypeDefinitionImpl;
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Struct, SymbolToken};
use ion_rs::IonType;

/// Provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]
/// [Ion Schema spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
#[derive(Debug, Clone, PartialEq)]
pub enum IslTypeRef {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType),
    /// represents an unnamed type definition reference
    Anonymous(IslTypeImpl),
}

// TODO: add a check for nullable type reference
impl IslTypeRef {
    /// Creates a [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn named<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::Named(name.into())
    }

    /// Creates a [IslTypeRef::Anonymous] using the [IonType] referenced inside it
    pub fn anonymous<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        IslTypeRef::Anonymous(IslTypeImpl::new(None, constraints.into()))
    }

    /// Tries to create an [IslTypeRef] from the given OwnedElement
    pub fn from_ion_element(
        value: &OwnedElement,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        match value.ion_type() {
            IonType::Symbol => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.symbol",
                    )
                }
                value.as_sym().unwrap()
                    .text()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "a base or alias type reference symbol doesn't have text",
                        )
                    }).map(|type_name| IslTypeRef::Named(type_name.to_owned()))
            }
            IonType::Struct => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.struct",
                    )
                }
                let value_struct = try_to!(value.as_struct());
                // if the struct doesn't have an id field then it must be an anonymous type
                if value_struct.get("id").is_none() {
                    return Ok(IslTypeRef::Anonymous(IslTypeImpl::from_owned_element(value, inline_imported_types)?))
                }
                // if it is an inline import type store it as import type reference
                 let isl_import_type = match IslImport::from_ion_element(value)? {
                    IslImport::Schema(_) => {
                         return invalid_schema_error("an inline import type reference does not have `type` field specified")
                    }
                    IslImport::Type(isl_import_type) => { isl_import_type }
                    IslImport::TypeAlias(_) => {
                         return invalid_schema_error("an inline import type reference can not have `alias` field specified")
                    }
                };
                // if an inline import type is encountered add it in the inline_imports_types
                // this will help resolve these inline imports before we start loading the schema types that uses them as reference
                inline_imported_types.push(isl_import_type.to_owned());
                Ok(IslTypeRef::TypeImport(isl_import_type))
            },
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    /// Return TypeId for given built-in type name from type_store
    fn get_type_id_from_built_in_type_name(
        alias: &str,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<TypeId> {
        if let Some(type_id) = type_store.get_builtin_type_id(alias) {
            // verify if the given alias is a Built-in type and if it is then return the type id from type_store
            // All Built-in types are preloaded into the type_store
            // Built-in types includes all ion types and derived types like any, lob, text, number, $int, $float, ...
            Ok(type_id)
        } else if let Some(type_id) = pending_types.get_type_id_by_name(alias, type_store) {
            // verify if the AliasType actually exists in the type_store or throw an error
            Ok(type_id)
        } else if let Some(parent) = pending_types.get_parent() {
            // if it is a self referencing type resolve it using parent information from type_store
            if parent.0.eq(alias) {
                Ok(parent.1)
            } else {
                unresolvable_schema_error(format!(
                    "Could not resolve type reference: {:?} does not exist",
                    alias
                ))
            }
        } else {
            unresolvable_schema_error(format!(
                "Could not resolve type reference: {:?} does not exist",
                alias
            ))
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
            IslTypeRef::Named(alias) => {
                IslTypeRef::get_type_id_from_built_in_type_name(alias, type_store, pending_types)
            }
            IslTypeRef::Anonymous(isl_type) => {
                let type_id = pending_types.get_total_types(type_store);
                let type_def = TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                    isl_type,
                    type_store,
                    pending_types,
                )?;
                // get the last added anonymous type's type_id for given anonymous type
                Ok(type_id)
            }
            IslTypeRef::TypeImport(isl_import_type) => {
                // verify if the inline import type already exists in the type_store
                match type_store.get_type_id_by_name(isl_import_type.type_name()) {
                    None => unresolvable_schema_error(format!(
                        "inline import type: {} does not exists",
                        isl_import_type.type_name()
                    )),
                    Some(type_id) => Ok(*type_id),
                }
            }
        }
    }
}
