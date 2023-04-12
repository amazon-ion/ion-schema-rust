use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslTypeImpl;
use crate::isl::IslVersion;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::type_reference::TypeReference;
use crate::types::TypeDefinitionImpl;
use crate::IslConstraintImpl;
use ion_rs::element::Element;
use ion_rs::IonType;

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::{IslConstraint, IslConstraintImpl};
    use crate::isl::isl_type::IslTypeImpl;
    use crate::isl::isl_type_reference::{IslTypeRef, IslTypeRefImpl, IslTypeRefModifier};
    use ion_rs::IonType;

    /// Creates a [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(
            name.into(),
            IslTypeRefModifier::Nothing,
        ))
    }

    /// Creates a nullable [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn nullable_built_in_type_ref(name: IonType) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(
            format!("{name}"),
            IslTypeRefModifier::Nullable,
        ))
    }

    /// Creates a [IslTypeRef::Anonymous] using the [IonType] referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        let constraints = constraints.into();
        let isl_constraints: Vec<IslConstraintImpl> = constraints
            .iter()
            .map(|c| c.constraint.to_owned())
            .collect();
        IslTypeRef::new(IslTypeRefImpl::Anonymous(
            IslTypeImpl::new(None, isl_constraints, None),
            IslTypeRefModifier::Nothing,
        ))
    }
}

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 2.0
pub mod v_2_0 {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type_reference::{v_1_0, IslTypeRef, IslTypeRefImpl, IslTypeRefModifier};

    /// Creates a [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        v_1_0::named_type_ref(name)
    }

    /// Creates a nullable [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn null_or_named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(
            name.into(),
            IslTypeRefModifier::NullOr,
        ))
    }

    /// Creates a [IslTypeRef::Anonymous] using the [IonType] referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        v_1_0::anonymous_type_ref(constraints)
    }
}

/// Provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]
/// [Ion Schema spec]: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#grammar>`
#[derive(Debug, Clone, PartialEq)]
pub struct IslTypeRef {
    pub(crate) type_reference: IslTypeRefImpl,
}

impl IslTypeRef {
    pub(crate) fn new(type_reference: IslTypeRefImpl) -> Self {
        Self { type_reference }
    }
}

//TODO: can add occurs here to store occurs range information for variably occurring type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IslTypeRefModifier {
    Nullable,
    NullOr,
    Nothing, // Represents that no modifiers were provided with the type reference
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IslTypeRefImpl {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String, IslTypeRefModifier),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType, IslTypeRefModifier),
    /// represents an unnamed type definition reference
    Anonymous(IslTypeImpl, IslTypeRefModifier),
}

impl IslTypeRefImpl {
    /// Verifies if the given type reference contains an `occurs` field or not
    // This is used to make sure only `ordered_elements` and `fields` constraint can contain `occurs`.
    // This method returns `false` for `Named` or `TypeImport` type references because `occurs` field is not allowed within named type definitions.
    pub fn get_occurs_constraint(&self) -> bool {
        match self {
            IslTypeRefImpl::Anonymous(anonymous_type_def, modifier) => anonymous_type_def
                .constraints()
                .iter()
                .any(|c| matches!(c, IslConstraintImpl::Occurs(_))),
            _ => false,
        }
    }

    /// Tries to create an [IslTypeRef] from the given Element
    pub fn from_ion_element(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        use crate::isl::isl_type_reference::IslTypeRefModifier::*;
        match value.ion_type() {
            IonType::Symbol => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.symbol",
                    )
                }

                let type_name = value.as_symbol().unwrap()
                    .text()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "a base or alias type reference symbol doesn't have text",
                        )
                    })?;

                // check for nullable type reference
                if value.has_annotation("nullable") {
                    if isl_version == IslVersion::V2_0 {
                        return invalid_schema_error(
                            "`nullable::` modifier is not supported since ISL 2.0",
                        )
                    }

                    return match type_name {
                        // TODO: currently it only allows for built in types to be defined with `nullable` annotation for all other type references it return an error
                        "int" | "float" | "bool" | "decimal" | "string" | "symbol" | "blob" | "clob" | "timestamp" | "struct" | "sexp" | "list" | "text" | "lob" | "number" | "any" => {
                            Ok(IslTypeRefImpl::Named(type_name.to_owned(), Nullable))
                        }
                        _ => {
                            invalid_schema_error(
                                "`nullable::` modifier is only supported for built in types",
                            )
                        }
                    }
                }

                // check for `$null_or` type reference
                if value.has_annotation("$null_or") {
                    if isl_version == IslVersion::V1_0 {
                        return invalid_schema_error(
                            "`$null_or::` modifier is not supported before ISL 2.0",
                        )
                    }
                    return Ok(IslTypeRefImpl::Named(type_name.to_owned(), NullOr))
                }

                Ok(IslTypeRefImpl::Named(type_name.to_owned(), Nothing))
            }
            IonType::Struct => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.struct",
                    )
                }
                let mut isl_type_ref_modifier = Nothing;

                // check for nullable type reference
                if value.has_annotation("nullable") {
                    if isl_version == IslVersion::V2_0 {
                        return invalid_schema_error(
                            "`nullable::` modifier is not supported since ISL 2.0",
                        )
                    }
                    // TODO: currently it only allows for built in types to be defined with `nullable` annotation for all other type references it return an error
                    return invalid_schema_error(
                        "`nullable` modifier is only supported for built-in types",
                    )
                }

                // check for `$null_or` type reference
                if value.has_annotation("$null_or") {
                    if isl_version == IslVersion::V1_0 {
                        return invalid_schema_error(
                            "`$null_or::` modifier is not supported before ISL 2.0",
                        )
                    }
                    isl_type_ref_modifier = NullOr
                }

                let value_struct = try_to!(value.as_struct());
                // if the struct doesn't have an id field then it must be an anonymous type
                if value_struct.get("id").is_none() {
                    return Ok(IslTypeRefImpl::Anonymous(IslTypeImpl::from_owned_element(isl_version, value, inline_imported_types)?, isl_type_ref_modifier))
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
                Ok(IslTypeRefImpl::TypeImport(isl_import_type, isl_type_ref_modifier))
            },
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    /// Return TypeId for given built-in type name from type_store
    fn get_type_id_from_type_name(
        alias: &str,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<TypeId> {
        if let Some(type_id) = type_store.get_builtin_type_id(alias) {
            // verify if the given alias is a Built-in type and if it is then return the type id from type_store
            // All Built-in types are preloaded into the type_store
            // Built-in types includes all ion types and derived types like any, lob, text, number, $int, $float, ...
            return Ok(type_id);
        } else if let Some(type_id) = pending_types.get_type_id_by_name(alias, type_store) {
            // verify if the AliasType actually exists in the type_store or pending types
            return Ok(type_id);
        } else if let Some(parent) = pending_types.get_parent() {
            // if it is a self referencing type resolve it using parent information from type_store
            if parent.0.eq(alias) {
                return Ok(parent.1);
            }
        }
        // Otherwise if the type is not found from any of the above places
        // then the type definition might be defined below this type definition in the schema
        // hence add current type into the pending types and at the end when the pending types
        // are moved to type store, verify that the type definition exists somewhere in the schema following current type
        // This is basically deferring the type reference from here and later checking its existence
        Ok(pending_types.add_deferred_type_with_name(alias, type_store))
    }

    // TODO: break match arms into helper methods as we add more constraints
    /// Resolves a type_reference into a [TypeId] using the type_store
    pub fn resolve_type_reference(
        isl_version: IslVersion,
        type_reference: &IslTypeRefImpl,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<TypeReference> {
        match type_reference {
            IslTypeRefImpl::Named(alias, type_ref_modifier) => Ok(TypeReference::new(
                IslTypeRefImpl::get_type_id_from_type_name(alias, type_store, pending_types)?,
                type_ref_modifier.to_owned(),
            )),
            IslTypeRefImpl::Anonymous(isl_type, type_ref_modifier) => {
                let type_id = pending_types.get_total_types(type_store);
                let type_def = TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                    isl_version,
                    isl_type,
                    type_store,
                    pending_types,
                )?;
                // get the last added anonymous type's type_id for given anonymous type
                Ok(TypeReference::new(type_id, type_ref_modifier.to_owned()))
            }
            IslTypeRefImpl::TypeImport(isl_import_type, type_ref_modifier) => {
                // verify if the inline import type already exists in the type_store
                match type_store.get_type_id_by_name(isl_import_type.type_name()) {
                    None => unresolvable_schema_error(format!(
                        "inline import type: {} does not exists",
                        isl_import_type.type_name()
                    )),
                    Some(type_id) => Ok(TypeReference::new(*type_id, type_ref_modifier.to_owned())),
                }
            }
        }
    }
}
