use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslTypeImpl;
use crate::isl::IslVersion;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::TypeDefinitionImpl;
use ion_rs::value::owned::Element;
use ion_rs::value::reader::{element_reader, ElementReader};
use ion_rs::value::{IonElement, IonStruct};
use ion_rs::IonType;

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::{IslConstraint, IslConstraintImpl};
    use crate::isl::isl_type::IslTypeImpl;
    use crate::isl::isl_type_reference::{IslTypeRef, IslTypeRefImpl};

    /// Creates a [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(name.into()))
    }

    /// Creates a [IslTypeRef::Anonymous] using the [IonType] referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        let constraints = constraints.into();
        let isl_constraints: Vec<IslConstraintImpl> = constraints
            .iter()
            .map(|c| c.constraint.to_owned())
            .collect();
        IslTypeRef::new(IslTypeRefImpl::Anonymous(IslTypeImpl::new(
            None,
            isl_constraints,
            None,
        )))
    }
}

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 2.0
pub mod v_2_0 {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type_reference::{v_1_0, IslTypeRef};

    /// Creates a [IslTypeRef::Named] using the [IonType] referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        v_1_0::named_type_ref(name)
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IslTypeRefImpl {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType),
    /// represents an unnamed type definition reference
    Anonymous(IslTypeImpl),
}

impl IslTypeRefImpl {
    /// Provides a string representing a definition of a nullable built in type reference
    // these are the built types that rae annotated with `nullable`
    fn get_nullable_type_reference_definition(type_ref_name: &str) -> IonSchemaResult<&str> {
        Ok(match type_ref_name {
            "int" => "{ type: $any, any_of: [$null, $int] }",
            "float" => "{ type: $any, any_of: [$null, $float] }",
            "bool" => "{ type: $any, any_of: [$null, $bool] }",
            "decimal" => "{ type: $any, any_of: [$null, $decimal] }",
            "string" => "{ type: $any, any_of: [$null, $string] }",
            "symbol" => "{ type: $any, any_of: [$null, $symbol] }",
            "blob" => "{ type: $any, any_of: [$null, $blob] }",
            "clob" => "{ type: $any, any_of: [$null, $clob] }",
            "timestamp" => "{ type: $any, any_of: [$null, $timestamp] }",
            "struct" => "{ type: $any, any_of: [$null, $struct] }",
            "sexp" => "{ type: $any, any_of: [$null, $sexp] }",
            "list" => "{ type: $any, any_of: [$null, $list] }",
            "lob" => "{ type: $any, any_of: [$null, $lob] }",
            "number" => "{ type: $any, any_of: [$null, $number] }",
            "text" => "{ type: $any, any_of: [$null, $text] }",
            "nothing" => "{ type: $null }",
            "any" => "{ type: $any }",
            // TODO: currently it only allows for built in types to be defined with `nullable` annotation for all other type references it return an error
            _ => {
                return invalid_schema_error(
                    "nullable type reference is only allowed for built in types",
                )
            }
        })
    }

    /// Tries to create an [IslTypeRef] from the given Element
    pub fn from_ion_element(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        match value.ion_type() {
            IonType::Symbol => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.symbol",
                    )
                }

                let type_name = value.as_sym().unwrap()
                    .text()
                    .ok_or_else(|| {
                        invalid_schema_error_raw(
                            "a base or alias type reference symbol doesn't have text",
                        )
                    })?;

                // check for nullable type reference
                if value.annotations().any(|a| a.text() == Some("nullable")) {
                    let built_in_type_def = IslTypeRefImpl::get_nullable_type_reference_definition(type_name)?;
                    let value = &element_reader()
                        .read_one(built_in_type_def.as_bytes()).unwrap();
                    return IslTypeRefImpl::from_ion_element(isl_version, value, inline_imported_types);
                }

                Ok(IslTypeRefImpl::Named(type_name.to_owned()))
            }
            IonType::Struct => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.struct",
                    )
                }
                if value.annotations().any(|a| a.text() == Some("nullable")) {
                    return invalid_schema_error(
                        "nullable type reference is only allowed for built in types",
                    )
                }
                let value_struct = try_to!(value.as_struct());
                // if the struct doesn't have an id field then it must be an anonymous type
                if value_struct.get("id").is_none() {
                    return Ok(IslTypeRefImpl::Anonymous(IslTypeImpl::from_owned_element(isl_version, value, inline_imported_types)?))
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
                Ok(IslTypeRefImpl::TypeImport(isl_import_type))
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
    ) -> IonSchemaResult<TypeId> {
        match type_reference {
            IslTypeRefImpl::Named(alias) => {
                IslTypeRefImpl::get_type_id_from_type_name(alias, type_store, pending_types)
            }
            IslTypeRefImpl::Anonymous(isl_type) => {
                let type_id = pending_types.get_total_types(type_store);
                let type_def = TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                    isl_version,
                    isl_type,
                    type_store,
                    pending_types,
                )?;
                // get the last added anonymous type's type_id for given anonymous type
                Ok(type_id)
            }
            IslTypeRefImpl::TypeImport(isl_import_type) => {
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
