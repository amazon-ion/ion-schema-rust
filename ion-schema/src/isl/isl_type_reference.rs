use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_range::Range;
use crate::isl::isl_type::IslTypeImpl;
use crate::isl::IslVersion;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::type_reference::{TypeReference, VariablyOccurringTypeRef};
use crate::types::TypeDefinitionImpl;
use crate::IslConstraintImpl;
use ion_rs::element::Element;
use ion_rs::IonType;

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::{IslConstraint, IslConstraintImpl};
    use crate::isl::isl_range::Range;
    use crate::isl::isl_type::IslTypeImpl;
    use crate::isl::isl_type_reference::{
        IslTypeRef, IslTypeRefImpl, IslVariablyOccurringTypeRef, NullabilityModifier,
    };
    use ion_rs::IonType;

    /// Creates a named [IslTypeRef] using the name of the type referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(
            name.into(),
            NullabilityModifier::Nothing,
        ))
    }

    /// Creates a nullable [IslTypeRef] using the [IonType] referenced inside it
    pub fn nullable_built_in_type_ref(name: IonType) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(
            format!("{name}"),
            NullabilityModifier::Nullable,
        ))
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        let constraints = constraints.into();
        let isl_constraints: Vec<IslConstraintImpl> = constraints
            .iter()
            .map(|c| c.constraint.to_owned())
            .collect();
        IslTypeRef::new(IslTypeRefImpl::Anonymous(
            IslTypeImpl::new(None, isl_constraints, None),
            NullabilityModifier::Nothing,
        ))
    }

    /// Creates an [IslVariablyOccurringTypeRef] using the [IslConstraint]s and [Range] referenced inside it
    pub fn variably_occurring_type_ref(
        type_ref: IslTypeRef,
        occurs: Range,
    ) -> IslVariablyOccurringTypeRef {
        IslVariablyOccurringTypeRef::new(type_ref, occurs)
    }
}

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 2.0
pub mod v_2_0 {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_range::Range;
    use crate::isl::isl_type_reference::{
        v_1_0, IslTypeRef, IslTypeRefImpl, IslVariablyOccurringTypeRef, NullabilityModifier,
    };

    /// Creates a named [IslTypeRef] using the name of the type referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        v_1_0::named_type_ref(name)
    }

    /// Creates a nullable [IslTypeRef] using the name of the type referenced inside it
    pub fn null_or_named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::new(IslTypeRefImpl::Named(
            name.into(),
            NullabilityModifier::NullOr,
        ))
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        v_1_0::anonymous_type_ref(constraints)
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s and [Range] referenced inside it
    pub fn variably_occurring_type_ref(
        type_ref: IslTypeRef,
        occurs: Range,
    ) -> IslVariablyOccurringTypeRef {
        v_1_0::variably_occurring_type_ref(type_ref, occurs)
    }
}

/// Provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]
///
/// [Ion Schema spec]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#grammar
#[derive(Debug, Clone, PartialEq)]
pub struct IslTypeRef {
    pub(crate) type_reference: IslTypeRefImpl,
}

impl IslTypeRef {
    pub(crate) fn new(type_reference: IslTypeRefImpl) -> Self {
        Self { type_reference }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum NullabilityModifier {
    Nullable,
    NullOr,
    Nothing, // Represents that no modifiers were provided with the type reference
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IslTypeRefImpl {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String, NullabilityModifier),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType, NullabilityModifier),
    /// represents an unnamed type definition reference
    Anonymous(IslTypeImpl, NullabilityModifier),
}

impl IslTypeRefImpl {
    fn from_ion_element_with_occurs_flag(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
        allow_occurs_field: bool,
    ) -> IonSchemaResult<Self> {
        use crate::isl::isl_type_reference::NullabilityModifier::*;
        let nullability = if value.annotations().contains("nullable") {
            if isl_version == IslVersion::V1_0 {
                Nullable
            } else {
                return invalid_schema_error(
                    "`nullable::` modifier is not supported since ISL 2.0",
                );
            }
        } else if value.annotations().contains("$null_or") {
            if isl_version != IslVersion::V1_0 {
                NullOr
            } else {
                return invalid_schema_error(
                    "`$null_or::` modifier is not supported before ISL 2.0",
                );
            }
        } else {
            Nothing
        };
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
                if nullability == Nullable {
                    return match type_name {
                        // TODO: currently it only allows for built in types (other than `document`) to be defined with `nullable` annotation. For `document` and all other type references it returns an error.
                        "int" | "float" | "bool" | "decimal" | "string" | "symbol" | "blob" | "clob" | "timestamp" | "struct" | "sexp" | "list" | "text" | "lob" | "number" | "any" | "nothing" |
                        "$null" | "$int" | "$float" | "$bool" | "$decimal" | "$string" | "$symbol" | "$blob" | "$clob" | "$timestamp" | "$struct" | "$sexp" | "$list" | "$text" | "$lob" | "$number" | "$any" => {
                            Ok(IslTypeRefImpl::Named(type_name.to_owned(), Nullable))
                        }
                        _ => {
                            invalid_schema_error(
                                "`nullable::` modifier is only supported for built in types",
                            )
                        }
                    }
                }

                Ok(IslTypeRefImpl::Named(type_name.to_owned(), nullability))
            }
            IonType::Struct => {
                if value.is_null() {
                    return invalid_schema_error(
                        "a base or alias type reference can not be null.struct",
                    )
                }

                // check for nullable type reference
                if nullability == Nullable {
                    // TODO: currently it only allows for built in types (other than `document`) to be defined with `nullable` annotation. For `document` and all other type references it returns an error.
                    return invalid_schema_error(
                        "`nullable` modifier is only supported for built-in types",
                    )
                }

                let value_struct = try_to!(value.as_struct());
                // if the struct doesn't have an id field then it must be an anonymous type
                if value_struct.get("id").is_none() {
                    let type_def= IslTypeImpl::from_owned_element(isl_version, value, inline_imported_types)?;
                    // if type reference contains `occurs` field and has modifier `$null_or` then return an error
                    if nullability == NullOr && type_def.constraints()
                        .iter()
                        .any(|c| matches!(c, IslConstraintImpl::Occurs(_))) {
                        return invalid_schema_error(
                            "`$null_or` annotation is not supported for a type reference with an explicit `occurs` field",
                        )
                    }

                    if !allow_occurs_field && isl_version == IslVersion::V2_0 && type_def.constraints()
                        .iter()
                        .any(|c| matches!(c, IslConstraintImpl::Occurs(_))) {
                        return invalid_schema_error(
                            "A type reference with an explicit `occurs` field can only be used for `fields` and `ordered_elements` constraint",
                        )
                    }

                    return Ok(IslTypeRefImpl::Anonymous(type_def, nullability))
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
                Ok(IslTypeRefImpl::TypeImport(isl_import_type, nullability))
            },
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    /// Tries to create an [IslTypeRef] from the given Element
    pub fn from_ion_element(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        IslTypeRefImpl::from_ion_element_with_occurs_flag(
            isl_version,
            value,
            inline_imported_types,
            false,
        )
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

/// Represents a [variably occurring type reference] that will be used by `ordered_elements` and `fields` constraints.
///  
/// ```ion
/// Grammar: <VARIABLY_OCCURRING_TYPE_ARGUMENT> ::= { <OCCURS>, <CONSTRAINT>... }
///                                      | <TYPE_ARGUMENT>
///
///         <OCCURS> ::= occurs: <INT>
///            | occurs: <RANGE_INT>
///            | occurs: optional
///            | occurs: required
/// ```
///
/// [variably occurring type reference]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#variably-occurring-type-arguments
#[derive(Debug, Clone, PartialEq)]
pub struct IslVariablyOccurringTypeRef {
    type_ref: IslTypeRefImpl,
    occurs: Range,
}

impl IslVariablyOccurringTypeRef {
    pub(crate) fn new(type_ref: IslTypeRef, occurs: Range) -> Self {
        Self {
            type_ref: type_ref.type_reference,
            occurs,
        }
    }

    pub fn optional(type_ref: IslTypeRef) -> Self {
        Self {
            type_ref: type_ref.type_reference,
            occurs: Range::optional(),
        }
    }

    pub fn required(type_ref: IslTypeRef) -> Self {
        Self {
            type_ref: type_ref.type_reference,
            occurs: Range::required(),
        }
    }

    pub fn occurs(&self) -> Range {
        self.occurs.to_owned()
    }

    /// Tries to create an [IslVariablyOccurringTypeRef] from the given Element
    pub fn from_ion_element(
        constraint_name: &str,
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        let type_ref = IslTypeRefImpl::from_ion_element_with_occurs_flag(
            isl_version,
            value,
            inline_imported_types,
            true,
        )?;

        let occurs: Range = if let IslTypeRefImpl::Anonymous(isl_type, _) = &type_ref {
            isl_type
                .constraints()
                .iter()
                .find_map(|c| match c {
                    IslConstraintImpl::Occurs(occurs) => Some(occurs.to_owned()),
                    _ => None,
                })
                .unwrap_or(if constraint_name == "fields" {
                    Range::optional()
                } else {
                    Range::required()
                })
        } else {
            if constraint_name == "fields" {
                Range::optional()
            } else {
                Range::required()
            }
        };

        Ok(IslVariablyOccurringTypeRef { type_ref, occurs })
    }

    /// Resolves an [IslVariablyOccurringTypeRef] into a [VariablyOccurringTypeRef] using the type_store
    pub fn resolve_type_reference(
        &self,
        isl_version: IslVersion,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<VariablyOccurringTypeRef> {
        let type_ref = IslTypeRefImpl::resolve_type_reference(
            isl_version,
            &self.type_ref,
            type_store,
            pending_types,
        )?;

        Ok(VariablyOccurringTypeRef::new(type_ref, self.occurs()))
    }
}
