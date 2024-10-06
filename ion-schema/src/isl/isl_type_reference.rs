use crate::ion_extension::ElementExtensions;
use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslType;
use crate::isl::ranges::{Limit, UsizeRange};
use crate::isl::IslVersion;
use crate::isl_require;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::type_reference::{TypeReference, VariablyOccurringTypeRef};
use crate::types::TypeDefinitionImpl;
use ion_rs::IonType;
use ion_rs::{Element, IonResult, StructWriter, Value, ValueWriter, WriteAsIon};

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type::IslType;
    use crate::isl::isl_type_reference::{
        IslTypeRef, IslVariablyOccurringTypeRef, NullabilityModifier,
    };
    use crate::isl::ranges::UsizeRange;
    use ion_rs::IonType;

    /// Creates a named [IslTypeRef] using the name of the type referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::Named(name.into(), NullabilityModifier::Nothing)
    }

    /// Creates a nullable [IslTypeRef] using the [IonType] referenced inside it
    pub fn nullable_built_in_type_ref(name: IonType) -> IslTypeRef {
        IslTypeRef::Named(format!("{name}"), NullabilityModifier::Nullable)
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        let constraints = constraints.into();
        IslTypeRef::Anonymous(
            IslType::new(None, constraints, None),
            NullabilityModifier::Nothing,
        )
    }

    /// Creates an [IslVariablyOccurringTypeRef] using the [IslConstraint]s and [Range] referenced inside it
    pub fn variably_occurring_type_ref(
        type_ref: IslTypeRef,
        occurs: UsizeRange,
    ) -> IslVariablyOccurringTypeRef {
        IslVariablyOccurringTypeRef::new(type_ref, occurs)
    }
}

/// Provides public facing APIs for constructing ISL type references programmatically for ISL 2.0
pub mod v_2_0 {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type_reference::{
        v_1_0, IslTypeRef, IslVariablyOccurringTypeRef, NullabilityModifier,
    };
    use crate::isl::ranges::UsizeRange;

    /// Creates a named [IslTypeRef] using the name of the type referenced inside it
    pub fn named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        v_1_0::named_type_ref(name)
    }

    /// Creates a nullable [IslTypeRef] using the name of the type referenced inside it
    pub fn null_or_named_type_ref<A: Into<String>>(name: A) -> IslTypeRef {
        IslTypeRef::Named(name.into(), NullabilityModifier::NullOr)
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s referenced inside it
    pub fn anonymous_type_ref<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslTypeRef {
        v_1_0::anonymous_type_ref(constraints)
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s and [Range] referenced inside it
    pub fn variably_occurring_type_ref(
        type_ref: IslTypeRef,
        occurs: UsizeRange,
    ) -> IslVariablyOccurringTypeRef {
        v_1_0::variably_occurring_type_ref(type_ref, occurs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum NullabilityModifier {
    Nullable,
    NullOr,
    Nothing, // Represents that no modifiers were provided with the type reference
}

impl NullabilityModifier {
    fn as_annotations(&self) -> &'static [&'static str] {
        match self {
            NullabilityModifier::Nullable => &["nullable"],
            NullabilityModifier::NullOr => &["$null_or"],
            NullabilityModifier::Nothing => &[],
        }
    }
}

/// Provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]
///
/// [Ion Schema spec]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#grammar
#[derive(Debug, Clone, PartialEq)]
pub enum IslTypeRef {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String, NullabilityModifier),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType, NullabilityModifier),
    /// represents an unnamed type definition reference
    Anonymous(IslType, NullabilityModifier),
}

impl IslTypeRef {
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
                            Ok(IslTypeRef::Named(type_name.to_owned(), Nullable))
                        }
                        _ => {
                            invalid_schema_error(
                                "`nullable::` modifier is only supported for built in types",
                            )
                        }
                    }
                }

                Ok(IslTypeRef::Named(type_name.to_owned(), nullability))
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
                    let type_def= IslType::from_owned_element(isl_version, value, inline_imported_types)?;
                    // if type reference contains `occurs` field and has modifier `$null_or` then return an error
                    if nullability == NullOr && value_struct.get("occurs").is_some() {
                        return invalid_schema_error(
                            "`$null_or` annotation is not supported for a type reference with an explicit `occurs` field",
                        )
                    }

                    if let Some(occurs_field) = value_struct.get("occurs") {
                        if !allow_occurs_field && isl_version == IslVersion::V2_0 {
                            return invalid_schema_error(
                            "A type reference with an explicit `occurs` field can only be used for `fields` and `ordered_elements` constraint",
                            )
                        } else {
                            // for ISL 1.0 `occurs` field is a no op when used with constraints other than `fields` and `ordered_elements`.
                            // Although ISL 1.0 will treat this `occurs` field as no op it still has to serialize the `occurs` range to see if its a valid range.
                            IslVariablyOccurringTypeRef::occurs_from_ion_element(occurs_field)?;
                        }
                    }

                    return Ok(IslTypeRef::Anonymous(type_def, nullability))
                }
                // if it is an inline import type store it as import type reference
                let isl_import_type = match IslImport::from_ion_element(value)? {
                    IslImport::Schema(_) => {
                        return invalid_schema_error("an inline import type reference does not have `type` field specified")
                    }
                    IslImport::Type(isl_import_type) => { isl_import_type }
                    IslImport::TypeAlias(_) if isl_version == IslVersion::V2_0 => {
                        return invalid_schema_error("an inline import type reference can not have `alias` field specified")
                    }
                    IslImport::TypeAlias(isl_import_type) => {
                        // consider the type alias as open content for ISL 1.0
                        IslImportType::new(isl_import_type.id().to_owned(), isl_import_type.type_name().to_owned(), None)
                    }
                };
                // if an inline import type is encountered add it in the inline_imports_types
                // this will help resolve these inline imports before we start loading the schema types that uses them as reference
                inline_imported_types.push(isl_import_type.to_owned());
                Ok(IslTypeRef::TypeImport(isl_import_type, nullability))
            },
            _ => Err(invalid_schema_error_raw(
                "type reference can either be a symbol(For base/alias type reference) or a struct (for anonymous type reference)",
            )),
        }
    }

    pub fn name(&self) -> String {
        match self {
            IslTypeRef::Named(name, _) => name.to_string(),
            IslTypeRef::TypeImport(import_type, _) => import_type.type_name().to_string(),
            IslTypeRef::Anonymous(anonymous_type, _) => "".to_string(),
        }
    }
    /// Tries to create an [IslTypeRef] from the given Element
    pub(crate) fn from_ion_element(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        IslTypeRef::from_ion_element_with_occurs_flag(
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
    pub(crate) fn resolve_type_reference(
        isl_version: IslVersion,
        type_reference: &IslTypeRef,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<TypeReference> {
        match type_reference {
            IslTypeRef::Named(alias, type_ref_modifier) => Ok(TypeReference::new(
                IslTypeRef::get_type_id_from_type_name(alias, type_store, pending_types)?,
                type_ref_modifier.to_owned(),
            )),
            IslTypeRef::Anonymous(isl_type, type_ref_modifier) => {
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
            IslTypeRef::TypeImport(isl_import_type, type_ref_modifier) => {
                // verify if the inline import type already exists in the type_store
                match type_store
                    .get_defined_type_id_or_imported_type_id_by_name(isl_import_type.type_name())
                {
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

impl WriteAsIon for IslTypeRef {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        match self {
            IslTypeRef::Named(name, nullability_modifier) => writer
                .with_annotations(nullability_modifier.as_annotations())?
                .write_symbol(name.as_str()),
            IslTypeRef::TypeImport(type_import, nullability_modifier) => writer
                .with_annotations(nullability_modifier.as_annotations())?
                .write(type_import),
            IslTypeRef::Anonymous(type_def, nullability_modifier) => writer
                .with_annotations(nullability_modifier.as_annotations())?
                .write(type_def),
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
    pub(crate) type_ref: IslTypeRef,
    occurs: UsizeRange,
}

impl IslVariablyOccurringTypeRef {
    pub(crate) fn new(type_ref: IslTypeRef, occurs: UsizeRange) -> Self {
        Self { type_ref, occurs }
    }

    pub fn type_reference(&self) -> &IslTypeRef {
        &self.type_ref
    }

    pub fn name(&self) -> String {
        self.type_ref.name()
    }

    pub fn optional(type_ref: IslTypeRef) -> Self {
        Self {
            type_ref,
            occurs: UsizeRange::zero_or_one(),
        }
    }

    pub fn required(type_ref: IslTypeRef) -> Self {
        Self {
            type_ref,
            occurs: UsizeRange::new_single_value(1),
        }
    }

    pub fn occurs(&self) -> UsizeRange {
        self.occurs.to_owned()
    }

    /// Tries to create an [IslVariablyOccurringTypeRef] from the given Element
    pub(crate) fn from_ion_element(
        constraint_name: &str,
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Self> {
        let type_ref = IslTypeRef::from_ion_element_with_occurs_flag(
            isl_version,
            value,
            inline_imported_types,
            true,
        )?;

        let occurs: UsizeRange = value
            .as_struct()
            .and_then(|s| {
                s.get("occurs")
                    .map(IslVariablyOccurringTypeRef::occurs_from_ion_element)
            })
            .unwrap_or(if constraint_name == "fields" {
                Ok(UsizeRange::zero_or_one())
            } else {
                Ok(UsizeRange::new_single_value(1))
            })?;

        isl_require!(occurs.upper() != &Limit::Inclusive(0usize) => "Occurs cannot be 0")?;
        Ok(IslVariablyOccurringTypeRef { type_ref, occurs })
    }

    fn occurs_from_ion_element(value: &Element) -> IonSchemaResult<UsizeRange> {
        if value.is_null() {
            return invalid_schema_error(
                "expected an integer or integer range for an `occurs` constraint, found null",
            );
        }
        match value.value() {
            Value::Symbol(s) if s.text() == Some("optional") => Ok(UsizeRange::zero_or_one()),
            Value::Symbol(s) if s.text() == Some("required") => Ok(UsizeRange::new_single_value(1)),
            Value::List(_) | Value::Int(_) => {
                UsizeRange::from_ion_element(value, Element::as_usize)
            }
            _ => invalid_schema_error(format!("Invalid occurs value: {value}")),
        }
    }

    /// Resolves an [IslVariablyOccurringTypeRef] into a [VariablyOccurringTypeRef] using the type_store
    pub(crate) fn resolve_type_reference(
        &self,
        isl_version: IslVersion,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<VariablyOccurringTypeRef> {
        let type_ref = IslTypeRef::resolve_type_reference(
            isl_version,
            &self.type_ref,
            type_store,
            pending_types,
        )?;

        Ok(VariablyOccurringTypeRef::new(type_ref, self.occurs()))
    }
}

impl WriteAsIon for IslVariablyOccurringTypeRef {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        match &self.type_ref {
            IslTypeRef::Named(name, nullability_modifier) => writer
                .with_annotations(nullability_modifier.as_annotations())?
                .write_symbol(name.as_str()),
            IslTypeRef::TypeImport(type_import, nullability_modifier) => writer
                .with_annotations(nullability_modifier.as_annotations())?
                .write(type_import),
            IslTypeRef::Anonymous(type_def, nullability_modifier) => {
                let mut struct_writer = writer
                    .with_annotations(nullability_modifier.as_annotations())?
                    .struct_writer()?;

                for constraint in type_def.constraints() {
                    let constraint_value = constraint.constraint();
                    struct_writer.write(constraint_value.field_name(), constraint_value)?;
                }
                struct_writer.write("occurs", &self.occurs)?;
                struct_writer.close()
            }
        }
    }
}
