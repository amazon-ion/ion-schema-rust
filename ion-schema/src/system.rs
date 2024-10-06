//! Provides functions for getting instances of [`Schema`] using [`SchemaSystem`].
//!
//! ## Example:
//! In general, users will create one or more authorities and use them to build the [`SchemaSystem`].
//! Then this [`SchemaSystem`] is used to create instances of [`Schema`].
//!
//! ```
//! use ion_schema::authority::{MapDocumentAuthority, FileSystemDocumentAuthority, DocumentAuthority};
//! use ion_schema::system::SchemaSystem;
//! use std::path::Path;
//!
//! // create a vector of authorities and construct schema system
//! let authorities: Vec<Box<dyn DocumentAuthority>> = vec![Box::new(
//!             FileSystemDocumentAuthority::new(Path::new("schemas")),
//!         )];
//! let mut schema_system = SchemaSystem::new(authorities);
//!
//! // use this schema_system to load a schema as following
//! let schema = schema_system.load_schema("sample.isl");
//! ```

use crate::authority::DocumentAuthority;
use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslType;
use crate::isl::{IslSchema, IslVersion};
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error,
    unresolvable_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use crate::schema::Schema;
use crate::types::{BuiltInTypeDefinition, Nullability, TypeDefinitionImpl, TypeDefinitionKind};
use crate::{is_isl_version_marker, is_reserved_word, UserReservedFields};
use ion_rs::IonType;
use ion_rs::{Annotations, Element};
use std::collections::{HashMap, HashSet};
use std::io::ErrorKind;
use std::sync::Arc;

// TODO: Shift PendingTypes and TypeStore implementations to a separate module
/// Stores information about types that are in the process of being defined.
///
/// An ISL type definition can include types that are not yet fully defined.
/// For example, an ISL type definition might include:
/// * A reference to itself. This could happen in a recursive structure like a
///   linked list or binary tree.
/// * A nested anonymous type definition.
/// * A reference to type definition that is followed by current type definition. These type references
///   will be deferred to later check if a type definition with that name exists in the schema.
/// Because the [`SchemaSystem`] does not yet know the complete definition
/// of these types, it cannot find them in the [`TypeStore`].
/// An instance of [`PendingTypes`] is used to track information about types
/// that we do not have a complete definition for yet. When the
/// [`SchemaSystem`] finishes loading these types, the type definitions in
/// [`PendingTypes`] can be promoted the [`TypeStore`].
///
/// Deferred type definition:
/// A deferred type definition is added to [`PendingTypes`] whenever encountered type reference whose definition
/// is outside the scope of current type definition that is being resolved.
/// e.g.
/// type:: {
///     name: foo,
///     type: bar,
/// }
/// type:: {
///     name: bar,
///     type: int
/// }
///
/// For above example, `bar` will be saved as deferred type definition until we resolve the definition of `bar`.
/// When the [`SchemaSystem`] finishes loading the type `foo`, the type definitions in
/// [`PendingTypes`] including deferred type definitions will be promoted to the [`TypeStore`].
/// Once we resolve type definition for `bar` it will be updated in the [`TypeStore`].
#[derive(Debug, Clone, Default)]
pub struct PendingTypes {
    builtin_type_ids_by_name: HashMap<String, TypeId>,
    ids_by_name: HashMap<String, TypeId>,
    parent: Option<(String, TypeId)>,
    types_by_id: Vec<Option<TypeDefinitionKind>>, // a None in this vector represents a not-yet-resolved type
}

impl PendingTypes {
    /// Adds all the types from PendingTypes into given [`TypeStore`] including adding all the imported types into imports of [`TypeStore`].
    /// It also clears [`PendingTypes`] types for loading next set of types.
    /// This method is used after a schema named type/root type is loaded entirely into [`PendingTypes`]
    /// * `type_store` - The TypeStore which will be updated with the types within this PendingType
    /// * `load_isl_import` - If this argument is Some(isl_import), then we are not within an import process of schema.
    ///                       Based on given enum variant isl_import we will add the types to type_store.
    ///                       Otherwise we will add all the types from this PendingTypes to TypeStore.
    /// * `isl_type_names` - The isl type names defined within the schema. This will be used to determine
    ///                      if a type definition actually exists within the schema. If a type definition from this list
    ///                      exists in [`PendingTypes`] it would have been added as a deferred type definition.
    ///                      This deferred type will be loaded into [`TypeStore`] as it is and will be replaced with a type definition
    ///                      once it is resolved.
    /// Returns true, if this update is not for an isl import type or it is for an isl import type but it is added to the type_store
    /// Otherwise, returns false if this update is for an isl import type and it is not yet added to the type_store.
    pub fn update_type_store(
        &mut self,
        type_store: &mut TypeStore,
        load_isl_import: Option<&IslImport>,
        isl_type_names: &HashSet<&str>,
    ) -> IonSchemaResult<bool> {
        // if load_isl_import is not None, then match the enum variant and update type store with import types accordingly
        if let Some(import) = load_isl_import {
            match import {
                IslImport::Schema(_) => {
                    self.update_type_store_with_all_isl_imported_types(
                        None,
                        type_store,
                        isl_type_names,
                    )?;
                }
                IslImport::Type(isl_import) => {
                    // if import has a specified type to import then only add that type
                    if let Some(named_type_def) =
                        self.get_type_by_name_for_import(isl_import.type_name(), type_store)
                    {
                        type_store
                            .add_isl_imported_type(isl_import.alias().as_ref(), named_type_def?);
                        self.update_type_store_with_all_isl_imported_types(
                            Some(isl_import.type_name()),
                            type_store,
                            isl_type_names,
                        )?;
                    } else {
                        // if the named_type_def appears as None then it means we haven't reached
                        // the named type to be imported yet hence we return with false pointing
                        // we haven't yet resolved this import.
                        return Ok(false);
                    }
                }
                IslImport::TypeAlias(isl_import) => {
                    // if import has a specified type with an alias then renamed that type to the given alias and add it
                    if type_store
                        .imported_type_ids_by_name
                        .contains_key(isl_import.alias().as_ref().unwrap())
                    {
                        // if the type_store already has the import in it then return true (i.e. TypeAlias has already been imported)
                        return Ok(true);
                    } else if let Some(named_type_def) =
                        self.get_type_by_name_for_import(isl_import.type_name(), type_store)
                    {
                        let aliased_type_def = named_type_def?
                            .with_name(isl_import.alias().as_ref().unwrap().to_owned());
                        type_store
                            .add_isl_imported_type(isl_import.alias().as_ref(), aliased_type_def);
                        self.update_type_store_with_all_isl_imported_types(
                            Some(isl_import.type_name()),
                            type_store,
                            isl_type_names,
                        )?;
                    } else {
                        // if the named_type_def appears as None then it means we haven't reached
                        // the named type to be imported yet hence we return with false pointing
                        // we haven't yet resolved this import.
                        return Ok(false);
                    }
                }
            }
        } else {
            // if load_isl_import is None i.e. it is the root schema, then update type_store with all the types inside this PendingTypes
            self.update_type_store_with_all_types(type_store, isl_type_names)?;
        }
        self.types_by_id.clear();
        self.ids_by_name.clear();
        Ok(true)
    }

    // helper method get named type for given import_type_name from this PendingTypes
    // this return type will be used by update_type_store method to then update type_store with this named type as import
    fn get_type_by_name_for_import(
        &self,
        import_type_name: &str,
        type_store: &mut TypeStore,
    ) -> Option<IonSchemaResult<TypeDefinitionImpl>> {
        return match self.ids_by_name.get(import_type_name) {
            Some(id) => self.types_by_id[*id]
                .to_owned()
                .map(|type_def| match type_def {
                    TypeDefinitionKind::Named(named_type_def) => Ok(named_type_def),
                    TypeDefinitionKind::Anonymous(_) => {
                        unreachable!(
                            "The TypeDefinition for the imported type '{}' was Anonymous.",
                            import_type_name
                        )
                    }
                    TypeDefinitionKind::BuiltIn(_) => {
                        unreachable!(
                            "The TypeDefinition for the imported type '{}' was a builtin type.",
                            import_type_name
                        )
                    }
                }),
            None => {
                match type_store.get_defined_type_id_or_imported_type_id_by_name(import_type_name) {
                    Some(id) => match type_store.types_by_id[*id].to_owned() {
                        TypeDefinitionKind::Named(named_type_def) => Some(Ok(named_type_def)),
                        TypeDefinitionKind::Anonymous(_) => {
                            unreachable!(
                                "The TypeDefinition for the imported type '{}' was Anonymous.",
                                import_type_name
                            )
                        }
                        TypeDefinitionKind::BuiltIn(_) => {
                            unreachable!(
                                "The TypeDefinition for the imported type '{}' was a builtin type.",
                                import_type_name
                            )
                        }
                    },
                    None => None,
                }
            }
        };
    }

    // helper method to update type store with all the types from this PendingTypes
    fn update_type_store_with_all_types(
        &self,
        type_store: &mut TypeStore,
        isl_type_names: &HashSet<&str>,
    ) -> IonSchemaResult<()> {
        for optional_type in &self.types_by_id {
            // return an error if any of the type in types_by_id vector is None/Unresolved
            let type_def = optional_type.to_owned().ok_or_else(|| {
                unresolvable_schema_error_raw("Unable to load schema due to unresolvable type")
            })?;

            match type_def {
                TypeDefinitionKind::Named(named_type_def) => {
                    // check if the type definitions that are not yet resolved actually exists within the schema
                    // we can use the isl_type_names to make sure if they exists, otherwise return error.
                    if named_type_def.is_deferred_type_def()
                        && !isl_type_names
                            .contains(named_type_def.name().as_ref().unwrap().as_str())
                    {
                        return unresolvable_schema_error(format!(
                            "Unable to load schema due to unresolvable type {}",
                            named_type_def.name().as_ref().unwrap()
                        ));
                    }
                    type_store.add_named_type(named_type_def)
                }
                TypeDefinitionKind::Anonymous(anonymous_type_def) => {
                    type_store.add_anonymous_type(anonymous_type_def)
                }
                TypeDefinitionKind::BuiltIn(builtin_type) => {
                    type_store.add_builtin_type(&builtin_type)
                }
            };
        }
        Ok(())
    }

    // helper method to update type store with all the types from this PendingTypes
    // import_type_name: this argument represents whether the import type is SchemaImport or a TypeImport (includes both TypeImport and TypeAliasImport)
    //                   None - represents its a schema import which imports all types into imported_type_ids_by_name section of type_store
    //                   Some(_) - represents a type import which import all types into types_by_id of type_store,
    //                             except specified import type as it will be already loaded by parent method that uses this helper method
    fn update_type_store_with_all_isl_imported_types(
        &self,
        isl_imported_type_name: Option<&str>,
        type_store: &mut TypeStore,
        isl_type_names: &HashSet<&str>,
    ) -> IonSchemaResult<()> {
        for optional_type in &self.types_by_id {
            // return an error if any of the type in types_by_id vector is None/Unresolved
            let type_def = optional_type.to_owned().ok_or_else(|| {
                unresolvable_schema_error_raw("Unable to load schema due to unresolvable type")
            })?;

            match type_def.to_owned() {
                TypeDefinitionKind::Named(named_type_def) => {
                    match isl_imported_type_name {
                        None => {
                            // check if the type definitions that are not yet resolved actually exists within the schema
                            // we can use the isl_type_names to make sure if they exists, otherwise return error.
                            if named_type_def.is_deferred_type_def()
                                && !isl_type_names
                                    .contains(named_type_def.name().as_ref().unwrap().as_str())
                            {
                                return unresolvable_schema_error(format!(
                                    "Unable to load schema due to unresolvable type {}",
                                    named_type_def.name().as_ref().unwrap()
                                ));
                            }
                            // imports all types into imported_type_ids_by_name section of type_store
                            type_store.add_isl_imported_type(None, named_type_def);
                        }
                        Some(import_type_name) => {
                            // check if the type definitions that are not yet resolved actually exists within the schema
                            // we can use the isl_type_names to make sure if they exists, otherwise return error.
                            if named_type_def.is_deferred_type_def()
                                && !isl_type_names
                                    .contains(named_type_def.name().as_ref().unwrap().as_str())
                            {
                                return unresolvable_schema_error(format!(
                                    "Unable to load schema due to unresolvable type {}",
                                    named_type_def.name().as_ref().unwrap()
                                ));
                            }
                            // skip the specified import type as it will be already loaded by parent method that uses this helper method
                            if named_type_def.name().as_ref().unwrap().eq(import_type_name) {
                                continue;
                            }
                            // import all types into types_by_id of type_store which will help resolving the given import type
                            type_store
                                .types_by_id
                                .push(TypeDefinitionKind::Named(named_type_def));
                        }
                    }
                }
                TypeDefinitionKind::Anonymous(anonymous_type_def) => {
                    type_store.add_anonymous_type(anonymous_type_def);
                }
                TypeDefinitionKind::BuiltIn(builtin_type) => {
                    type_store.add_builtin_type(&builtin_type);
                }
            };
        }
        Ok(())
    }

    /// Returns total number of types stored in the [`TypeStore`]
    pub(crate) fn get_total_types(&self, type_store: &mut TypeStore) -> usize {
        self.types_by_id.len() + type_store.types_by_id.len()
    }

    /// Provides the [`TypeId`] associated with given name if it exists in the [`TypeStore`] or [`PendingTypes`]  
    /// Otherwise returns None
    pub(crate) fn get_type_id_by_name(
        &self,
        name: &str,
        type_store: &mut TypeStore,
    ) -> Option<TypeId> {
        match self.ids_by_name.get(name) {
            Some(id) => Some(*id + type_store.types_by_id.len()),
            None => type_store
                .get_defined_type_id_or_imported_type_id_by_name(name)
                .copied(),
        }
    }

    /// Updates the unresolved named type that was added as None while loading types in a schema
    /// with a resolved type definition
    pub(crate) fn update_named_type(
        &mut self,
        type_id: TypeId,
        name: &str,
        type_def: TypeDefinitionImpl,
        type_store: &mut TypeStore,
    ) -> TypeId {
        if let Some(exists) = self.ids_by_name.get(name) {
            return exists.to_owned() + type_store.types_by_id.len();
        }

        if let Some(exists) = type_store.imported_type_ids_by_name.get(name) {
            return exists.to_owned();
        }

        match type_store.update_deferred_type_def(type_def.to_owned(), name) {
            None => {
                let type_id = type_id - type_store.types_by_id.len();
                self.ids_by_name.insert(name.to_owned(), type_id);
                self.types_by_id[type_id] = Some(TypeDefinitionKind::Named(type_def));
                type_id + type_store.types_by_id.len()
            }
            Some(exists) => exists,
        }
    }

    /// Updates the unresolved anonymous type that was added as None while loading types in a schema
    /// with a resolved type definition
    pub(crate) fn update_anonymous_type(
        &mut self,
        type_id: TypeId,
        type_def: TypeDefinitionImpl,
        type_store: &mut TypeStore,
    ) -> TypeId {
        self.types_by_id[type_id - type_store.types_by_id.len()] =
            Some(TypeDefinitionKind::Anonymous(type_def));
        type_id
    }

    /// Adds parent information storing the name and possible TypeId of the parent
    pub(crate) fn add_parent(&mut self, name: String, type_store: &mut TypeStore) {
        // Parent information is used when resolving a self referencing type
        // while we resolve a type using the PendingTypes (a temporary type store used while we resolve a type definition)
        // the type id for any type definition should be the PendingType's types_by_id length in  + TypeStore's types_by_id length
        // This gives a correct type id when all the types within PendingTypes are shifted to TypeStore
        self.parent = Some((name, self.types_by_id.len() + type_store.types_by_id.len()))
    }

    /// Provides parent information: (parent name, type id)
    pub(crate) fn get_parent(&self) -> &Option<(String, TypeId)> {
        &self.parent
    }

    /// Clears parent information once that tree of types is traversed
    pub(crate) fn clear_parent(&mut self) {
        self.parent = None
    }

    /// Adds the unresolved type as None before it gets resolved and gets the associated [`TypeId`]
    pub(crate) fn add_type(
        &mut self,
        type_store: &mut TypeStore,
        type_name: Option<String>,
    ) -> TypeId {
        if let Some(name) = type_name {
            if let Some(exists) = self.ids_by_name.get(&name) {
                return exists.to_owned() + type_store.types_by_id.len();
            }
            if let Some(exists) = type_store.get_defined_type_id_or_imported_type_id_by_name(&name)
            {
                return exists.to_owned();
            }
        }
        let type_id = self.types_by_id.len();
        self.types_by_id.push(None);
        type_id + type_store.types_by_id.len()
    }

    /// Adds the unresolved type as None before it gets resolved and gets the associated [`TypeId`]
    pub(crate) fn add_deferred_type_with_name(
        &mut self,
        alias: &str,
        type_store: &mut TypeStore,
    ) -> TypeId {
        let type_id = self.types_by_id.len();
        self.ids_by_name.insert(alias.to_owned(), type_id);
        self.types_by_id.push(Some(TypeDefinitionKind::Named(
            TypeDefinitionImpl::new_deferred_type_def(alias.to_owned()),
        )));
        type_id + type_store.types_by_id.len()
    }
}

/// Represents an array of BuiltIn derived ISL types
/// for more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type-system
static DERIVED_ISL_TYPES: [&str; 10] = [
    "type::{ name: lob, one_of: [ blob, clob ] }",
    "type::{ name: number, one_of: [ decimal, float, int ] }",
    "type::{ name: text, one_of: [ string, symbol ] }",
    "type::{ name: $lob, one_of: [ $blob, $clob ] }",
    "type::{ name: $number, one_of: [ $decimal, $float, $int ] }",
    "type::{ name: $text, one_of: [ $string, $symbol ] }",
    // this is just a place holder for document type,
    // IonSchemaElement::Document(_) type is used to verify the correctness on the validation side
    "type::{ name: document }",
    "type::{ name: $any, one_of: [ $blob, $bool, $clob, $decimal,
                                    $float, $int, $string, $symbol, $timestamp,
                                    $list, $sexp, $struct, $null, document ] }",
    "type::{ name: nothing, not: $any }",
    "type::{ name: any, one_of: [ blob, bool, clob, decimal,
                                    float, int, string, symbol, timestamp,
                                    list, sexp, struct, document ] }",
];

pub type TypeId = usize;

/// Defines a cache that can be used to store resolved type definitions of a [`Schema`]
#[derive(Debug, Clone)]
pub struct TypeStore {
    builtin_type_ids_by_name: HashMap<String, TypeId>, // stores all the builtin types used within this schema
    imported_type_ids_by_name: HashMap<String, TypeId>, // stores all the imported types of a schema
    ids_by_name: HashMap<String, TypeId>, // stores named types defined within the schema
    types_by_id: Vec<TypeDefinitionKind>,
}

impl Default for TypeStore {
    fn default() -> Self {
        let mut type_store = Self {
            builtin_type_ids_by_name: HashMap::new(),
            imported_type_ids_by_name: HashMap::new(),
            ids_by_name: HashMap::new(),
            types_by_id: Vec::new(),
        };
        type_store
            .preload()
            .expect("The type store didn't preload with built-in types correctly");
        type_store
    }
}

impl TypeStore {
    /// Preloads all [builtin isl types] into the TypeStore
    /// [builtin isl types]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type-system
    /// TODO: add document builtin type
    pub(crate) fn preload(&mut self) -> IonSchemaResult<()> {
        let isl_version = IslVersion::V1_0;
        // add all ion types to the type store
        // TODO: this array can be turned into an iterator implementation in ion-rust for IonType
        use IonType::*;
        let built_in_atomic_types: [IonType; 12] = [
            Int, Float, Decimal, Timestamp, String, Symbol, Bool, Blob, Clob, SExp, List, Struct,
        ];
        // add all the atomic ion types that doesn't allow nulls [type_ids: 0 - 11]
        for atomic_type in built_in_atomic_types {
            self.add_builtin_type(&BuiltInTypeDefinition::Atomic(
                atomic_type.to_owned(),
                Nullability::NotNullable,
            ));
        }

        // add all the atomic ion types that allows nulls [type_ids: 12 - 23]
        for atomic_type in built_in_atomic_types {
            self.add_builtin_type(&BuiltInTypeDefinition::Atomic(
                atomic_type.to_owned(),
                Nullability::Nullable,
            ));
        }

        // add $null to the built-in types [type_id: 24]
        self.add_builtin_type(&BuiltInTypeDefinition::Atomic(Null, Nullability::Nullable));

        // get the derived built in types map and related text value for given type_name [type_ids: 25 - 33]
        let pending_types = &mut PendingTypes::default();
        for text in DERIVED_ISL_TYPES {
            let isl_type = IslType::from_owned_element(
                isl_version,
                &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
                &mut vec![],
            )
            .unwrap();
            let type_def = BuiltInTypeDefinition::parse_from_isl_type(
                isl_version,
                &isl_type,
                self,
                pending_types,
            )?;
            self.add_builtin_type(&type_def);
        }
        Ok(())
    }

    /// Returns [`TypeId`]s stored in the [`TypeStore`] to be used by [`SchemaTypeIterator`]
    pub(crate) fn get_types(&self) -> Vec<TypeId> {
        self.ids_by_name.values().cloned().collect()
    }

    /// Returns import [`TypeId`]s stored in the [`TypeStore`] to be used by [`SchemaTypeIterator`]
    pub(crate) fn get_imports(&self) -> Vec<TypeId> {
        self.imported_type_ids_by_name.values().cloned().collect()
    }

    /// Provides the [`TypeId`] associated with given name if it exists in the [`TypeStore`] as an imported type;
    /// Otherwise returns None
    pub(crate) fn get_imported_type_id_by_name(&self, name: &str) -> Option<&TypeId> {
        self.imported_type_ids_by_name.get(name)
    }

    /// Provides the [`Type`] associated with given name if it exists in the [`TypeStore`]  
    /// Otherwise returns None
    pub(crate) fn get_type_by_name(&self, name: &str) -> Option<&TypeDefinitionKind> {
        self.ids_by_name
            .get(name)
            .and_then(|id| self.types_by_id.get(*id))
            .or_else(|| {
                self.imported_type_ids_by_name
                    .get(name)
                    .and_then(|id| self.types_by_id.get(*id))
            })
    }

    /// Provides the [`TypeId`] associated with given name if it exists in the [`TypeStore`] either as
    /// a built-in type or a type defined within schema; Otherwise returns None
    pub(crate) fn get_built_in_type_id_or_defined_type_id_by_name(
        &self,
        name: &str,
    ) -> Option<&TypeId> {
        self.ids_by_name
            .get(name)
            .or_else(|| self.builtin_type_ids_by_name.get(name))
    }

    /// Provides the [`TypeId`] associated with given name if it exists in the [`TypeStore`] as a type
    /// defined within schema (This doesn't include built-in types); Otherwise returns None
    pub(crate) fn get_defined_type_id_or_imported_type_id_by_name(
        &self,
        name: &str,
    ) -> Option<&TypeId> {
        self.ids_by_name
            .get(name)
            .or_else(|| self.imported_type_ids_by_name.get(name))
    }

    /// Provides the [`TypeId`] associated with given name if it exists in the [`TypeStore`] as a type
    /// defined within schema (This includes built-in types); Otherwise returns None
    pub(crate) fn get_type_id_by_name(&self, name: &str) -> Option<&TypeId> {
        self.ids_by_name
            .get(name)
            .or_else(|| self.imported_type_ids_by_name.get(name))
            .or_else(|| self.builtin_type_ids_by_name.get(name))
    }

    /// Provides the [`Type`] associated with given name if it exists in the [`TypeStore`] as a type
    /// defined within schema (This doesn't include built-in types and imported types); Otherwise returns None
    pub(crate) fn get_type_def_by_name(&self, name: &str) -> Option<&TypeDefinitionKind> {
        self.ids_by_name
            .get(name)
            .and_then(|id| self.types_by_id.get(*id))
    }

    /// Provides the [`TypeId`] associated with given type name if it exists in the [`TypeStore`]  
    /// Otherwise returns None
    pub(crate) fn get_builtin_type_id(&self, type_name: &str) -> Option<TypeId> {
        self.builtin_type_ids_by_name
            .get(type_name)
            .map(|t| t.to_owned())
    }

    /// Provides the [`Type`] associated with given [`TypeId`] if it exists in the [`TypeStore`]  
    /// Otherwise returns None
    pub(crate) fn get_type_by_id(&self, id: TypeId) -> Option<&TypeDefinitionKind> {
        self.types_by_id.get(id)
    }

    /// Adds the [`NamedTypeDefinition`] and the associated name in the [`TypeStore`] and returns the [`TypeId`] for it
    /// If the name already exists in the [`TypeStore`] it returns the associated [`TypeId`]
    pub(crate) fn add_named_type(&mut self, type_def: TypeDefinitionImpl) -> TypeId {
        let name = type_def.name().as_ref().unwrap();
        if let Some(exists) = self.ids_by_name.get(name) {
            return exists.to_owned();
        }
        let type_id = self.types_by_id.len();
        self.ids_by_name.insert(name.to_owned(), type_id);
        self.types_by_id.push(TypeDefinitionKind::Named(type_def));
        type_id
    }

    /// Updates the deferred [`NamedTypeDefinition`] for given type definition name
    /// If the name already exists in the [`TypeStore`] it returns the associated [`TypeId`]
    /// otherwise return [`None`]
    pub(crate) fn update_deferred_type_def(
        &mut self,
        type_def: TypeDefinitionImpl,
        name: &str,
    ) -> Option<TypeId> {
        if let Some(exists) = self.ids_by_name.get(name) {
            if let Some(TypeDefinitionKind::Named(existing_type_def)) = self.get_type_by_id(*exists)
            {
                // if existing_type_def is a deferred type def then this is the definition for it,
                // resolve the deferred type definition here by replacing with given type definition
                if existing_type_def.is_deferred_type_def() {
                    self.types_by_id[*exists] = TypeDefinitionKind::Named(type_def);
                }
            }
            return Some(*exists);
        }
        None
    }

    /// Adds the [BuiltInTypeDefinition] in the [TypeStore] and returns the [TypeId] for it
    /// If the name already exists in the [TypeStore] it returns the associated [TypeId]
    pub(crate) fn add_builtin_type(
        &mut self,
        builtin_type_definition: &BuiltInTypeDefinition,
    ) -> TypeId {
        let builtin_type_name = match builtin_type_definition {
            BuiltInTypeDefinition::Atomic(ion_type, is_nullable) => match is_nullable {
                Nullability::Nullable => format!("${ion_type}"),
                Nullability::NotNullable => format!("{ion_type}"),
            },
            BuiltInTypeDefinition::Derived(other_type) => other_type.name().to_owned().unwrap(),
        };

        if let Some(exists) = self.builtin_type_ids_by_name.get(&builtin_type_name) {
            return exists.to_owned();
        }
        let type_id = self.types_by_id.len();
        self.builtin_type_ids_by_name
            .insert(builtin_type_name, type_id);
        self.types_by_id.push(TypeDefinitionKind::BuiltIn(
            builtin_type_definition.to_owned(),
        ));
        type_id
    }

    /// Adds the [`NamedTypeDefinition`] and the associated name as the imports of [`TypeStore`]
    ///  and returns the [`TypeId`] for it. If the name already exists in the [`TypeStore`] it returns the associated [`TypeId`]
    pub(crate) fn add_isl_imported_type(
        &mut self,
        alias: Option<&String>,
        type_def: TypeDefinitionImpl,
    ) -> TypeId {
        let name = match alias {
            None => type_def.name().as_ref().unwrap(),
            Some(name) => name,
        };

        if let Some(exists) = self.imported_type_ids_by_name.get(name) {
            return exists.to_owned();
        }
        let type_id = self.types_by_id.len();
        self.imported_type_ids_by_name
            .insert(name.to_owned(), type_id);
        self.types_by_id.push(TypeDefinitionKind::Named(type_def));
        type_id
    }

    /// Adds the [`Type`] in the [`TypeStore`] and returns the [`TypeId`] for it
    pub(crate) fn add_anonymous_type(&mut self, type_def: TypeDefinitionImpl) -> TypeId {
        let type_id = self.types_by_id.len();
        self.types_by_id
            .push(TypeDefinitionKind::Anonymous(type_def));
        type_id
    }
}

/// Provides functions to load [`Schema`] with type definitions using authorities for [`SchemaSystem`]
pub struct Resolver {
    authorities: Vec<Box<dyn DocumentAuthority>>,
    resolved_schema_cache: HashMap<String, Arc<Schema>>,
}

impl Resolver {
    pub fn new(authorities: Vec<Box<dyn DocumentAuthority>>) -> Self {
        Self {
            authorities,
            resolved_schema_cache: HashMap::new(),
        }
    }

    pub fn schema_from_isl_types<A: AsRef<str>, B: Into<Vec<IslType>>>(
        &self,
        isl_version: IslVersion,
        id: A,
        isl_types: B,
    ) -> IonSchemaResult<Schema> {
        let isl_types = isl_types.into();
        // create type_store and pending types which will be used to create type definition
        let mut type_store = TypeStore::default();
        let pending_types = &mut PendingTypes::default();

        // get all isl type names from given isl types
        // this will be used to resolve type references which might not have yet resolved while loading a type definition
        let isl_type_names: HashSet<&str> = HashSet::from_iter(
            isl_types
                .iter()
                .filter(|t| t.name().is_some())
                .map(|t| t.name().as_ref().unwrap().as_str()),
        );

        for isl_type in &isl_types {
            // convert [IslType] into [TypeDefinitionKind]
            match &isl_type.name() {
                Some(isl_type_name) => {
                    // verify that the ISL type doesn't contain constraints from another ISL version
                    let has_other_isl_constraints = isl_type
                        .constraints()
                        .iter()
                        .any(|c| c.version != isl_version);

                    if has_other_isl_constraints {
                        return invalid_schema_error(format!("ISL type: {isl_type_name} contains constraints from another ISL version. Only use {isl_version} constraints for this method."));
                    }

                    TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                        isl_version,
                        isl_type,
                        &mut type_store,
                        pending_types,
                    )?
                }
                None => {
                    // top level schema types can not be anonymous
                    return invalid_schema_error("Top level types must be named type definitions");
                }
            };
        }

        // add all types from pending_types to type_store
        pending_types.update_type_store(&mut type_store, None, &isl_type_names)?;
        Ok(Schema::new(id, Arc::new(type_store)))
    }

    /// Converts given owned elements into ISL v2.0 representation
    pub fn isl_schema_from_elements<I: Iterator<Item = Element>>(
        &mut self,
        elements: I,
        id: &str,
    ) -> IonSchemaResult<IslSchema> {
        // properties that will be stored in the ISL representation
        let mut isl_imports: Vec<IslImport> = vec![];
        let mut isl_types: Vec<IslType> = vec![];
        let mut isl_inline_imports: Vec<IslImportType> = vec![];
        let mut open_content = vec![];
        let mut isl_user_reserved_fields = UserReservedFields::default();
        let mut isl_version = IslVersion::V1_0;

        let mut found_header = false;
        let mut found_footer = false;
        let mut found_type_definition = false;
        let mut found_isl_version_marker = false;

        for value in elements {
            let annotations: &Annotations = value.annotations();

            // load header for schema
            if !found_isl_version_marker
                && value.ion_type() == IonType::Symbol
                && is_isl_version_marker(value.as_text().unwrap())
            {
                // This implementation supports Ion Schema 1.0 and Ion Schema 2.0
                isl_version = match value.as_text().unwrap() {
                    "$ion_schema_1_0" => IslVersion::V1_0,
                    "$ion_schema_2_0" => IslVersion::V2_0,
                    _ => {
                        return invalid_schema_error(format!(
                            "Unsupported Ion Schema Language version: {value}"
                        ));
                    }
                };
                found_isl_version_marker = true;
            } else if annotations.contains("schema_header") {
                if isl_version == IslVersion::V2_0 {
                    if found_type_definition {
                        return invalid_schema_error(
                            "The schema header must come before top level type definitions",
                        );
                    }

                    if found_header {
                        return invalid_schema_error(
                            "Schema must only contain a single schema header",
                        );
                    }

                    if annotations.len() > 1 {
                        return invalid_schema_error(
                            "schema header must not have any other annotations then `schema_header`",
                        );
                    }
                }

                found_header = true;

                // if we didn't find an isl version marker before finding a schema header
                // then isl version will be defaulted to be ISL 1.0
                if !found_isl_version_marker {
                    found_isl_version_marker = true;
                }

                let schema_header = try_to!(value.as_struct());
                if let Some(imports) = schema_header.get("imports").and_then(|it| it.as_sequence())
                {
                    for import in imports.elements() {
                        let isl_import = IslImport::from_ion_element(import)?;
                        isl_imports.push(isl_import);
                    }
                }
                if isl_version == IslVersion::V2_0 {
                    if let Some(user_reserved_fields_element) =
                        schema_header.get("user_reserved_fields")
                    {
                        if !user_reserved_fields_element.annotations().is_empty() {
                            return invalid_schema_error(
                                "User reserved field must be an unannotated struct",
                            )?;
                        }
                        let user_reserved_fields_struct = user_reserved_fields_element
                            .as_struct()
                            .ok_or(invalid_schema_error_raw(
                                "User reserved field must be a non-null struct",
                            ))?;

                        isl_user_reserved_fields =
                            UserReservedFields::from_ion_elements(user_reserved_fields_struct)?;
                    }
                    isl_user_reserved_fields.validate_field_names_in_header(schema_header)?;
                }
            }
            // load types for schema
            else if annotations.contains("type") {
                found_type_definition = true;

                if isl_version == IslVersion::V2_0 && annotations.len() > 1 {
                    return invalid_schema_error(
                        "Top level types definitions must not have any other annotations then `type`",
                    );
                }
                // if we didn't find an isl version marker before finding a type definition
                // then isl version will be defaulted to be ISL 1.0
                if !found_isl_version_marker {
                    found_isl_version_marker = true;
                }

                // convert Element to IslType
                let isl_type: IslType =
                    IslType::from_owned_element(isl_version, &value, &mut isl_inline_imports)?;
                if isl_type.name().is_none() {
                    // if a top level type definition doesn't contain `name` field return an error
                    return invalid_schema_error(
                        "Top level types must contain field `name` in their definition",
                    );
                }

                if isl_version == IslVersion::V2_0 {
                    isl_user_reserved_fields.validate_field_names_in_type(&isl_type)?;
                }

                // top level named type definition can not contain `occurs` field as per ISL specification
                if value.as_struct().unwrap().get("occurs").is_some() {
                    return invalid_schema_error(
                        "Top level types must not contain `occurs` field in their definition",
                    );
                }

                isl_types.push(isl_type);
            }
            // load footer for schema
            else if annotations.contains("schema_footer") {
                found_footer = true;
                if isl_version == IslVersion::V2_0 {
                    if annotations.len() > 1 {
                        return invalid_schema_error(
                            "schema footer must not have any other annotations then `schema_footer`",
                        );
                    }
                    let schema_footer = try_to!(value.as_struct());
                    isl_user_reserved_fields.validate_field_names_in_footer(schema_footer)?;
                }
            } else {
                // open content
                if value.ion_type() == IonType::Symbol
                    && !value.is_null()
                    && is_isl_version_marker(value.as_text().unwrap())
                {
                    return invalid_schema_error(
                        "top level open content can not be an Ion Schema version marker",
                    );
                }

                if isl_version == IslVersion::V2_0
                    && value
                        .annotations()
                        .iter()
                        .any(|a| is_reserved_word(a.text().unwrap()))
                {
                    return invalid_schema_error(
                        "top level open content may not be annotated with any reserved keyword",
                    );
                }

                open_content.push(value);
                continue;
            }
        }

        if found_footer ^ found_header {
            return invalid_schema_error("For any schema while a header and footer are both optional, a footer is required if a header is present (and vice-versa).");
        }

        match isl_version {
            IslVersion::V1_0 => Ok(IslSchema::schema_v_1_0(
                id,
                isl_imports,
                isl_types,
                isl_inline_imports,
                open_content,
            )),
            IslVersion::V2_0 => Ok(IslSchema::schema_v_2_0(
                id,
                isl_user_reserved_fields,
                isl_imports,
                isl_types,
                isl_inline_imports,
                open_content,
            )),
        }
    }

    /// Converts given ISL representation into a [`Schema`] based on given ISL version
    pub fn schema_from_isl_schema(
        &mut self,
        isl_version: IslVersion,
        isl: IslSchema,
        type_store: &mut TypeStore,
        load_isl_import: Option<&IslImport>,
    ) -> IonSchemaResult<Arc<Schema>> {
        // This is used while resolving an import, it is initialized as `false` to indicate that
        // the type to be imported is not yet added to the type_store.
        // This will be changed to `true` as soon as the type to be imported is resolved and is added to the type_store
        let mut added_imported_type_to_type_store = false;

        if isl_version != isl.version() {
            return invalid_schema_error(format!(
                "Expected {isl_version} schema but found {}",
                isl.version()
            ));
        }

        let isl_types = isl.types();

        // Resolve all inline import types if there are any
        // this will help resolve all inline imports before they are used as a reference to another type
        for isl_inline_imported_type in isl.inline_imported_types() {
            let import_id = isl_inline_imported_type.id();
            let inline_imported_type = self.load_schema(
                import_id,
                type_store,
                Some(&IslImport::Type(isl_inline_imported_type.to_owned())),
            )?;
        }

        // Resolve all ISL imports
        for isl_import in isl.imports() {
            let import_id = isl_import.id();
            let imported_schema = self.load_schema(import_id, type_store, Some(isl_import))?;
        }

        // get all isl type names that are defined within the schema
        // this will be used to resolve type references which might not have yet resolved while loading a type definition
        let isl_type_names: HashSet<&str> = HashSet::from_iter(
            isl.types()
                .filter(|t| t.name().is_some())
                .map(|t| t.name().as_ref().unwrap().as_str()),
        );

        // Resolve all ISL types and constraints
        for isl_type in isl_types {
            let pending_types = &mut PendingTypes::default();

            if let Some(isl_type_name) = &isl_type.name() {
                // verify if there are any constraints with ISL 2.0 for this isl_type
                let has_other_isl_constraints = isl_type
                    .constraints()
                    .iter()
                    .any(|c| c.version != isl_version);

                if has_other_isl_constraints {
                    return invalid_schema_error(format!("ISL type: {isl_type_name} contains constraints from other ISL version. Only use {isl_version} constraints for this method."));
                }

                // convert IslType to TypeDefinitionKind
                let type_id: TypeId =
                    TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                        isl_version,
                        isl_type,
                        type_store,
                        pending_types,
                    )?;
            }

            // add all types from pending types to type_store
            added_imported_type_to_type_store =
                pending_types.update_type_store(type_store, load_isl_import, &isl_type_names)?;
        }

        // if currently loading an ISL import (i.e. load_isl_import != None)
        // then check if the type to be imported is added to the type_store or not
        if load_isl_import.is_some() && !added_imported_type_to_type_store {
            unreachable!(
                "Unable to load import: {} as the type/types were not added to the type_store correctly",
                isl.id()
            );
        }

        let schema = Arc::new(Schema::new(isl.id(), Arc::new(type_store.clone())));

        // add schema to schema cache
        // if we are loading an import of the schema then we can only add this schema to cache if its a full schema import
        // and can not add it to cache if we are loading specific type imports from the schema.
        match load_isl_import {
            None => {
                self.resolved_schema_cache
                    .insert(isl.id(), Arc::clone(&schema));
            }
            Some(IslImport::Schema(_)) => {
                self.resolved_schema_cache
                    .insert(isl.id(), Arc::clone(&schema));
            }
            _ => {
                // No op for type imports
            }
        }

        Ok(schema)
    }

    /// Loads a [`Schema`] with resolved [`Type`]s using authorities and type_store
    // If we are loading the root schema then this will be set to `None` ( i.e. in the beginning when
    // this method is called from the load_schema method of schema_system it is set to `None`)
    // Otherwise if we are loading an import of the schema then this will be set to `Some(isl_import)`
    // to be loaded (i.e. Inside schema_from_elements while loading imports this will be set to
    // `Some(isl_import)`)
    fn load_schema<A: AsRef<str>>(
        &mut self,
        id: A,
        type_store: &mut TypeStore,
        load_isl_import: Option<&IslImport>,
    ) -> IonSchemaResult<Arc<Schema>> {
        let id: &str = id.as_ref();

        if let Some(schema) = self.resolved_schema_cache.get(id) {
            return Ok(Arc::clone(schema));
        }

        for authority in &self.authorities {
            return match authority.elements(id) {
                Err(error) => match error {
                    IonSchemaError::IoError { source } => match source.kind() {
                        ErrorKind::NotFound => continue,
                        _ => Err(IonSchemaError::IoError { source }),
                    },
                    _ => Err(error),
                },
                Ok(schema_content) => {
                    let isl = self.isl_schema_from_elements(schema_content.into_iter(), id)?;
                    self.schema_from_isl_schema(isl.version(), isl, type_store, load_isl_import)
                }
            };
        }
        unresolvable_schema_error("Unable to load schema: ".to_owned() + id)
    }

    /// Loads an [`IslSchema`] using authorities and type_store based on ISL version.
    // If we are loading the root schema then this will be set to `None` ( i.e. in the beginning when
    // this method is called from the load_schema method of schema_system it is set to `None`)
    // Otherwise if we are loading an import of the schema then this will be set to `Some(isl_import)`
    // to be loaded (i.e. Inside schema_from_elements while loading imports this will be set to
    // `Some(isl_import)`)
    fn load_isl_schema<A: AsRef<str>>(
        &mut self,
        id: A,
        load_isl_import: Option<&IslImport>,
    ) -> IonSchemaResult<IslSchema> {
        let id: &str = id.as_ref();

        for authority in &self.authorities {
            return match authority.elements(id) {
                Ok(schema_content) => self.isl_schema_from_elements(schema_content.into_iter(), id),
                Err(IonSchemaError::IoError { source: e }) if e.kind() == ErrorKind::NotFound => {
                    continue;
                }
                Err(error) => Err(error),
            };
        }
        unresolvable_schema_error("Unable to load ISL model: ".to_owned() + id)
    }
}

/// Provides functions for instantiating instances of [`Schema`].
///
/// [`SchemaSystem`] is *[Send and Sync]* i.e. it is safe to send it to another thread and to be shared between threads.
/// For cases when one does need thread-safe interior mutability, they can use the explicit locking via [`std::sync::Mutex`] and [`std::sync::RwLock`].
///
/// [Send and Sync]: https://doc.rust-lang.org/nomicon/send-and-sync.html
pub struct SchemaSystem {
    resolver: Resolver,
}

// TODO: make methods public based on the requirements
impl SchemaSystem {
    pub fn new(authorities: Vec<Box<dyn DocumentAuthority>>) -> Self {
        Self {
            resolver: Resolver::new(authorities),
        }
    }

    /// Requests each of the provided [`DocumentAuthority`]s, in order, to resolve the requested schema id
    /// until one successfully resolves it.
    /// If an authority throws an exception, resolution silently proceeds to the next authority.
    /// This method returns an `Arc<Schema>` which allows to load this schema once re-use it across threads.
    // TODO: Add support for Rc<Schema> by providing a trait implementation of schema and schema cache. This should
    //  allow users to choose what variant of schema they want.
    pub fn load_schema<A: AsRef<str>>(&mut self, id: A) -> IonSchemaResult<Arc<Schema>> {
        self.resolver
            .load_schema(id, &mut TypeStore::default(), None)
    }

    /// Constructs a new schema using provided ISL content.
    pub fn new_schema(&mut self, schema_content: &[u8], id: &str) -> IonSchemaResult<Arc<Schema>> {
        let elements = Element::read_all(schema_content)?;
        let isl = self
            .resolver
            .isl_schema_from_elements(elements.into_iter(), id)?;
        self.resolver
            .schema_from_isl_schema(isl.version(), isl, &mut TypeStore::default(), None)
    }

    /// Requests each of the provided [`DocumentAuthority`]s, in order, to get ISL model for the
    /// requested schema id until one successfully resolves it.
    /// If an authority throws an exception, resolution silently proceeds to the next authority.
    pub fn load_isl_schema<A: AsRef<str>>(&mut self, id: A) -> IonSchemaResult<IslSchema> {
        self.resolver.load_isl_schema(id, None)
    }

    /// Constructs a new ISL model using provided ISL content.
    pub fn new_isl_schema(
        &mut self,
        schema_content: &[u8],
        id: &str,
    ) -> IonSchemaResult<IslSchema> {
        let elements = Element::read_all(schema_content)?;
        self.resolver
            .isl_schema_from_elements(elements.into_iter(), id)
    }

    /// Resolves given ISL 1.0 model into a [Schema].
    /// If the given ISL model has any ISL 2.0 related types/constraints, resolution returns an error.
    /// This method returns an `Arc<Schema>` which allows to load this schema once re-use it across threads.
    // TODO: Add support for Rc<Schema> by providing a trait implementation of schema and schema cache. This should
    //  allow users to choose what variant of schema they want.
    pub fn load_schema_from_isl_schema_v1_0(
        &mut self,
        isl: IslSchema,
    ) -> IonSchemaResult<Arc<Schema>> {
        self.resolver
            .schema_from_isl_schema(IslVersion::V1_0, isl, &mut TypeStore::default(), None)
    }

    /// Resolves given ISL 2.0 model into a [Schema].
    /// If the given ISL model has any ISL 1.0 related types/constraints, resolution returns an error.
    /// This method returns an `Arc<Schema>` which allows to load this schema once re-use it across threads.
    // TODO: Add support for Rc<Schema> by providing a trait implementation of schema and schema cache. This should
    //  allow users to choose what variant of schema they want.
    pub fn load_schema_from_isl_schema_v2_0(
        &mut self,
        isl: IslSchema,
    ) -> IonSchemaResult<Arc<Schema>> {
        self.resolver
            .schema_from_isl_schema(IslVersion::V2_0, isl, &mut TypeStore::default(), None)
    }

    /// Returns authorities associated with this [`SchemaSystem`]
    fn authorities(&mut self) -> &[Box<dyn DocumentAuthority>] {
        &self.resolver.authorities
    }

    /// Adds the provided authority to the list of [`DocumentAuthority`]s.
    fn add_authority(&mut self, authority: Box<dyn DocumentAuthority>) {
        self.resolver.authorities.push(authority);
    }

    /// Replaces the list of [`DocumentAuthority`]s with a list containing only the specified authority.
    fn with_authority(&mut self, authority: Box<dyn DocumentAuthority>) {
        let authorities: Vec<Box<dyn DocumentAuthority>> = vec![authority];
        self.resolver.authorities = authorities;
    }

    // TODO: Use IntoIterator here instead of a Vec
    /// Replaces the list of [`DocumentAuthority`]s with the specified list of [`DocumentAuthority`]s.
    fn with_authorities(&mut self, authorities: Vec<Box<dyn DocumentAuthority>>) {
        self.resolver.authorities = authorities;
    }

    /// Creates a schema from given [`IslType`]s using ISL 1.0
    /// Note: This method assumes that there are no imported type definitions used for these [`IslType`]s
    pub fn schema_from_isl_types_v1_0<A: AsRef<str>, B: Into<Vec<IslType>>>(
        &self,
        id: A,
        isl_types: B,
    ) -> IonSchemaResult<Schema> {
        self.resolver
            .schema_from_isl_types(IslVersion::V1_0, id, isl_types)
    }

    /// Creates a schema from given [`IslType`]s using ISL 2.0
    /// Note: This method assumes that there are no imported type definitions used for these [`IslType`]s
    pub fn schema_from_isl_types_v2_0<A: AsRef<str>, B: Into<Vec<IslType>>>(
        &self,
        id: A,
        isl_types: B,
    ) -> IonSchemaResult<Schema> {
        self.resolver
            .schema_from_isl_types(IslVersion::V2_0, id, isl_types)
    }
}

#[cfg(test)]
mod schema_system_tests {
    use super::*;
    use crate::authority::{FileSystemDocumentAuthority, MapDocumentAuthority};
    use crate::isl::isl_constraint;
    use crate::isl::isl_type;
    use crate::isl::isl_type_reference;
    use crate::system::IonSchemaError::InvalidSchemaError;
    use std::path::Path;

    #[test]
    fn schema_system_add_authorities_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(
            FileSystemDocumentAuthority::new(Path::new("")),
        )]);
        schema_system.add_authority(Box::new(FileSystemDocumentAuthority::new(Path::new(
            "test",
        ))));
        let schema_system_authorities = schema_system.authorities();
        assert_eq!(2, schema_system_authorities.len());
    }

    #[test]
    fn schema_system_with_authority_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(
            FileSystemDocumentAuthority::new(Path::new("")),
        )]);
        schema_system.with_authority(Box::new(FileSystemDocumentAuthority::new(Path::new(
            "test",
        ))));
        let schema_system_authorities = schema_system.authorities();
        assert_eq!(1, schema_system_authorities.len());
    }

    #[test]
    fn schema_system_with_authorities_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(
            FileSystemDocumentAuthority::new(Path::new("")),
        )]);
        schema_system.with_authorities(vec![
            Box::new(FileSystemDocumentAuthority::new(Path::new("test"))),
            Box::new(FileSystemDocumentAuthority::new(Path::new("ion"))),
        ]);
        let schema_system_authorities = schema_system.authorities();
        assert_eq!(2, schema_system_authorities.len());
    }

    #[test]
    fn schema_system_map_authority_with_type_alias_import_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample_number.isl",
                r#"
                    schema_header::{
                      imports: [{ id: "sample_decimal.isl", type: my_decimal, as: other_decimal }],
                    }
                    
                    type::{
                      name: my_int,
                      type: int,
                    }
                    
                    type::{
                      name: my_number,
                      all_of: [
                        my_int,
                        other_decimal,
                      ],
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_decimal.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_decimal,
                      type: decimal,
                    }
                    
                    type::{
                      name: my_string,
                      type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample_number.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_type_import_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample_number.isl",
                r#"
                    schema_header::{
                      imports: [{ id: "sample_decimal.isl", type: my_decimal }],
                    }
                    
                    type::{
                      name: my_int,
                      type: int,
                    }
                    
                    type::{
                      name: my_number,
                      all_of: [
                        my_int,
                        my_decimal,
                      ],
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_decimal.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_decimal,
                      type: decimal,
                    }
                    
                    type::{
                      name: my_string,
                      type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample_number.isl");
        assert!(schema.is_ok());

        // Verify that the schema has the imported types and defined types
        let isl_imported_type = schema.as_ref().unwrap().get_imported_type("my_decimal");
        assert!(isl_imported_type.is_some());
        let isl_defined_type = schema
            .as_ref()
            .unwrap()
            .get_built_in_or_defined_type("my_number");
        assert!(isl_defined_type.is_some());
        let isl_type = schema.as_ref().unwrap().get_type("my_decimal");
        assert!(isl_type.is_some());
    }

    #[test]
    fn schema_system_map_authority_with_schema_import_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample_import_string.isl",
                r#"
                    schema_header::{
                      imports: [{ id: "sample_string.isl" }],
                    }
                    
                    type::{
                      name: import_string,
                      type: my_string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_string.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_string,
                      type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample_import_string.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_inline_import_type_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample_number.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_int,
                      type: int,
                    }
                    
                    type::{
                      name: my_number,
                      all_of: [
                        my_int,
                        { id: "sample_decimal.isl", type: my_decimal },
                      ],
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_decimal.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_decimal,
                      type: decimal,
                    }
                    
                    type::{
                      name: my_string,
                      type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample_number.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_incorrect_inline_import_type_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample_number.isl",
                r#"
                    $ion_schema_2_0
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_int,
                      type: int,
                    }
                    
                    type::{
                      name: my_number,
                      all_of: [
                        my_int,
                        { id: "sample_decimal.isl", type: my_decimal, as: other_decimal},
                      ],
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_decimal.isl",
                r#"
                    $ion_schema_2_0
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_decimal,
                      type: decimal,
                    }
                    
                    type::{
                      name: my_string,
                      type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads with specific errors
        let schema = schema_system.load_schema("sample_number.isl");
        assert!(schema.is_err());
        assert!(matches!(schema.unwrap_err(), InvalidSchemaError { .. }));
    }

    #[test]
    fn schema_system_map_authority_with_isl_builtin_derived_types() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample.isl",
                r#"
                    schema_header::{
                      imports: [ { id: "sample_builtin_types.isl" } ],
                    }
                    
                    type::{
                        name: my_number,
                        type: number,
                    }
                    
                    type::{
                      name: my_type,
                      one_of: [
                        my_number,
                        my_text,
                        my_lob
                      ],
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_builtin_types.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_text,
                      type: text,
                    }
                    
                    type::{
                        name: my_lob,
                        type: lob,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_isl_builtin_derived_nullable_types() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample.isl",
                r#"
                    schema_header::{
                      imports: [ { id: "sample_builtin_nullable_types.isl" } ],
                    }
                    
                    type::{
                        name: my_number,
                        type: $number,
                    }
                    
                    type::{
                        name: my_any,
                        type: $any,
                    }
                    
                    type::{
                      name: my_type,
                      one_of: [
                        my_number,
                        my_text,
                        my_lob
                      ],
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_builtin_nullable_types.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_text,
                      type: $text,
                    }
                    
                    type::{
                        name: my_lob,
                        type: $lob,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_valid_transitive_type_import_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample.isl",
                r#"
                    schema_header::{
                      imports: [ { id: "sample_builtin_nullable_types.isl", type: my_text } ],
                    }

                    type::{
                      name: my_type,
                      type: my_text
                    }

                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_builtin_nullable_types.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_text,
                      one_of: [
                        my_string, 
                        symbol,
                      ],
                    }
                    
                    type::{
                        name: my_string,
                        type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any error
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_invalid_transitive_type_import_test() {
        // map with (id, ion content)
        let map_authority = [
            (
                "sample.isl",
                r#"
                    schema_header::{
                      imports: [ { id: "sample_builtin_nullable_types.isl", type: my_text } ],
                    }
                    
                    type::{
                      name: my_type,
                      type: my_string, // this type reference was not imported by name in sample.isl 
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
            (
                "sample_builtin_nullable_types.isl",
                r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_text,
                      one_of: [
                        my_string, 
                        symbol,
                      ],
                    }
                    
                    type::{
                        name: my_string,
                        type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
            ),
        ];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads with an error for invalid transitive type import
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn schema_system_map_authority_with_multiple_type_definitions() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                    schema_header::{
                      imports: [],
                    }
                    
                    type::{
                      name: my_text,
                      one_of: [
                        my_string, 
                        symbol,
                      ],
                    }
                    
                    type::{
                        name: my_string,
                        type: string,
                    }
                    
                    schema_footer::{
                    }
                "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_multiple_codependent_type_definitions() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                    type::{
                       name: node_a,
                       type: list,
                       element: node_b,
                    }
                    
                    type::{
                       name: node_b,
                       type: sexp,
                       element: node_a,
                    }
                "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn schema_system_map_authority_with_self_referencing_type_definition() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                    type::{
                       name: binary_heap_node,
                       type: sexp,
                       element: { one_of: [ binary_heap_node, int ] },
                       container_length: range::[0, 2],
                    }
                "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn top_level_type_def_without_name_field() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                    type::{
                        // top level type definition must contain a `name` field
                    }
                "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema return error for top level type definition without `name` field
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn top_level_type_def_with_multiple_name_field() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                    type::{
                        name: my_type,
                        name: new_type
                    }
                "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema return error for top level type definition with multiple `name` field
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn top_level_type_def_with_non_symbol_name_field() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                    type::{
                        name: "my_type"
                    }
                "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema return error for top level type definition with a `name` field of string type
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn valid_isl_version_marker_test() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
               $ion_schema_1_0
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema loads without any errors
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_ok());
    }

    #[test]
    fn invalid_isl_version_marker_test() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                $ion_schema_4_5
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema return error for invalid ISL version marker
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn unsupported_isl_version_marker_test() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                $ion_schema_2_1
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        // verify if the schema return error for unsupported ISL version marker
        let schema = schema_system.load_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn open_content_test() -> IonSchemaResult<()> {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                schema_header::{}
                
                type::{
                  name: my_type,
                  type: string,
                }
                
                open_content_1::{
                    unknown_constraint: "this is an open content struct"
                }
                
                open_content_2::{
                    unknown_constraint: "this is an open content struct"
                }
                
                schema_footer::{}
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        let schema = schema_system.load_isl_schema("sample.isl")?;
        let expected_open_content: Vec<_> = Element::read_all(
            r#"
                open_content_1::{
                    unknown_constraint: "this is an open content struct"
                }
                
                open_content_2::{
                    unknown_constraint: "this is an open content struct"
                }
            "#
            .as_bytes(),
        )?
        .into_iter()
        .collect();

        // verify the open content that is retrieved from the ISL model is same as the expected open content
        let open_content: Vec<_> = schema.open_content().map(|x| x.to_owned()).collect();
        assert_eq!(open_content.len(), 2);
        assert_eq!(open_content, expected_open_content);
        Ok(())
    }

    #[test]
    fn unexpected_fields_in_isl_v2_0_header() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                $ion_schema_2_0
                schema_header::{
                    user_reserved_fields: {
                        schema_header: [ foo, bar ],
                        type: [ baz ]
                    },
                    baz: "this is an unexpected field in schema header"
                }
                
                type::{
                  name: my_type,
                  type: string,
                }
                
                schema_footer::{}
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        let schema = schema_system.load_isl_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn unexpected_fields_in_isl_v2_0_footer() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                $ion_schema_2_0
                schema_header::{
                    user_reserved_fields: {
                        schema_footer: [ foo, bar ],
                        sdhema_header: [ baz ]
                    },
                }
                
                type::{
                  name: my_type,
                  type: string,
                }
                
                schema_footer::{
                    baz: "this is an unexpected field in schema header"
                }
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        let schema = schema_system.load_isl_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn unexpected_fields_in_isl_v2_0_type() {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                $ion_schema_2_0
                schema_header::{
                    user_reserved_fields: {
                        schema_header: [ baz ],
                        type: [ foo, bar ]
                    },
                }
                
                type::{
                  name: my_type,
                  type: string,
                  baz: "this is an unexpected field in schema header"
                }
                
                schema_footer::{}
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        let schema = schema_system.load_isl_schema("sample.isl");
        assert!(schema.is_err());
    }

    #[test]
    fn load_schema_from_isl_schema_v1_0_test() {
        // an ISL 1.0 type definition
        let isl_type = isl_type::v_1_0::named_type(
            "my_type",
            [isl_constraint::v_1_0::element(
                isl_type_reference::v_1_0::named_type_ref("int"),
            )],
        );
        let isl = IslSchema::schema_v_1_0("sample.isl", vec![], vec![isl_type], vec![], vec![]);
        let mut schema_system = SchemaSystem::new(vec![]);
        let schema = schema_system.load_schema_from_isl_schema_v1_0(isl);

        // verify the resolved schema generated from the ISL 1.0 model is valid
        assert!(schema.is_ok());
    }

    #[test]
    fn load_schema_from_isl_schema_v2_0_test() {
        // an ISL 2.0 type definition
        let isl_type = isl_type::v_2_0::named_type(
            "my_type",
            [isl_constraint::v_2_0::type_constraint(
                isl_type_reference::v_2_0::named_type_ref("int"),
            )],
        );
        let isl = IslSchema::schema_v_2_0(
            "sample.isl",
            UserReservedFields::default(),
            vec![],
            vec![isl_type],
            vec![],
            vec![],
        );
        let mut schema_system = SchemaSystem::new(vec![]);
        let schema = schema_system.load_schema_from_isl_schema_v2_0(isl);

        // verify the resolved schema generated from the ISL 2.0 model is valid
        assert!(schema.is_ok());
    }

    #[test]
    fn load_schema_from_isl_schema_v1_0_with_isl_v2_0_constraints_test() {
        // an ISL 1.0 type that contains ISL 2.0 related constraints
        let isl_type = isl_type::v_1_0::named_type(
            "my_type",
            [isl_constraint::v_2_0::type_constraint(
                isl_type_reference::v_2_0::named_type_ref("int"),
            )],
        );
        let isl = IslSchema::schema_v_1_0("sample.isl", vec![], vec![isl_type], vec![], vec![]);
        let mut schema_system = SchemaSystem::new(vec![]);
        let schema = schema_system.load_schema_from_isl_schema_v1_0(isl);

        // verify the resolved schema generated from the ISL 1.0 model that contains ISL 2.0 constraints is invalid
        assert!(schema.is_err());
    }

    #[test]
    fn load_schema_from_isl_schema_v2_0_with_isl_v1_0_constraints_test() {
        // an ISL 2.0 type that contains ISL 1.0 related constraints
        let isl_type = isl_type::v_2_0::named_type(
            "my_type",
            [isl_constraint::v_1_0::type_constraint(
                isl_type_reference::v_1_0::named_type_ref("int"),
            )],
        );
        let isl = IslSchema::schema_v_2_0(
            "sample.isl",
            UserReservedFields::default(),
            vec![],
            vec![isl_type],
            vec![],
            vec![],
        );
        let mut schema_system = SchemaSystem::new(vec![]);
        let schema = schema_system.load_schema_from_isl_schema_v2_0(isl);

        // verify the resolved schema generated from the ISL 2.0 model that contains ISL 1.0 constraints is invalid
        assert!(&schema.is_err());
    }

    #[test]
    fn new_schema_test() {
        let mut schema_system = SchemaSystem::new(vec![]);
        let schema = schema_system.new_schema(
            br#"
                $ion_schema_2_0
                schema_header::{}
                
                type::{
                  name: my_type,
                  type: string,
                }
                
                schema_footer::{}
            "#,
            "sample.isl",
        );
        assert!(schema.is_ok());
    }

    #[test]
    fn new_schema_invalid_test() {
        let mut schema_system = SchemaSystem::new(vec![]);
        let schema = schema_system.new_schema(
            br#"
                $ion_schema_2_0
                schema_header::{}
                
                type::{
                  name: my_type,
                  type: nullable::string, // `nullable` annotation is not supported in ISL 2.0
                }
                
                schema_footer::{}
            "#,
            "sample.isl",
        );
        assert!(schema.is_err());
    }

    #[test]
    fn new_isl_schema_test() {
        let mut schema_system = SchemaSystem::new(vec![]);
        let isl = schema_system.new_isl_schema(
            br#"
                $ion_schema_2_0
                schema_header::{}
                
                type::{
                  name: my_type,
                  type: string,
                }
                
                schema_footer::{}
            "#,
            "sample.isl",
        );
        assert!(isl.is_ok());
    }

    #[test]
    fn new_isl_schema_invalid_test() {
        let mut schema_system = SchemaSystem::new(vec![]);
        let isl = schema_system.new_isl_schema(
            br#"
                $ion_schema_2_0
                schema_header::{}
                
                type::{
                  name: my_type,
                  type: nullable::string, // `nullable` annotation is not supported in ISL 2.0
                }
                
                schema_footer::{}
            "#,
            "sample.isl",
        );
        assert!(isl.is_err());
    }

    #[test]
    fn test_send_schema() {
        fn assert_send<T: Send>() {}
        assert_send::<Schema>();
    }

    #[test]
    fn test_sync_schema() {
        fn assert_sync<T: Sync>() {}
        assert_sync::<Schema>();
    }

    #[test]
    fn test_send_schema_system() {
        fn assert_send<T: Send>() {}
        assert_send::<SchemaSystem>();
    }

    #[test]
    fn test_sync_schema_system() {
        fn assert_sync<T: Sync>() {}
        assert_sync::<SchemaSystem>();
    }
}
