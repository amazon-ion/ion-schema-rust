// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::{SchemaDocument, TypeDefinition};
use crate::resolver::SchemaStore;
use crate::resolver::{resolve, unresolve};
use crate::result::IonSchemaError;
use std::sync::Arc;

/// Represents a [`SchemaDocument`] that has had all of its type references successfully resolved
/// and linked to their target types. From a [`ResolvedSchema`], you can get a [`ResolvedType`],
/// which can be used to perform data validation.
///
/// ### Example:
/// ```
/// # use ion_schema::result::IonSchemaResult;
/// use ion_schema::ISL_2_0;
/// use ion_schema::model::{SchemaDocument, TypeDefinition};
/// use ion_schema::resolver::ResolvedSchema;
///
/// # fn foo() -> IonSchemaResult<()> {
/// let schema = SchemaDocument::builder::<ISL_2_0>()
///     .type_definition("short_string", TypeDefinition::builder()
///         .codepoint_length(0..10)
///         .build())
///     .build();
///
/// let resolved_schema: ResolvedSchema = schema.try_into()?;
/// # Ok(())
/// # }
/// ```
///
/// For resolving schemas with dependencies on other schemas, see [`resolve`](crate::resolve).
#[derive(Debug, Clone)]
pub struct ResolvedSchema {
    schema_store: Arc<SchemaStore>,
    schema_index: usize,
}

impl ResolvedSchema {
    /// Constructs a [`ResolvedSchema`] backed by the given [`SchemaStore`].
    pub(super) fn new(schema_store: Arc<SchemaStore>, schema_index: usize) -> Self {
        Self {
            schema_store,
            schema_index,
        }
    }

    /// Returns the [`SchemaStore`] that backs this [`ResolvedSchema`].
    pub(super) fn schema_store(&self) -> Arc<SchemaStore> {
        self.schema_store.clone()
    }

    /// Gets a [`ResolvedType`] from this schema.
    pub fn get_type(&self, type_name: &str) -> Option<ResolvedType<'_>> {
        let schema = self.schema_store.get_schema(self.schema_index);
        let type_index = schema.get_type_idx_by_name(type_name)?;
        Some(ResolvedType {
            resolved_schema: self,
            type_index,
        })
    }

    /// Immutably borrows the [`SchemaDocument`] that backs this [`ResolvedSchema`].
    pub fn as_schema_document(&self) -> &SchemaDocument {
        self.schema_store.get_schema(self.schema_index)
    }

    /// Consumes this [`ResolvedSchema`], and attempts to return the [`SchemaDocument`]s for this
    /// schema and all of its dependencies.
    ///
    /// If all other [`ResolvedSchema`] that were resolved in the same batch have been dropped,
    /// returns all the schemas that were used in resolving this schema. Otherwise, returns `None`.
    ///
    /// To get an owned copy of the [`SchemaDocument`] for just this schema, you can alternately
    /// use `self.as_schema_document().clone()`.
    pub fn unresolve(self) -> Option<impl Iterator<Item = (String, SchemaDocument)>> {
        unresolve([("".to_owned(), self)])
    }
}

// Implementation of TryFrom that wraps the `resolve` function.
impl TryFrom<SchemaDocument> for ResolvedSchema {
    type Error = IonSchemaError;
    fn try_from(value: SchemaDocument) -> Result<Self, Self::Error> {
        let mut resolved = resolve([("".to_owned(), value)])?;
        Ok(resolved.next().unwrap().1)
    }
}

impl AsRef<SchemaDocument> for ResolvedSchema {
    fn as_ref(&self) -> &SchemaDocument {
        self.as_schema_document()
    }
}

/// A representation of a [`TypeDefinition`] that can be used for validation.
#[derive(Clone, Copy, Debug)]
pub struct ResolvedType<'schema> {
    resolved_schema: &'schema ResolvedSchema,
    type_index: usize,
}

impl ResolvedType<'_> {
    // TODO: validate functions
}

impl AsRef<TypeDefinition> for ResolvedType<'_> {
    fn as_ref(&self) -> &TypeDefinition {
        self.resolved_schema
            .as_schema_document()
            .get_type_by_idx(self.type_index)
            .unwrap()
    }
}
