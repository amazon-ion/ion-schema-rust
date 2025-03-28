// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::schema::schema_doc_builder_state::{BeforeHeader, HasFooter, Types};
use crate::model::{SchemaHeader, TypeDefinition, VersionedTypeDefinition};
use crate::resolver::*;
use crate::{IslVersion, Versioned};
use ion_rs::Element;
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq)]
pub enum SchemaItem {
    Header(SchemaHeader),
    Type(String, TypeDefinition),
    OpenContent(Element),
    Footer(SchemaFooter),
}
impl From<SchemaHeader> for SchemaItem {
    fn from(value: SchemaHeader) -> Self {
        SchemaItem::Header(value)
    }
}
impl From<SchemaFooter> for SchemaItem {
    fn from(value: SchemaFooter) -> Self {
        SchemaItem::Footer(value)
    }
}
impl From<Element> for SchemaItem {
    fn from(value: Element) -> Self {
        SchemaItem::OpenContent(value)
    }
}

impl TypeRefWalker for SchemaItem {
    fn walk<V: TypeRefVisitor>(&self, visitor: &mut V) {
        if let SchemaItem::Type(_, t) = self {
            t.walk(visitor)
        }
    }
}

/// Represents an Ion Schema document.
///
/// A schema is a collection of types that can be used to constrain the Ion data model.
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaDocument {
    /// The version in which this schema was created, but not necessarily the only version for which it is valid.
    isl_version: (u8, u8),
    items: Vec<SchemaItem>,
    types_by_name: HashMap<String, usize>,
}
impl_type_ref_walker!(SchemaDocument, items);

impl SchemaDocument {
    pub fn builder<V: IslVersion>() -> SchemaDocumentBuilder<V, BeforeHeader> {
        SchemaDocumentBuilder::new()
    }

    fn new<V: IslVersion>(items: Vec<SchemaItem>) -> Self {
        let types_by_name = items
            .iter()
            .enumerate()
            .filter_map(|(idx, item)| match item {
                SchemaItem::Type(name, _) => Some((name.clone(), idx)),
                _ => None,
            })
            .collect();
        let isl_version = V::MAJOR_MINOR;
        SchemaDocument {
            isl_version,
            items,
            types_by_name,
        }
    }

    /// The ISL version with which this `SchemaDocument` was constructed.
    pub fn source_version(&self) -> (u8, u8) {
        self.isl_version
    }

    /// Iterate all items in this schema definition
    pub fn items(&self) -> impl Iterator<Item = &SchemaItem> {
        self.items.iter()
    }

    /// Get a type from the schema, by name.
    pub fn get_type(&self, name: &str) -> Option<&TypeDefinition> {
        self.items.iter().find_map(|item| match item {
            SchemaItem::Type(type_name, type_def) if name == type_name => Some(type_def),
            _ => None,
        })
    }

    pub(crate) fn get_type_idx_by_name(&self, name: &str) -> Option<usize> {
        self.items
            .iter()
            .enumerate()
            .find_map(|(idx, item)| match item {
                SchemaItem::Type(type_name, _) if name == type_name => Some(idx),
                _ => None,
            })
    }

    pub(crate) fn get_type_by_idx(&self, id: usize) -> Option<&TypeDefinition> {
        let item = self.items.get(id);
        match item {
            Some(SchemaItem::Type(_, type_def)) => Some(type_def),
            _ => None,
        }
    }
}

/// Holds empty types that are states for [`SchemaDocumentBuilder`].
/// This module is _not_ `pub` so that the type-states are not nameable (for now).
mod schema_doc_builder_state {
    pub enum BeforeHeader {}
    pub enum Types {}
    pub enum HasFooter {}
}

#[derive(Debug, Clone)]
pub struct SchemaDocumentBuilder<V: IslVersion, State> {
    version: PhantomData<V>,
    state: PhantomData<State>,
    items: Vec<SchemaItem>,
}
impl<V: IslVersion> SchemaDocumentBuilder<V, BeforeHeader> {
    fn new() -> Self {
        SchemaDocumentBuilder {
            version: PhantomData,
            state: PhantomData,
            items: vec![],
        }
    }

    pub fn header(mut self, header: Versioned<SchemaHeader, V>) -> SchemaDocumentBuilder<V, Types> {
        self.items.push(header.into_inner().into());
        self.change_state()
    }

    pub fn type_definition<S: Into<String>>(
        self,
        name: S,
        type_def: VersionedTypeDefinition<V>,
    ) -> SchemaDocumentBuilder<V, Types> {
        self.change_state::<Types>().type_definition(name, type_def)
    }

    pub fn footer(self, footer: Versioned<SchemaFooter, V>) -> SchemaDocumentBuilder<V, HasFooter> {
        self.change_state::<Types>().footer(footer)
    }
}

impl<V: IslVersion> SchemaDocumentBuilder<V, Types> {
    pub fn type_definition<S: Into<String>>(
        mut self,
        name: S,
        type_def: VersionedTypeDefinition<V>,
    ) -> Self {
        self.items.push(SchemaItem::Type(
            name.into(),
            VersionedTypeDefinition::into_inner(type_def),
        ));
        self
    }

    pub fn footer(
        mut self,
        footer: Versioned<SchemaFooter, V>,
    ) -> SchemaDocumentBuilder<V, HasFooter> {
        self.items.push(footer.into_inner().into());
        self.change_state()
    }
}

impl<V: IslVersion, S> SchemaDocumentBuilder<V, S> {
    pub fn open_content(mut self, content: Element) -> SchemaDocumentBuilder<V, S> {
        // TODO: Check open content
        self.items.push(content.into());
        self
    }

    pub fn build(self) -> SchemaDocument {
        SchemaDocument::new::<V>(self.items)
    }

    fn change_state<ToState>(self) -> SchemaDocumentBuilder<V, ToState> {
        SchemaDocumentBuilder {
            version: self.version,
            state: PhantomData,
            items: self.items,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaFooter {
    // TODO: Add open content
}
impl SchemaFooter {
    pub fn empty<V: IslVersion>() -> Versioned<SchemaFooter, V> {
        Versioned::new(SchemaFooter {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::bag::bag;
    use crate::model::constraints::ContainerLength;
    use crate::model::TypeDefinitionBuilder;
    use crate::ISL_2_0;

    #[test]
    fn test_builder() {
        let header = SchemaHeader::builder()
            .import(("foo.isl", "BarType"))
            .build();

        let schema = SchemaDocument::builder::<ISL_2_0>()
            .header(header.clone())
            .type_definition(
                "quux",
                TypeDefinitionBuilder::new().container_length(0..5).build(),
            )
            .open_content(Element::string("foo"))
            .footer(SchemaFooter::empty())
            .build();

        assert_eq!((2, 0), schema.source_version());

        assert_eq!(
            schema.items.to_vec(),
            vec![
                SchemaItem::Header(header.into_inner()),
                SchemaItem::Type(
                    "quux".to_string(),
                    TypeDefinition::new(bag![ContainerLength::new(0..5).into()], bag![])
                ),
                SchemaItem::OpenContent(Element::string("foo")),
                SchemaItem::Footer(SchemaFooter {})
            ]
        )
    }
}
