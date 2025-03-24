// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::bag::Bag;
use crate::{IslVersion, Versioned, ISL_2_0};
use ion_rs::Element;
use std::marker::PhantomData;

/// Represents an Ion Schema document header, providing support for schema and type imports in
/// [ISL 1.0] and [ISL 2.0], and [open content] in ISL 2.0).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#schema-definitions
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#imports
/// [open content]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#open-content
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaHeader {
    imports: Bag<Import>,
    user_reserved_header_keywords: Bag<String>,
    user_reserved_type_keywords: Bag<String>,
    user_reserved_footer_keywords: Bag<String>,
    open_content: Bag<(String, Element)>,
}

impl SchemaHeader {
    pub fn builder<V: IslVersion>() -> SchemaHeaderBuilder<V> {
        SchemaHeaderBuilder::new()
    }

    pub fn imports(&self) -> impl Iterator<Item = &Import> {
        self.imports.iter()
    }

    pub fn open_content(&self) -> impl Iterator<Item = &(String, Element)> {
        self.open_content.iter()
    }
}

/// Represents an import in a [`SchemaHeader`].
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    schema_id: String,
    type_import: Option<TypeImport>,
}

/// Narrows an [`Import`] to a specific type from the source schema, with an optional alias.
#[derive(Debug, Clone, PartialEq)]
struct TypeImport {
    type_name: String,
    alias: Option<String>,
}
impl Import {
    pub fn schema(schema_id: &str) -> Self {
        Import {
            schema_id: schema_id.to_string(),
            type_import: None,
        }
    }
    pub fn schema_type(schema_id: &str, type_name: &str, alias: Option<&str>) -> Self {
        Import {
            schema_id: schema_id.to_string(),
            type_import: Some(TypeImport {
                type_name: type_name.to_string(),
                alias: alias.map(|s| s.to_string()),
            }),
        }
    }
}

/// Builds a [`SchemaHeader`].
///
/// ### Example:
/// ```
/// # use ion_schema::model::SchemaHeaderBuilder;
/// # use ion_schema::ISL_1_0;
/// # use ion_schema::model::SchemaHeader;
/// let header = SchemaHeader::builder::<ISL_1_0>()
///     .import("foo_schema.isl")
///     .import(("bar_schema.isl", "baz_type"))
///     .build();
/// ```
#[derive(Debug, Clone)]
pub struct SchemaHeaderBuilder<V: IslVersion> {
    version: PhantomData<V>,
    imports: Vec<Import>,
    user_reserved_header_keywords: Vec<String>,
    user_reserved_type_keywords: Vec<String>,
    user_reserved_footer_keywords: Vec<String>,
    open_content: Vec<(String, Element)>,
}

impl<V: IslVersion> Default for SchemaHeaderBuilder<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V: IslVersion> SchemaHeaderBuilder<V> {
    pub fn new() -> Self {
        SchemaHeaderBuilder {
            version: PhantomData,
            imports: vec![],
            user_reserved_header_keywords: vec![],
            user_reserved_type_keywords: vec![],
            user_reserved_footer_keywords: vec![],
            open_content: vec![],
        }
    }

    pub fn import<T: IntoImport>(mut self, import: T) -> Self {
        self.imports.push(import.into_import());
        self
    }

    pub fn build(self) -> Versioned<SchemaHeader, V> {
        Versioned::new(SchemaHeader {
            imports: self.imports.into(),
            user_reserved_header_keywords: self.user_reserved_header_keywords.into(),
            user_reserved_type_keywords: self.user_reserved_type_keywords.into(),
            user_reserved_footer_keywords: self.user_reserved_footer_keywords.into(),
            open_content: self.open_content.into(),
        })
    }
}

impl SchemaHeaderBuilder<ISL_2_0> {
    pub fn reserve_header_keywords<S: Into<String>, I: IntoIterator<Item = S>>(
        mut self,
        keywords: I,
    ) -> Self {
        keywords
            .into_iter()
            .map(S::into)
            .for_each(|s| self.user_reserved_header_keywords.push(s));
        self
    }
    pub fn reserve_type_keywords<S: Into<String>, I: IntoIterator<Item = S>>(
        mut self,
        keywords: I,
    ) -> Self {
        keywords
            .into_iter()
            .map(S::into)
            .for_each(|s| self.user_reserved_type_keywords.push(s));
        self
    }
    pub fn reserve_footer_keywords<S: Into<String>, I: IntoIterator<Item = S>>(
        mut self,
        keywords: I,
    ) -> Self {
        keywords
            .into_iter()
            .map(S::into)
            .for_each(|s| self.user_reserved_footer_keywords.push(s));
        self
    }
}

/// Can be converted to an [`Import`].
pub trait IntoImport {
    fn into_import(self) -> Import;
}
impl IntoImport for Import {
    fn into_import(self) -> Import {
        self
    }
}
impl IntoImport for &str {
    fn into_import(self) -> Import {
        Import::schema(self)
    }
}
impl IntoImport for String {
    fn into_import(self) -> Import {
        Import::schema(self.as_str())
    }
}
impl<S: AsRef<str>> IntoImport for (S,) {
    fn into_import(self) -> Import {
        Import::schema(self.0.as_ref())
    }
}
impl<S: AsRef<str>, T: AsRef<str>> IntoImport for (S, T) {
    fn into_import(self) -> Import {
        Import::schema_type(self.0.as_ref(), self.1.as_ref(), None)
    }
}
impl<S: AsRef<str>, T: AsRef<str>, A: AsRef<str>> IntoImport for (S, T, A) {
    fn into_import(self) -> Import {
        Import::schema_type(self.0.as_ref(), self.1.as_ref(), Some(self.2.as_ref()))
    }
}

#[cfg(test)]
mod tests {
    use crate::model::bag::bag;
    use crate::model::{Import, SchemaHeader, SchemaHeaderBuilder};
    use crate::{ISL_1_0, ISL_2_0};

    #[test]
    fn test_builder_isl_1_0() {
        let header = SchemaHeaderBuilder::<ISL_1_0>::new()
            .import("widget.isl")
            .import(("foo.isl",))
            .import(("bar.isl", "TypeA"))
            .import(("baz.isl", "TypeB", "AliasA"))
            .build()
            .into_inner();

        assert_eq!(
            header,
            SchemaHeader {
                imports: bag![
                    Import::schema("widget.isl"),
                    Import::schema("foo.isl"),
                    Import::schema_type("bar.isl", "TypeA", None),
                    Import::schema_type("baz.isl", "TypeB", Some("AliasA")),
                ],
                user_reserved_header_keywords: bag![],
                user_reserved_type_keywords: bag![],
                user_reserved_footer_keywords: bag![],
                open_content: bag![],
            }
        )
    }

    #[test]
    fn test_builder_isl_2_0() {
        let header = SchemaHeaderBuilder::<ISL_2_0>::new()
            .import("widget.isl")
            .import(("foo.isl",))
            .import(("bar.isl", "TypeA"))
            .import(("baz.isl", "TypeB", "AliasA"))
            .reserve_type_keywords(["foo", "bar"])
            .reserve_type_keywords(["baz", "bat"])
            .build()
            .into_inner();

        assert_eq!(
            header,
            SchemaHeader {
                imports: bag![
                    Import::schema_type("baz.isl", "TypeB", Some("AliasA")),
                    Import::schema_type("bar.isl", "TypeA", None),
                    Import::schema("foo.isl"),
                    Import::schema("widget.isl"),
                ],
                user_reserved_header_keywords: bag![],
                user_reserved_type_keywords: bag![
                    "bat".to_string(),
                    "baz".to_string(),
                    "bar".to_string(),
                    "foo".to_string(),
                ],
                user_reserved_footer_keywords: bag![],
                open_content: bag![],
            }
        )
    }
}
