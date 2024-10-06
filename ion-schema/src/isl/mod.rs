//! Provides a way to construct ISL types/constraints programmatically.
//!
//! This module consists of three submodules that help constructing ISL types/constraint:
//!
//! * `isl_type` module represents a schema type [IslType] which converts given ion content in the schema file
//! into an ISL types(not-yet-resolved types). It stores `IslConstraint`s defined within the given type.
//!
//! * `isl_import` module represents a schema import [IslImport] which converts given ion content in the schema file
//! into an ISL import. It stores schema id, an optional type that needs to be imported and an optional alias to that type.
//!
//! * `isl_constraint` module represents schema constraints [IslConstraint]
//! which converts given ion content in the schema file into an ISL constraint(not-yet-resolved constraints).
//!
//! * `isl_type_reference` module provides a schema type reference.
//! The type reference grammar is defined in the [Ion Schema Specification]
//!
//! [IslConstraint]: isl_constraint::IslConstraint
//! [Ion Schema Specification]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#grammar
//!
//! ## Example usage of `isl` module to create an `IslSchema` using `IslType`:
//! ```
//! use ion_schema::isl::isl_constraint::v_1_0::{all_of, type_constraint};
//! use ion_schema::isl::isl_type::v_1_0::named_type;
//! use ion_schema::isl::isl_type_reference::v_1_0::{anonymous_type_ref, named_type_ref};
//! use ion_schema::isl::IslSchema;
//! use ion_schema::system::SchemaSystem;
//!
//! // below code represents an ISL type:
//! // type:: {
//! //      name:my_type_name,
//! //      type: int,
//! //      all_of: [
//! //          { type: bool }
//! //      ]
//! //  }
//! let isl_type = named_type(
//!     // represents the `name` of the defined type
//!     "my_type_name".to_owned(),
//!     vec![
//!         // represents the `type: int` constraint
//!         type_constraint(
//!             named_type_ref("int")
//!         ),
//!         // represents `all_of` with anonymous type `{ type: bool }` constraint
//!         all_of(
//!             vec![
//!                 anonymous_type_ref(
//!                     vec![
//!                         type_constraint(
//!                             named_type_ref("bool")
//!                         )
//!                     ]
//!                 )
//!             ]
//!         )
//!     ]
//! );
//!
//! // create an ISL schema using above IslType
//! let isl_schema = IslSchema::schema_v_1_0("my_schema", vec![], vec![isl_type.to_owned()], vec![], vec![]);
//!
//! // create a schema with resolved type references from given IslSchema using SchemaSystem
//! let mut schema_system = SchemaSystem::new(vec![]); // no authorities added
//! let schema = schema_system.load_schema_from_isl_schema_v1_0(isl_schema);
//!
//! assert!(schema.is_ok());
//!
//! // verify if the generated schema contains the correct type
//! assert!(schema.unwrap().get_type("my_type_name").is_some())
//! ```
//!
//!  ## Example of serializing a programmatically constructed schema into a schema file:
//! ```
//! use ion_rs::{Writer, WriteConfig, TextFormat, SequenceWriter};
//! use ion_rs::v1_0::Text;
//! use ion_schema::isl::{isl_type::v_1_0::*, isl_constraint::v_1_0::*, isl_type_reference::v_1_0::*, IslSchema};
//! use ion_schema::schema::Schema;
//! use ion_schema::system::SchemaSystem;
//!
//! let isl_type = named_type(
//!     // represents the `name` of the defined type
//!     "my_type_name".to_owned(),
//!     vec![
//!         // represents the `type: int` constraint
//!         type_constraint(
//!             named_type_ref("int")
//!         ),
//!         // represents `all_of` with anonymous type `{ type: bool }` constraint
//!         all_of(
//!             vec![
//!                 anonymous_type_ref(
//!                     vec![
//!                         type_constraint(
//!                             named_type_ref("bool")
//!                         )
//!                     ]
//!                 )
//!             ]
//!         )
//!     ]
//! );
//!
//! // create an ISL schema using above IslType
//! let isl_schema = IslSchema::schema_v_1_0("my_schema", vec![], vec![isl_type.to_owned()], vec![], vec![]);
//!
//! // initiate an Ion pretty text writer
//! let mut buffer = Vec::new();
//! let mut writer = Writer::new(Text, buffer).unwrap();
//!
//! // write the previously constructed ISL model into a schema file using `write_to`
//! let write_schema_result = writer.write_all(&isl_schema);
//! assert!(write_schema_result.is_ok());
//!
//! // The above written schema file looks like following:
//! // $ion_schema_1_0
//! // schema_header::{}
//! // type::{
//! //    name: my_type_name,
//! //   type: int,
//! //   all_of: [
//! //       {
//! //           type: bool
//! //       }
//! //   ]
//! // }
//! // schema_footer::{}
//! ```
//! Note that all the above functions to construct a type, constraint and type reference comes from `v_1_0` module which represents functions for [ISL 1.0](https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec).
//! In order to programmatically construct [ISL 2.0](https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec) types, constraints and type references use `v_2_0` module.

// The given schema is loaded with a two phase approach:
// 1. Phase 1: Constructing an internal representation of ISL types/constraints from given schema file.
//             This phase creates all [IslType],  [IslConstraint], [IslTypeRef] structs from the ion content in schema file.
// 2. Phase 2: Constructing resolved types/constraints from internal representation of ISL types/constraints(not-yet-resolved types/constraints).
//             This is done by loading all types into [Schema] as below:
//                 a. Convert all [IslType] → [TypeDefinition], [IslConstraint] → [Constraint], [IslTypeRef] → [TypeDefinition]
//                 b. While doing (a) store all [TypeDefinition] in the [TypeStore](which could help
//                    returning resolved types in a schema) and store generated [TypeId] in the constraint.

use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslType;
use crate::UserReservedFields;
use ion_rs::Element;
use ion_rs::{IonResult, SequenceWriter, StructWriter, ValueWriter, WriteAsIon};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub mod isl_constraint;
pub mod isl_import;
pub mod isl_type;
pub mod isl_type_reference;
pub mod ranges;
pub mod util;

/// Represents Ion Schema Language Versions
/// Currently it support v1.0 and v2.0
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum IslVersion {
    V1_0,
    V2_0,
}

impl WriteAsIon for IslVersion {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        let text = match self {
            IslVersion::V1_0 => "$ion_schema_1_0",
            IslVersion::V2_0 => "$ion_schema_2_0",
        };
        writer.write_symbol(text)
    }
}

impl Display for IslVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IslVersion::V1_0 => "ISL 1.0",
                IslVersion::V2_0 => "ISL 2.0",
            }
        )
    }
}

/// Models the content that could be in a Schema document.
#[derive(Debug, Clone, PartialEq)]
pub enum SchemaContent {
    Version(IslVersion),
    Header(SchemaHeader),
    Type(IslType),
    Footer(SchemaFooter),
    OpenContent(Element),
}
impl From<IslVersion> for SchemaContent {
    fn from(value: IslVersion) -> Self {
        SchemaContent::Version(value)
    }
}
impl From<SchemaHeader> for SchemaContent {
    fn from(value: SchemaHeader) -> Self {
        SchemaContent::Header(value)
    }
}
impl From<IslType> for SchemaContent {
    fn from(value: IslType) -> Self {
        SchemaContent::Type(value)
    }
}
impl From<SchemaFooter> for SchemaContent {
    fn from(value: SchemaFooter) -> Self {
        SchemaContent::Footer(value)
    }
}

impl SchemaContent {
    fn as_header(&self) -> Option<&SchemaHeader> {
        if let SchemaContent::Header(schema_header) = self {
            Some(schema_header)
        } else {
            None
        }
    }
    fn as_type(&self) -> Option<&IslType> {
        if let SchemaContent::Type(value) = self {
            Some(value)
        } else {
            None
        }
    }
    fn as_open_content(&self) -> Option<&Element> {
        if let SchemaContent::OpenContent(value) = self {
            Some(value)
        } else {
            None
        }
    }
    fn expect_header(&self) -> &SchemaHeader {
        if let SchemaContent::Header(value) = self {
            value
        } else {
            panic!("illegal state encountered; this should be unreachable. Expected to find a Header, but found: {:?}", self)
        }
    }
    fn expect_isl_version(&self) -> &IslVersion {
        if let SchemaContent::Version(value) = self {
            value
        } else {
            panic!("illegal state encountered; this should be unreachable. Expected to find a Version, but found: {:?}", self)
        }
    }
    fn expect_type(&self) -> &IslType {
        if let SchemaContent::Type(value) = self {
            value
        } else {
            panic!("illegal state encountered; this should be unreachable. Expected to find a Type, but found: {:?}", self)
        }
    }
    fn expect_footer(&self) -> &SchemaFooter {
        if let SchemaContent::Footer(value) = self {
            value
        } else {
            panic!("illegal state encountered; this should be unreachable. Expected to find a Footer, but found: {:?}", self)
        }
    }
}

impl WriteAsIon for SchemaContent {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        match self {
            SchemaContent::Version(value) => writer.write(value),
            SchemaContent::Header(value) => writer.write(value),
            SchemaContent::Type(value) => writer.write(value),
            SchemaContent::Footer(value) => writer.write(value),
            SchemaContent::OpenContent(value) => writer.write(value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
struct SchemaHeader {
    /// Represents the user defined reserved fields
    user_reserved_fields: UserReservedFields,
    /// Represents all the IslImports inside the schema file.
    /// For more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports
    imports: Vec<IslImport>,
    /// User-defined (aka "open") content
    user_content: Vec<(String, Element)>,
}

impl WriteAsIon for SchemaHeader {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        let mut struct_writer = writer
            .with_annotations(["schema_header"])?
            .struct_writer()?;
        if !self.imports.is_empty() {
            struct_writer.field_writer("imports").write(&self.imports)?;
        }
        if !self.user_reserved_fields.is_empty() {
            struct_writer
                .field_writer("user_reserved_fields")
                .write(&self.user_reserved_fields)?;
        }
        if !self.user_content.is_empty() {
            for (k, v) in &self.user_content {
                struct_writer.write(k.as_str(), v)?;
            }
        }
        struct_writer.close()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
struct SchemaFooter {
    /// User-defined (aka "open") content
    user_content: Vec<(String, Element)>,
}

impl WriteAsIon for SchemaFooter {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        let mut struct_writer = writer
            .with_annotations(["schema_footer"])?
            .struct_writer()?;
        if !self.user_content.is_empty() {
            for (k, v) in &self.user_content {
                struct_writer.write(k.as_str(), v)?;
            }
        }
        struct_writer.close()
    }
}

/// Provides an internal representation of a schema file
#[derive(Debug, Clone, PartialEq)]
pub struct IslSchema {
    /// Represents an id for the given ISL model
    id: String,
    /// Content of the schema document.
    schema_content: Vec<SchemaContent>,
    /// Position of the header within [schema_content].
    header_index: Option<usize>,
    /// Position of the footer within [schema_content].
    footer_index: Option<usize>,
    /// Position of all types defined in this schema file.
    types_by_name: HashMap<String, usize>,

    /// Represents all the inline IslImportTypes in this schema file.
    inline_imported_types: Vec<IslImportType>,
}

impl IslSchema {
    fn new_from_content_vec(
        id: String,
        schema_content: Vec<SchemaContent>,
        inline_imported_types: Vec<IslImportType>,
    ) -> Self {
        assert!(matches!(schema_content[0], SchemaContent::Version(_)));
        let header_index = schema_content
            .iter()
            .position(|x| matches!(x, SchemaContent::Header(_)));
        let footer_index = schema_content
            .iter()
            .position(|x| matches!(x, SchemaContent::Footer(_)));
        // TODO: Ensure that ISL 1.0 schemas have footer iff header
        let types_by_name: HashMap<String, usize> = schema_content
            .iter()
            .enumerate()
            .filter(|&(_, x)| matches!(x, SchemaContent::Type(_)))
            .map(|(i, t)| (t.expect_type().name().unwrap().to_string(), i))
            .collect();

        Self {
            id,
            schema_content,
            header_index,
            footer_index,
            types_by_name,
            inline_imported_types,
        }
    }

    /// TODO: Replace with a builder
    pub(crate) fn new<A: AsRef<str>>(
        id: A,
        version: IslVersion,
        user_reserved_fields: Option<UserReservedFields>,
        imports: Vec<IslImport>,
        types: Vec<IslType>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> Self {
        let mut schema_content: Vec<SchemaContent> = vec![];
        schema_content.push(version.into());
        schema_content.push(
            SchemaHeader {
                user_reserved_fields: user_reserved_fields.unwrap_or_default(),
                imports,
                user_content: vec![],
            }
            .into(),
        );
        for t in types {
            schema_content.push(t.into());
        }
        for e in open_content {
            schema_content.push(SchemaContent::OpenContent(e));
        }
        Self::new_from_content_vec(id.as_ref().to_string(), schema_content, inline_imports)
    }

    /// Creates an ISL schema using the [IslType]s, [IslImport]s, open content and schema id
    /// TODO: Replace with a builder
    pub fn schema_v_1_0<A: AsRef<str>>(
        id: A,
        imports: Vec<IslImport>,
        types: Vec<IslType>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> IslSchema {
        IslSchema::new(
            id.as_ref(),
            IslVersion::V1_0,
            None,
            imports,
            types,
            inline_imports,
            open_content,
        )
    }

    /// Creates an ISL schema using the [IslType]s, [IslImport]s, [UserReservedFields] open content and schema id
    /// TODO: Replace with a builder
    pub fn schema_v_2_0<A: AsRef<str>>(
        id: A,
        user_reserved_fields: UserReservedFields,
        imports: Vec<IslImport>,
        types: Vec<IslType>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> IslSchema {
        IslSchema::new(
            id.as_ref(),
            IslVersion::V2_0,
            Some(user_reserved_fields),
            imports,
            types,
            inline_imports,
            open_content,
        )
    }

    pub fn id(&self) -> String {
        self.id.to_owned()
    }

    pub fn version(&self) -> IslVersion {
        *(self.schema_content[0].expect_isl_version())
    }

    fn header(&self) -> Option<&SchemaHeader> {
        self.header_index
            .map(|i| self.schema_content.get(i).unwrap())
            .map(SchemaContent::expect_header)
    }

    fn footer(&self) -> Option<&SchemaFooter> {
        self.footer_index
            .map(|i| self.schema_content.get(i).unwrap())
            .map(SchemaContent::expect_footer)
    }

    const EMPTY_VEC: Vec<IslImport> = vec![];
    const EMPTY_VEC_REF: &'static Vec<IslImport> = &Self::EMPTY_VEC;

    pub fn imports(&self) -> impl Iterator<Item = &IslImport> {
        self.header()
            .map(|x| &x.imports)
            .unwrap_or(Self::EMPTY_VEC_REF)
            .iter()
    }

    pub fn types(&self) -> impl Iterator<Item = &IslType> {
        self.schema_content.iter().filter_map(|x| x.as_type())
    }

    pub fn inline_imported_types(&self) -> impl Iterator<Item = &IslImportType> {
        self.inline_imported_types.iter()
    }

    /// Provides top level open content for given schema
    /// For open content defined within type definitions use IslType#open_content()
    pub fn open_content(&self) -> impl Iterator<Item = &Element> {
        self.schema_content
            .iter()
            .filter_map(|x| x.as_open_content())
    }

    /// Provide user reserved field defined in the given schema for ISL 2.0,
    /// Otherwise returns None
    pub fn user_reserved_fields(&self) -> Option<&UserReservedFields> {
        self.header().map(|x| &x.user_reserved_fields)
    }
}

impl IslSchema {
    fn write_as_ion<W: SequenceWriter>(&self, writer: &mut W) -> IonResult<()> {
        for item in &self.schema_content {
            writer.write(item)?;
        }
        Ok(())
    }
}

impl<'a> IntoIterator for &'a IslSchema {
    type Item = &'a SchemaContent;
    type IntoIter = SchemaIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        SchemaIterator {
            content: &self.schema_content,
            i: 0,
        }
    }
}

pub struct SchemaIterator<'a> {
    content: &'a Vec<SchemaContent>,
    i: usize,
}

impl<'a> Iterator for SchemaIterator<'a> {
    type Item = &'a SchemaContent;

    fn next(&mut self) -> Option<Self::Item> {
        let next_item = self.content.get(self.i);
        self.i += 1;
        next_item
    }
}

#[cfg(test)]
mod isl_tests {
    use crate::authority::FileSystemDocumentAuthority;
    use crate::ion_extension::ElementExtensions;
    use crate::isl::isl_constraint::v_1_0::*;
    use crate::isl::isl_type::v_1_0::load_isl_type as load_isl_type_def;
    use crate::isl::isl_type::v_1_0::*;
    use crate::isl::isl_type::v_2_0::load_isl_type as load_isl_type_def_v2_0;
    use crate::isl::isl_type::IslType;
    use crate::isl::isl_type_reference::v_1_0::*;
    use crate::isl::ranges::*;
    use crate::isl::util::Ieee754InterchangeFormat;
    use crate::isl::util::TimestampPrecision;
    use crate::isl::util::ValidValue;
    use crate::isl::*;
    use crate::result::IonSchemaResult;
    use crate::system::SchemaSystem;
    use ion_rs::v1_0;
    use ion_rs::Decimal;
    use ion_rs::IonType;
    use ion_rs::Symbol;
    use ion_rs::{Element, TextFormat, WriteConfig, Writer};
    use rstest::*;
    use std::path::Path;
    use test_generator::test_resources;

    // helper function to create AnonymousIslType for isl tests using ISL 1.0
    fn load_isl_type(text: &str) -> IslType {
        load_isl_type_def(text.as_bytes()).unwrap()
    }

    // helper function to create AnonymousIslType for isl tests using ISL 2.0
    fn load_isl_type_v2_0(text: &str) -> IslType {
        load_isl_type_def_v2_0(text.as_bytes()).unwrap()
    }

    #[test]
    fn test_open_content_for_type_def() -> IonSchemaResult<()> {
        let type_def = load_isl_type(
            r#" 
                // type definition with open content
                type:: { 
                    name: my_int, 
                    type: int,
                    unknown_constraint: "this is an open content field value"
                }
            "#,
        );

        assert_eq!(
            type_def.open_content(),
            vec![(
                "unknown_constraint".to_owned(),
                Element::read_one(r#""this is an open content field value""#.as_bytes())?
            )]
        );
        Ok(())
    }

    #[rstest(
    isl_type1,isl_type2,
    case::type_constraint_with_anonymous_type(
        load_isl_type(r#" // For a schema with single anonymous type
                {type: any}
            "#),
        anonymous_type([type_constraint(named_type_ref("any"))])
    ),
    case::type_constraint_with_nullable_annotation(
        load_isl_type(r#" // For a schema with `nullable` annotation`
                {type: nullable::int}
            "#),
        anonymous_type([type_constraint(nullable_built_in_type_ref(IonType::Int))])
    ),
    case::type_constraint_with_null_or_annotation(
        load_isl_type_v2_0(r#" // For a schema with `$null_or` annotation`
                    {type: $null_or::int}
                "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::type_constraint(isl_type_reference::v_2_0::null_or_named_type_ref("int"))])
    ),
    case::type_constraint_with_named_type(
        load_isl_type(r#" // For a schema with named type
                type:: { name: my_int, type: int }
            "#),
        named_type("my_int", [type_constraint(named_type_ref("int"))])
    ),
    case::type_constraint_with_named_nullable_type(
        load_isl_type(r#" // For a schema with named type
                type:: { name: my_nullable_int, type: $int }
            "#),
        named_type("my_nullable_int", [type_constraint(named_type_ref("$int"))])
    ),
    case::type_constraint_with_self_reference_type(
        load_isl_type(r#" // For a schema with self reference type
                type:: { name: my_int, type: my_int }
            "#),
        named_type("my_int", [type_constraint(named_type_ref("my_int"))])
    ),
    case::type_constraint_with_nested_self_reference_type(
        load_isl_type(r#" // For a schema with nested self reference type
                type:: { name: my_int, type: { type: my_int } }
            "#),
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("my_int"))]))])
    ),
    case::type_constraint_with_nested_type(
        load_isl_type(r#" // For a schema with nested types
                type:: { name: my_int, type: { type: int } }
            "#),
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("int"))]))])
    ),
    case::type_constraint_with_nested_multiple_types(
        load_isl_type(r#" // For a schema with nested multiple types
                type:: { name: my_int, type: { type: int }, type: { type: my_int } }
            "#),
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("int"))])), type_constraint(anonymous_type_ref([type_constraint(named_type_ref("my_int"))]))])
    ),
    case::all_of_constraint(
        load_isl_type(r#" // For a schema with all_of type as below:
                { all_of: [{ type: int }] }
            "#),
        anonymous_type([all_of([anonymous_type_ref([type_constraint(named_type_ref("int"))])])])
    ),
    case::any_of_constraint(
        load_isl_type(r#" // For a schema with any_of constraint as below:
                    { any_of: [{ type: int }, { type: decimal }] }
                "#),
        anonymous_type([any_of([anonymous_type_ref([type_constraint(named_type_ref("int"))]), anonymous_type_ref([type_constraint(named_type_ref("decimal"))])])])
    ),
    case::one_of_constraint(
        load_isl_type(r#" // For a schema with one_of constraint as below:
                    { one_of: [{ type: int }, { type: decimal }] }
                "#),
        anonymous_type([one_of([anonymous_type_ref([type_constraint(named_type_ref("int"))]), anonymous_type_ref([type_constraint(named_type_ref("decimal"))])])])
    ),
    case::not_constraint(
        load_isl_type(r#" // For a schema with not constraint as below:
                    { not: { type: int } }
                "#),
        anonymous_type([not(anonymous_type_ref([type_constraint(named_type_ref("int"))]))])
    ),
    case::ordered_elements_constraint(
        load_isl_type(r#" // For a schema with ordered_elements constraint as below:
                    { ordered_elements: [  symbol, { type: int },  ] }
                "#),
        anonymous_type([ordered_elements([variably_occurring_type_ref(named_type_ref("symbol"), UsizeRange::new_single_value(1)), variably_occurring_type_ref(anonymous_type_ref([type_constraint(named_type_ref("int"))]), UsizeRange::new_single_value(1))])])
    ),
    case::closed_fields_constraint(
        load_isl_type_v2_0(r#" // For a schema with fields constraint as below:
                    { fields: closed::{ name: string, id: int} }
                "#),
        anonymous_type([isl_constraint::v_2_0::fields(vec![("name".to_owned(), variably_occurring_type_ref(named_type_ref("string"), UsizeRange::zero_or_one())), ("id".to_owned(), variably_occurring_type_ref(named_type_ref("int"), UsizeRange::zero_or_one()))].into_iter(), true)]),
    ),
        case::fields_constraint(
            load_isl_type(r#" // For a schema with fields constraint as below:
                    { fields: { name: string, id: int} }
                "#),
            anonymous_type([fields(vec![("name".to_owned(), variably_occurring_type_ref(named_type_ref("string"), UsizeRange::zero_or_one())), ("id".to_owned(), variably_occurring_type_ref(named_type_ref("int"), UsizeRange::zero_or_one()))].into_iter())]),
        ),
    case::field_names_constraint(
        load_isl_type_v2_0(r#" // For a schema with field_names constraint as below:
                        { field_names: distinct::symbol }
                    "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::field_names(isl_type_reference::v_2_0::named_type_ref("symbol"), true)]),
    ),
    case::contains_constraint(
        load_isl_type(r#" // For a schema with contains constraint as below:
                    { contains: [true, 1, "hello"] }
                "#),
        anonymous_type([contains([true.into(), 1.into(), "hello".to_owned().into()])])
    ),
    case::container_length_constraint(
        load_isl_type(r#" // For a schema with container_length constraint as below:
                    { container_length: 3 }
                "#),
        anonymous_type([container_length(3.into())])
    ),
    case::byte_length_constraint(
        load_isl_type(r#" // For a schema with byte_length constraint as below:
                    { byte_length: 3 }
                "#),
        anonymous_type([byte_length(3.into())])
    ),
    case::codepoint_length_constraint(
        load_isl_type(r#" // For a schema with codepoint_length constraint as below:
                    { codepoint_length: 3 }
                "#),
        anonymous_type([codepoint_length(3.into())])
    ),
    case::element_constraint(
        load_isl_type(r#" // For a schema with element constraint as below:
                    { element: int }
                "#),
        anonymous_type([element(named_type_ref("int"))])
    ),
    case::distinct_element_constraint(
        load_isl_type_v2_0(r#" // For a schema with distinct element constraint as below:
                        { element: distinct::int }
                    "#),
    isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::element(named_type_ref("int"), true)])
    ),
    case::annotations_constraint(
        load_isl_type(r#" // For a schema with annotations constraint as below:
                        { annotations: closed::[red, blue, green] }
                    "#),
        anonymous_type([annotations(vec!["closed"], vec![Symbol::from("red").into(), Symbol::from("blue").into(), Symbol::from("green").into()])])
    ),
    case::standard_syantx_annotations_constraint(
        load_isl_type_v2_0(r#" // For a schema with annotations constraint as below:
                            { annotations: { container_length: 1 } }
                        "#),
    isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::annotations(isl_type_reference::v_2_0::anonymous_type_ref([isl_constraint::v_2_0::container_length(1.into())]))])
    ),
    case::precision_constraint(
        load_isl_type(r#" // For a schema with precision constraint as below:
                        { precision: 2 }
                    "#),
        anonymous_type([precision(2.into())])
    ),
    case::scale_constraint(
        load_isl_type(r#" // For a schema with scale constraint as below:
                        { scale: 2 }
                    "#),
        anonymous_type([scale(2.into())])
    ),
    case::exponent_constraint(
        load_isl_type_v2_0(r#" // For a schema with exponent constraint as below:
                        { exponent: 2 }
                    "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::exponent(2.into())])
    ),
    case::timestamp_precision_constraint(
        load_isl_type(r#" // For a schema with timestamp_precision constraint as below:
                            { timestamp_precision: year }
                        "#),
        anonymous_type([timestamp_precision(TimestampPrecisionRange::new_single_value(TimestampPrecision::Year))])
    ),
    case::valid_values_constraint(
        load_isl_type(r#" // For a schema with valid_values constraint as below:
                        { valid_values: [2, 3.5, 5e7, "hello", hi] }
                    "#),
        anonymous_type([valid_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), Symbol::from("hi").into()]).unwrap()])
    ),
    case::valid_values_with_range_constraint(
        load_isl_type(r#" // For a schema with valid_values constraint as below:
                        { valid_values: range::[1, 5.5] }
                    "#),
        anonymous_type(
                [valid_values(vec![
                    ValidValue::NumberRange(
                        NumberRange::new_inclusive(
                            1.into(),
                            Decimal::new(55, -1)
                        ).unwrap()
                    )
                ]).unwrap()]
            )
        ),
    case::utf8_byte_length_constraint(
        load_isl_type(r#" // For a schema with utf8_byte_length constraint as below:
                        { utf8_byte_length: 3 }
                    "#),
        anonymous_type([utf8_byte_length(3.into())])
    ),
    case::regex_constraint(
        load_isl_type(r#" // For a schema with regex constraint as below:
                            { regex: "[abc]" }
                        "#),
        anonymous_type(
            [
                regex(
                    false, // case insensitive
                    false, // multiline
                    "[abc]".to_string()
                )
            ]
        )
    ),
    case::timestamp_offset_constraint(
        load_isl_type(r#" // For a schema with timestamp_offset constraint as below:
                            { timestamp_offset: ["+00:00"] }
                        "#),
        anonymous_type(
            [timestamp_offset(vec!["+00:00".try_into().unwrap()])]
        )
    ),
    case::ieee754_float_constraint(
        load_isl_type_v2_0(r#" // For a schema with ieee754_float constraint as below:
                            { ieee754_float: binary16 }
                        "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::ieee754_float(Ieee754InterchangeFormat::Binary16)])
    ),
    )]
    fn owned_struct_to_isl_type(isl_type1: IslType, isl_type2: IslType) {
        // assert if both the IslType are same in terms of constraints and name
        assert_eq!(isl_type1, isl_type2);
    }

    // helper function to create a timestamp precision range
    fn load_timestamp_precision_range(text: &str) -> IonSchemaResult<TimestampPrecisionRange> {
        TimestampPrecisionRange::from_ion_element(
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            |e| {
                let symbol_text = e.as_symbol().and_then(Symbol::text)?;
                TimestampPrecision::try_from(symbol_text).ok()
            },
        )
    }

    // helper function to create a number range
    fn load_number_range(text: &str) -> IonSchemaResult<NumberRange> {
        NumberRange::from_ion_element(
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            Element::any_number_as_decimal,
        )
    }

    // helper function to return Elements for range `contains` tests
    fn elements<T: Into<Element> + std::clone::Clone>(values: &[T]) -> Vec<Element> {
        values.iter().cloned().map(|v| v.into()).collect()
    }

    const SKIP_LIST: [&str; 5] = [
        "ion-schema-schemas/json/json.isl", // the file contains `nan` which fails on equivalence for two schemas
        "ion-schema-tests/ion_schema_1_0/nullable.isl", // Needs `nullable` annotation related fixes
        // following skip list files are related to order of types in the schema file
        // https://github.com/amazon-ion/ion-schema-rust/issues/184
        "ion-schema-tests/ion_schema_1_0/schema/import/import_inline.isl",
        "ion-schema-tests/ion_schema_2_0/imports/tree/inline_import_a.isl",
        "ion-schema-tests/ion_schema_2_0/imports/tree/inline_import_c.isl",
    ];

    fn is_skip_list_path(file_name: &str) -> bool {
        SKIP_LIST
            .iter()
            .map(|p| p.replace('/', std::path::MAIN_SEPARATOR_STR))
            .any(|p| p == file_name)
    }

    #[test_resources("ion-schema-tests/**/*.isl")]
    #[test_resources("ion-schema-schemas/**/*.isl")]
    fn test_write_to_isl(file_name: &str) -> IonSchemaResult<()> {
        if is_skip_list_path(file_name) {
            return Ok(());
        }

        // creates a schema system that will be sued to load schema files into a schema model
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(FileSystemDocumentAuthority::new(
                Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap(),
            ))]);

        // construct an ISL model from a schema file
        let expected_schema = schema_system.load_isl_schema(file_name)?;

        // initiate an Ion pretty text writer
        let buffer = Vec::new();
        let config = WriteConfig::<v1_0::Text>::new(TextFormat::Pretty);
        let mut writer = Writer::new(config, buffer)?;

        // write the previously constructed ISL model into a schema file using `write_to`
        let write_schema_result = expected_schema.write_as_ion(&mut writer);
        assert!(write_schema_result.is_ok());

        let output = writer.close()?;

        // create a new schema model from the schema file generated by the previous step
        let actual_schema = schema_system.new_isl_schema(output.as_slice(), file_name)?;

        // compare the actual schema model that was generated from dynamically created schema file
        // with the expected schema model that was constructed from given schema file
        assert_eq!(actual_schema, expected_schema);
        Ok(())
    }
}
