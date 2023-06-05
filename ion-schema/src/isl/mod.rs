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
//! use ion_schema::isl::{isl_type::v_1_0::*, isl_constraint::v_1_0::*, isl_type_reference::v_1_0::*, IslSchema};
//! use ion_schema::schema::Schema;
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
use ion_rs::element::Element;
use std::fmt::{Display, Formatter};

pub mod isl_constraint;
pub mod isl_import;
pub mod isl_range;
pub mod isl_type;
pub mod isl_type_reference;
pub mod util;

/// Represents Ion Schema Language Versions
/// Currently it support v1.0 and v2.0
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum IslVersion {
    V1_0,
    V2_0,
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

/// Provides an internal representation of an schema file
#[derive(Debug, Clone, PartialEq)]
pub struct IslSchema {
    pub(crate) schema: IslSchemaImpl,
}

impl IslSchema {
    /// Creates an ISL schema using the [IslType]s, [IslImport]s, open content and schema id
    pub fn schema_v_1_0<A: AsRef<str>>(
        id: A,
        imports: Vec<IslImport>,
        types: Vec<IslType>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> IslSchema {
        IslSchema {
            schema: IslSchemaImpl::new(
                id.as_ref(),
                IslVersion::V1_0,
                None,
                imports,
                types,
                inline_imports,
                open_content,
            ),
        }
    }

    /// Creates an ISL schema using the [IslType]s, [IslImport]s, [UserReservedFields] open content and schema id
    pub fn schema_v_2_0<A: AsRef<str>>(
        id: A,
        user_reserved_fields: UserReservedFields,
        imports: Vec<IslImport>,
        types: Vec<IslType>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> IslSchema {
        IslSchema {
            schema: IslSchemaImpl::new(
                id.as_ref(),
                IslVersion::V2_0,
                Some(user_reserved_fields),
                imports,
                types,
                inline_imports,
                open_content,
            ),
        }
    }

    pub fn id(&self) -> String {
        self.schema.id.to_owned()
    }

    pub fn version(&self) -> IslVersion {
        self.schema.version
    }

    pub fn imports(&self) -> &[IslImport] {
        &self.schema.imports
    }

    pub fn types(&self) -> &[IslType] {
        &self.schema.types
    }

    pub fn inline_imported_types(&self) -> &[IslImportType] {
        &self.schema.inline_imported_types
    }

    /// Provides top level open content for given schema
    /// For open content defined within type definitions use IslType#open_content()
    pub fn open_content(&self) -> &Vec<Element> {
        &self.schema.open_content
    }

    /// Provide user reserved field defined in the given schema for ISL 2.0,
    /// Otherwise returns None
    pub fn user_reserved_fields(&self) -> Option<&UserReservedFields> {
        self.schema.user_reserved_fields.as_ref()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IslSchemaImpl {
    /// Represents an id for the given ISL model
    id: String,
    /// Represents the ISL version for given schema
    version: IslVersion,
    /// Represents the user defined reserved fields
    /// For ISL 2.0 this contains the use reserved fields that are defined within schema header,
    /// Otherwise, it is None.
    user_reserved_fields: Option<UserReservedFields>,
    /// Represents all the IslImports inside the schema file.
    /// For more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports
    imports: Vec<IslImport>,
    /// Represents all the IslType defined in this schema file.
    /// For more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type-definitions
    types: Vec<IslType>,
    /// Represents all the inline IslImportTypes in this schema file.
    inline_imported_types: Vec<IslImportType>,
    /// Represents open content as `Element`s
    /// Note: This doesn't preserve the information about where the open content is placed in the schema file.
    /// e.g. This method doesn't differentiate between following schemas with open content:
    /// ```ion
    /// $foo
    /// $bar
    /// type::{ name: foo, codepoint_length: 1 }
    /// ```
    ///
    /// ```ion
    /// type::{ name: foo, codepoint_length: 1 }
    /// $foo
    /// $bar
    /// ```
    open_content: Vec<Element>,
}

impl IslSchemaImpl {
    pub fn new<A: AsRef<str>>(
        id: A,
        version: IslVersion,
        user_reserved_fields: Option<UserReservedFields>,
        imports: Vec<IslImport>,
        types: Vec<IslType>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> Self {
        Self {
            id: id.as_ref().to_owned(),
            version,
            user_reserved_fields,
            imports,
            types,
            inline_imported_types: inline_imports,
            open_content,
        }
    }
}

#[cfg(test)]
mod isl_tests {
    use crate::isl::isl_constraint::v_1_0::*;
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_range::DecimalRange;
    use crate::isl::isl_range::FloatRange;
    use crate::isl::isl_range::IntegerRange;
    use crate::isl::isl_range::Number;
    use crate::isl::isl_range::NumberRange;
    use crate::isl::isl_range::TimestampPrecisionRange;
    use crate::isl::isl_range::TimestampRange;
    use crate::isl::isl_range::{Range, RangeBoundaryValue, RangeType};
    use crate::isl::isl_type::v_1_0::*;
    use crate::isl::isl_type::{IslType, IslTypeImpl};
    use crate::isl::isl_type_reference::v_1_0::*;
    use crate::isl::util::Ieee754InterchangeFormat;
    use crate::isl::util::TimestampPrecision;
    use crate::isl::IslVersion;
    use crate::isl::*;
    use crate::result::IonSchemaResult;
    use ion_rs::element::Element;
    use ion_rs::types::decimal::*;
    use ion_rs::types::integer::Int as IntegerValue;
    use ion_rs::types::timestamp::Timestamp;
    use ion_rs::IonType;
    use ion_rs::Symbol;
    use rstest::*;
    use std::io::Write;

    // helper function to create NamedIslType for isl tests using ISL 1.0
    fn load_named_type(text: &str) -> IslType {
        let type_def = IslTypeImpl::from_owned_element(
            IslVersion::V1_0,
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            &mut vec![],
        )
        .unwrap();
        let constraints = type_def
            .constraints()
            .iter()
            .map(|c| IslConstraint::new(IslVersion::V1_0, c.to_owned()))
            .collect();
        IslType::new(type_def, constraints)
    }

    // helper function to create AnonymousIslType for isl tests using ISL 1.0
    fn load_anonymous_type(text: &str) -> IslType {
        let type_def = IslTypeImpl::from_owned_element(
            IslVersion::V1_0,
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            &mut vec![],
        )
        .unwrap();
        let constraints = type_def
            .constraints()
            .iter()
            .map(|c| IslConstraint::new(IslVersion::V1_0, c.to_owned()))
            .collect();
        IslType::new(type_def, constraints)
    }

    // helper function to create AnonymousIslType for isl tests using ISL 2.0
    fn load_anonymous_type_v2_0(text: &str) -> IslType {
        let type_def = IslTypeImpl::from_owned_element(
            IslVersion::V2_0,
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            &mut vec![],
        )
        .unwrap();
        let constraints = type_def
            .constraints()
            .iter()
            .map(|c| IslConstraint::new(IslVersion::V2_0, c.to_owned()))
            .collect();
        IslType::new(type_def, constraints)
    }

    #[test]
    fn test_open_content_for_type_def() -> IonSchemaResult<()> {
        let type_def = load_named_type(
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
        load_anonymous_type(r#" // For a schema with single anonymous type
                {type: any}
            "#),
        anonymous_type([type_constraint(named_type_ref("any"))])
    ),
    case::type_constraint_with_nullable_annotation(
        load_anonymous_type(r#" // For a schema with `nullable` annotation`
                {type: nullable::int}
            "#),
        anonymous_type([type_constraint(nullable_built_in_type_ref(IonType::Int))])
    ),
    case::type_constraint_with_null_or_annotation(
        load_anonymous_type_v2_0(r#" // For a schema with `$null_or` annotation`
                    {type: $null_or::int}
                "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::type_constraint(isl_type_reference::v_2_0::null_or_named_type_ref("int"))])
    ),
    case::type_constraint_with_named_type(
        load_named_type(r#" // For a schema with named type
                type:: { name: my_int, type: int }
            "#),
        named_type("my_int", [type_constraint(named_type_ref("int"))])
    ),
    case::type_constraint_with_named_nullable_type(
        load_named_type(r#" // For a schema with named type
                type:: { name: my_nullable_int, type: $int }
            "#),
        named_type("my_nullable_int", [type_constraint(named_type_ref("$int"))])
    ),
    case::type_constraint_with_self_reference_type(
        load_named_type(r#" // For a schema with self reference type
                type:: { name: my_int, type: my_int }
            "#),
        named_type("my_int", [type_constraint(named_type_ref("my_int"))])
    ),
    case::type_constraint_with_nested_self_reference_type(
        load_named_type(r#" // For a schema with nested self reference type
                type:: { name: my_int, type: { type: my_int } }
            "#),
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("my_int"))]))])
    ),
    case::type_constraint_with_nested_type(
        load_named_type(r#" // For a schema with nested types
                type:: { name: my_int, type: { type: int } }
            "#),
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("int"))]))])
    ),
    case::type_constraint_with_nested_multiple_types(
        load_named_type(r#" // For a schema with nested multiple types
                type:: { name: my_int, type: { type: int }, type: { type: my_int } }
            "#),
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("int"))])), type_constraint(anonymous_type_ref([type_constraint(named_type_ref("my_int"))]))])
    ),
    case::all_of_constraint(
        load_anonymous_type(r#" // For a schema with all_of type as below:
                { all_of: [{ type: int }] }
            "#),
        anonymous_type([all_of([anonymous_type_ref([type_constraint(named_type_ref("int"))])])])
    ),
    case::any_of_constraint(
        load_anonymous_type(r#" // For a schema with any_of constraint as below:
                    { any_of: [{ type: int }, { type: decimal }] }
                "#),
        anonymous_type([any_of([anonymous_type_ref([type_constraint(named_type_ref("int"))]), anonymous_type_ref([type_constraint(named_type_ref("decimal"))])])])
    ),
    case::one_of_constraint(
        load_anonymous_type(r#" // For a schema with one_of constraint as below:
                    { one_of: [{ type: int }, { type: decimal }] }
                "#),
        anonymous_type([one_of([anonymous_type_ref([type_constraint(named_type_ref("int"))]), anonymous_type_ref([type_constraint(named_type_ref("decimal"))])])])
    ),
    case::not_constraint(
        load_anonymous_type(r#" // For a schema with not constraint as below:
                    { not: { type: int } }
                "#),
        anonymous_type([not(anonymous_type_ref([type_constraint(named_type_ref("int"))]))])
    ),
    case::ordered_elements_constraint(
        load_anonymous_type(r#" // For a schema with ordered_elements constraint as below:
                    { ordered_elements: [  symbol, { type: int },  ] }
                "#),
        anonymous_type([ordered_elements([variably_occurring_type_ref(named_type_ref("symbol"), Range::required()), variably_occurring_type_ref(anonymous_type_ref([type_constraint(named_type_ref("int"))]), Range::required())])])
    ),
    case::fields_constraint(
        load_anonymous_type(r#" // For a schema with fields constraint as below:
                    { fields: { name: string, id: int} }
                "#),
        anonymous_type([fields(vec![("name".to_owned(), variably_occurring_type_ref(named_type_ref("string"), Range::optional())), ("id".to_owned(), variably_occurring_type_ref(named_type_ref("int"), Range::optional()))].into_iter())]),
    ),
    case::field_names_constraint(
        load_anonymous_type_v2_0(r#" // For a schema with field_names constraint as below:
                        { field_names: distinct::symbol }
                    "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::field_names(isl_type_reference::v_2_0::named_type_ref("symbol"), true)]),
    ),
    case::contains_constraint(
        load_anonymous_type(r#" // For a schema with contains constraint as below:
                    { contains: [true, 1, "hello"] }
                "#),
        anonymous_type([contains([true.into(), 1.into(), "hello".to_owned().into()])])
    ),
    case::container_length_constraint(
        load_anonymous_type(r#" // For a schema with container_length constraint as below:
                    { container_length: 3 }
                "#),
        anonymous_type([container_length(3.into())])
    ),
    case::byte_length_constraint(
        load_anonymous_type(r#" // For a schema with byte_length constraint as below:
                    { byte_length: 3 }
                "#),
        anonymous_type([byte_length(3.into())])
    ),
    case::codepoint_length_constraint(
        load_anonymous_type(r#" // For a schema with codepoint_length constraint as below:
                    { codepoint_length: 3 }
                "#),
        anonymous_type([codepoint_length(3.into())])
    ),
    case::element_constraint(
        load_anonymous_type(r#" // For a schema with element constraint as below:
                    { element: int }
                "#),
        anonymous_type([element(named_type_ref("int"))])
    ),
    case::distinct_element_constraint(
        load_anonymous_type_v2_0(r#" // For a schema with distinct element constraint as below:
                        { element: distinct::int }
                    "#),
    isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::element(named_type_ref("int"), true)])
    ),
    case::annotations_constraint(
        load_anonymous_type(r#" // For a schema with annotations constraint as below:
                        { annotations: closed::[red, blue, green] }
                    "#),
        anonymous_type([annotations(vec!["closed"], vec![Symbol::from("red").into(), Symbol::from("blue").into(), Symbol::from("green").into()])])
    ),
    case::standard_syantx_annotations_constraint(
        load_anonymous_type_v2_0(r#" // For a schema with annotations constraint as below:
                            { annotations: { container_length: 1 } }
                        "#),
    isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::annotations(isl_type_reference::v_2_0::anonymous_type_ref([isl_constraint::v_2_0::container_length(1.into())]))])
    ),
    case::precision_constraint(
        load_anonymous_type(r#" // For a schema with precision constraint as below:
                        { precision: 2 }
                    "#),
        anonymous_type([precision(2.into())])
    ),
    case::scale_constraint(
        load_anonymous_type(r#" // For a schema with scale constraint as below:
                        { scale: 2 }
                    "#),
        anonymous_type([scale(IntegerValue::I64(2).into())])
    ),
    case::exponent_constraint(
        load_anonymous_type_v2_0(r#" // For a schema with exponent constraint as below:
                        { exponent: -2 }
                    "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::exponent(IntegerValue::I64(-2).into())])
    ),
    case::timestamp_precision_constraint(
        load_anonymous_type(r#" // For a schema with timestamp_precision constraint as below:
                            { timestamp_precision: year }
                        "#),
        anonymous_type([timestamp_precision("year".try_into().unwrap())])
    ),
    case::valid_values_constraint(
        load_anonymous_type(r#" // For a schema with valid_values constraint as below:
                        { valid_values: [2, 3.5, 5e7, "hello", hi] }
                    "#),
        anonymous_type([valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), Symbol::from("hi").into()]).unwrap()])
    ),
    case::valid_values_with_range_constraint(
        load_anonymous_type(r#" // For a schema with valid_values constraint as below:
                        { valid_values: range::[1, 5.5] }
                    "#),
        anonymous_type(
                [valid_values_with_range(
                    NumberRange::new(
                        Number::from(&IntegerValue::I64(1)),
                        Number::from(&Decimal::new(55, -1))
                    ).unwrap().into())
                ]
            )
        ),
    case::utf8_byte_length_constraint(
        load_anonymous_type(r#" // For a schema with utf8_byte_length constraint as below:
                        { utf8_byte_length: 3 }
                    "#),
        anonymous_type([utf8_byte_length(3.into())])
    ),
    case::regex_constraint(
        load_anonymous_type(r#" // For a schema with regex constraint as below:
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
        load_anonymous_type(r#" // For a schema with timestamp_offset constraint as below:
                            { timestamp_offset: ["+00:00"] }
                        "#),
        anonymous_type(
            [timestamp_offset(vec!["+00:00".try_into().unwrap()])]
        )
    ),
    case::ieee754_float_constraint(
        load_anonymous_type_v2_0(r#" // For a schema with ieee754_float constraint as below:
                            { ieee754_float: binary16 }
                        "#),
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::ieee754_float(Ieee754InterchangeFormat::Binary16)])
    ),
    )]
    fn owned_struct_to_isl_type(isl_type1: IslType, isl_type2: IslType) {
        // assert if both the IslType are same in terms of constraints and name
        assert_eq!(isl_type1, isl_type2);
    }

    // helper function to create a range
    fn load_range(text: &str, isl_version: IslVersion) -> IonSchemaResult<Range> {
        Range::from_ion_element(
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            RangeType::Any,
            isl_version,
        )
    }

    // helper function to create a timestamp precision range
    fn load_timestamp_precision_range(text: &str) -> IonSchemaResult<Range> {
        Range::from_ion_element(
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            RangeType::TimestampPrecision,
            IslVersion::V1_0,
        )
    }

    // helper function to create a timestamp precision range
    fn load_number_range(text: &str) -> IonSchemaResult<Range> {
        Range::from_ion_element(
            &Element::read_one(text.as_bytes()).expect("parsing failed unexpectedly"),
            RangeType::NumberOrTimestamp,
            IslVersion::V1_0,
        )
    }

    // helper function to return Elements for range `contains` tests
    fn elements<T: Into<Element> + std::clone::Clone>(values: &[T]) -> Vec<Element> {
        values.iter().cloned().map(|v| v.into()).collect()
    }

    #[rstest(
        range1,
        range2,
        case::range_with_integer(
            load_range(
                r#"
                    range::[min, 5]
                "#,
                IslVersion::V1_0
            ).unwrap(),
            IntegerRange::new(
                RangeBoundaryValue::Min,
                IntegerValue::I64(5)
            ).unwrap()
        ),
        case::range_with_float(
            load_range(
                r#"
                    range::[2e1, 5e1]
                "#,
                IslVersion::V1_0
            ).unwrap(),
            FloatRange::new(
                2e1,
                5e1
            ).unwrap()
        ),
        case::range_with_decimal(
            load_range(
                r#"
                    range::[20.4, 50.5]
                "#,
                IslVersion::V1_0
            ).unwrap(),
            DecimalRange::new(
                Decimal::new(204, -1),
                Decimal::new(505, -1)
            ).unwrap()
        ),
        case::range_with_timestamp(
            load_range(
                r#"
                    range::[2020-01-01T, 2021-01-01T]
                "#,
                IslVersion::V2_0
            ).unwrap(),
            TimestampRange::new(
                Timestamp::with_year(2020).with_month(1).with_day(1).build().unwrap(),
                Timestamp::with_year(2021).with_month(1).with_day(1).build().unwrap()
            ).unwrap()
        ),
        case::range_with_timestamp_precision(
            load_timestamp_precision_range(
                r#"
                    range::[year, month]
                "#
            ).unwrap(),
            TimestampPrecisionRange::new(
                TimestampPrecision::Year,
                TimestampPrecision::Month
            ).unwrap()
        ),
        case::range_with_number(
            load_number_range(
                r#"
                    range::[1, 5.5]
                "#
            ).unwrap(),
            NumberRange::new(
                Number::from(&IntegerValue::I64(1)),
                Number::try_from(&Decimal::new(55, -1)).unwrap()
            ).unwrap()
        )
    )]
    fn owned_struct_to_range(range1: Range, range2: impl Into<Range>) {
        // assert if both the ranges are same
        assert_eq!(range1, range2.into());
    }

    #[rstest(
        range,
        case::range_with_min_max(load_range(
            r#"
                range::[min, max]
            "#,
            IslVersion::V1_0
        )),
        case::range_with_max_lower_bound(load_range(
            r#"
                range::[max, 5]
            "#,
            IslVersion::V1_0
        )),
        case::range_with_min_upper_bound(load_range(
            r#"
                range::[5, min]
            "#,
            IslVersion::V1_0
        )),
        case::range_with_mismatched_bounds(load_range(
            r#"
                range::[5, 7.834]
            "#,
            IslVersion::V1_0
        )),
        case::range_with_lower_bound_greater_than_upper_bound(load_range(
            r#"
                range::[10, 5]
            "#,
            IslVersion::V1_0
        ))
    )]
    fn invalid_ranges(range: IonSchemaResult<Range>) {
        // determine that the range is created with an error for an invalid range
        assert!(range.is_err());
    }

    #[rstest(
        range,
        valid_values,
        invalid_values,
        case::int_range(
            load_range(
            r#"
                range::[0, 10]
            "#,
            IslVersion::V1_0
            ),
            elements(&[5, 0, 10]),
            elements(&[-5, 11])
        ),
        case::int_range_with_min(
            load_range(
            r#"
                range::[min, 10]
            "#,
            IslVersion::V1_0
            ),
            elements(&[5, -5, 0]),
            elements(&[11])
        ),
        case::int_range_with_max(
            load_range(
            r#"
                range::[0, max]
            "#,
            IslVersion::V1_0
            ),
            elements(&[5, 0, 11]),
            elements(&[-5])
        ),
        case::int_range_with_exclusive(
            load_range(
            r#"
                range::[exclusive::0, exclusive::10]
            "#,
            IslVersion::V1_0
            ),
            elements(&[5, 9]),
            elements(&[-5, 0, 10])
        ),
        case::decimal_range(
            load_range(
            r#"
                range::[0.0, 10.0]
            "#,
            IslVersion::V1_0
            ),
            elements(&[Decimal::new(55,-1), Decimal::new(0, 0), Decimal::new(100, -1)]),
            elements(&[Decimal::new(-55, -1), Decimal::new(115, -1)])
        ),
        case::decimal_range_with_min(
            load_range(
            r#"
                range::[min, 10.0]
            "#,
            IslVersion::V1_0
            ),
            elements(&[Decimal::new(50, -1), Decimal::new(-55, -1), Decimal::new(0, 0)]),
            elements(&[Decimal::new(115, -1)])
        ),
        case::decimal_range_with_max(
            load_range(
            r#"
                range::[0.0, max]
            "#,
            IslVersion::V1_0
            ),
            elements(&[Decimal::new(55, -1), Decimal::new(115, -1)]),
            elements(&[Decimal::new(-55, -1)])
        ),
        case::decimal_range_with_exclusive(
            load_range(
            r#"
                range::[exclusive::1.0, exclusive::10.0]
            "#,
            IslVersion::V1_0
            ),
            elements(&[Decimal::new(50, -1), Decimal::new(95, -1)]),
            elements(&[Decimal::new(-55, -1), Decimal::new(10, -1), Decimal::new(100, -1)])
        ),
        case::float_range(
            load_range(
            r#"
                range::[1e2, 5e2]
            "#,
            IslVersion::V1_0
            ),
            elements(&[2e2, 1e2, 5e2]),
            elements(&[-1e2,1e1, 6e2, f64::NAN, 0e0, -0e0])
        ),
        case::float_range_with_min(
            load_range(
            r#"
                range::[min, 2e5]
            "#,
            IslVersion::V1_0
            ),
            elements(&[f64::NEG_INFINITY, 2.2250738585072014e-308, 2e5, -2e5, 0e0, -0e0]),
            elements(&[3e5, f64::NAN])
        ),
        case::float_range_with_max(
            load_range(
            r#"
                range::[1e5, max]
            "#,
            IslVersion::V1_0
            ),
            elements(&[1e5, 5e5, 1e6, 1.7976931348623157e308, f64::INFINITY]),
            elements(&[-5e5, 1e2, f64::NAN])
        ),
        case::float_range_with_exclusive(
            load_range(
            r#"
                range::[exclusive::1e2, exclusive::5e2]
            "#,
            IslVersion::V1_0
            ),
            elements(&[2e2]),
            elements(&[-1e2 ,1e1, 6e2, 1e2, 5e2, f64::NAN])
        ),
        case::timestamp_precision_range(
            load_timestamp_precision_range(
            r#"
                range::[minute, second]
            "#
            ),
            elements(&[Timestamp::with_ymd(2020, 1, 1).with_hms(0, 1, 0).build_at_offset(4 * 60).unwrap(),
                Timestamp::with_ymd(2020, 1, 1).with_hour_and_minute(0, 1).build_at_offset(4 * 60).unwrap()]),
            elements(&[Timestamp::with_year(2020).build().unwrap(),
                Timestamp::with_ymd(2020, 1, 1).with_hms(0, 1, 0).with_milliseconds(678).build_at_offset(4 * 60).unwrap()])
        ),
        case::number_range(
            load_number_range(
                r#"
                    range::[-1, 5.5]
                "#
            ),
            vec![0.into(), (-1).into(), 1.into(), Decimal::new(55, -1).into(), 5e0.into()],
            vec![(-2).into() , Decimal::new(-15, -1).into(), Decimal::new(56, -1).into(), 5e1.into()]
        ),
    )]
    fn range_contains(
        range: IonSchemaResult<Range>,
        valid_values: Vec<Element>,
        invalid_values: Vec<Element>,
    ) {
        // verify if the range contains given valid values
        for valid_value in valid_values {
            let range_contains_result = range.as_ref().unwrap().contains(&valid_value);
            assert!(range_contains_result)
        }

        // verify that range doesn't contain the invalid values
        for invalid_value in invalid_values {
            let range_contains_result = range.as_ref().unwrap().contains(&invalid_value);
            assert!(!range_contains_result)
        }
    }

    #[rstest(
    range,
    expected,
    case::range_with_integer(
        IntegerRange::new(
            RangeBoundaryValue::Min,
            IntegerValue::I64(5)
        ).unwrap(),
        "range::[ min, 5 ]"
    ),
    case::range_with_float(
        FloatRange::new(
            2e1,
            5e1
        ).unwrap(),
        "range::[ 20, 50 ]"
    ),
    case::range_with_decimal(
        DecimalRange::new(
            Decimal::new(204, -1),
            Decimal::new(505, -1)
        ).unwrap(),
        "range::[ 20.4, 50.5 ]"
    ),
    case::range_with_timestamp(
        TimestampRange::new(
            Timestamp::with_year(2020).with_month(1).with_day(1).build().unwrap(),
            Timestamp::with_year(2021).with_month(1).with_day(1).build().unwrap()
        ).unwrap(),
        "range::[ 2020-01-01T, 2021-01-01T ]"
    ),
    case::range_with_timestamp_precision(
        TimestampPrecisionRange::new(
            TimestampPrecision::Year,
            TimestampPrecision::Month
        ).unwrap(),
        "range::[ year, month ]"
    ),
    case::range_with_number(
        NumberRange::new(
            Number::from(&IntegerValue::I64(1)),
            Number::try_from(&Decimal::new(55, -1)).unwrap()
        ).unwrap(),
        "range::[ 1, 5.5 ]"
    )
    )]
    fn range_display(range: impl Into<Range>, expected: String) {
        let mut buf = Vec::new();
        write!(&mut buf, "{}", range.into()).unwrap();
        assert_eq!(expected, String::from_utf8(buf).unwrap());
    }
}
