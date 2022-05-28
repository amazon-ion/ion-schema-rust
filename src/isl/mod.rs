//! Provides a way to construct ISL types/constraints programmatically.
//!
//! This module consists of three submodules that help constructing ISL types/constraint:
//!
//! * `isl_type` module represents a schema type [IslType] which converts given ion content in the schema file
//! into an ISL types(not-yet-resolved types). It stores [IslConstraint]s defined within the given type.
//!
//! * `isl_import` module represents a schema import [IslImport] which converts given ion content in the schema file
//! into an ISL import. It stores schema id, an optional type that needs to be imported and an optional alias to that type.
//!
//! * `isl_constraint` module represents schema constraints [IslConstraint]
//! which converts given ion content in the schema file into an ISL constraint(not-yet-resolved constraints).
//! This stores [IslTypeRef]s for the given ISL constraint.
//!
//! * `isl_type_reference` module provides a schema type reference.
//! The type reference grammar is defined in the [Ion Schema Spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
//!
//! ## Example usage of `isl` module to create an [IslType]:
//! ```
//! use ion_schema::isl::{isl_type::IslType, isl_constraint::IslConstraint, isl_type_reference::IslTypeRef};
//! use ion_schema::schema::Schema;
//! use ion_schema::system::SchemaSystem;
//! use std::path::Path;
//!
//! // below code represents an ISL type:
//! // type:: {
//! //      name:my_type_name,
//! //      type: int,
//! //      all_of: [
//! //          { type: bool }
//! //      ]
//! //  }
//! let isl_type = IslType::named(
//!     // represents the `name` of the defined type
//!     "my_type_name".to_owned(),
//!     vec![
//!         // represents the `type: int` constraint
//!         IslConstraint::type_constraint(
//!             IslTypeRef::named("int")
//!         ),
//!         // represents `all_of` with anonymous type `{ type: bool }` constraint
//!         IslConstraint::all_of(
//!             vec![
//!                 IslTypeRef::anonymous(
//!                     vec![
//!                         IslConstraint::type_constraint(
//!                             IslTypeRef::named("bool")
//!                         )
//!                     ]
//!                 )
//!             ]
//!         )
//!     ]
//! );
//!
//! // create a schema from given IslType using SchemaSystem
//! let schema_system = SchemaSystem::new(vec![]); // no authorities added
//! let schema = schema_system.schema_from_isl_types("my_schema", [isl_type.to_owned()]);
//!
//! assert!(schema.is_ok());
//!
//! // verify if the generated schema contains the correct type
//! assert!(schema.unwrap().get_type("my_type_name").is_some())
//! ```

// The given schema is loaded with a two phase approach:
// 1. Phase 1: Constructing an internal representation of ISL types/constraints from given schema file.
//             This phase creates all [IslType],  [IslConstraint], [IslTypeRef] structs from the ion content in schema file.
// 2. Phase 2: Constructing resolved types/constraints from internal representation of ISL types/constraints(not-yet-resolved types/constraints).
//             This is done by loading all types into [Schema] as below:
//                 a. Convert all [IslType] → [TypeDefinition], [IslConstraint] → [Constraint], [IslTypeRef] → [TypeDefinition]
//                 b. While doing (a) store all [TypeDefinition] in the [TypeStore](which could help
//                    returning resolved types in a schema) and store generated [TypeId] in the constraint.

use crate::isl::isl_import::{IslImport, IslImportType};
use crate::isl::isl_type::IslTypeImpl;

pub mod isl_constraint;
pub mod isl_import;
pub mod isl_type;
pub mod isl_type_reference;
pub mod util;

/// Provides an internal representation of an schema file
#[derive(Debug, Clone)]
pub struct IslSchema {
    // Represents all the IslImports inside the schema file.
    // For more information: https://amzn.github.io/ion-schema/docs/spec.html#imports
    imports: Vec<IslImport>,
    // Represents all the IslTypeImpls defined in this schema file.
    // For more information: https://amzn.github.io/ion-schema/docs/spec.html#type-definitions
    types: Vec<IslTypeImpl>,
    // Represents all the inline IslImportTypes in this schema file.
    inline_imported_types: Vec<IslImportType>,
}

impl IslSchema {
    pub fn new(
        imports: Vec<IslImport>,
        types: Vec<IslTypeImpl>,
        inline_imports: Vec<IslImportType>,
    ) -> Self {
        Self {
            imports,
            types,
            inline_imported_types: inline_imports,
        }
    }

    pub fn imports(&self) -> &[IslImport] {
        &self.imports
    }

    pub fn types(&self) -> &[IslTypeImpl] {
        &self.types
    }

    pub fn inline_imported_types(&self) -> &[IslImportType] {
        &self.inline_imported_types
    }
}

#[cfg(test)]
mod isl_tests {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type::{IslType, IslTypeImpl};
    use crate::isl::isl_type_reference::IslTypeRef;
    use crate::isl::util::TimestampPrecisionValue;
    use crate::isl::util::{Range, RangeBoundaryType, RangeBoundaryValue, RangeType};
    use crate::result::IonSchemaResult;
    use ion_rs::types::decimal::*;
    use ion_rs::types::integer::Integer as IntegerValue;
    use ion_rs::types::timestamp::Timestamp;
    use ion_rs::value::owned::text_token;
    use ion_rs::value::owned::OwnedElement;
    use ion_rs::value::reader::element_reader;
    use ion_rs::value::reader::ElementReader;
    use rstest::*;

    // helper function to create NamedIslType for isl tests
    fn load_named_type(text: &str) -> IslType {
        IslType::Named(
            IslTypeImpl::from_owned_element(
                &element_reader()
                    .read_one(text.as_bytes())
                    .expect("parsing failed unexpectedly"),
                &mut vec![],
            )
            .unwrap(),
        )
    }

    // helper function to create AnonymousIslType for isl tests
    fn load_anonymous_type(text: &str) -> IslType {
        IslType::Anonymous(
            IslTypeImpl::from_owned_element(
                &element_reader()
                    .read_one(text.as_bytes())
                    .expect("parsing failed unexpectedly"),
                &mut vec![],
            )
            .unwrap(),
        )
    }

    #[rstest(
    isl_type1,isl_type2,
    case::type_constraint_with_anonymous_type(
        load_anonymous_type(r#" // For a schema with single anonymous type
                {type: any}
            "#),
        IslType::anonymous([IslConstraint::type_constraint(IslTypeRef::named("any"))])
    ),
    case::type_constraint_with_named_type(
        load_named_type(r#" // For a schema with named type
                type:: { name: my_int, type: int }
            "#),
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::named("int"))])
    ),
    case::type_constraint_with_named_nullable_type(
        load_named_type(r#" // For a schema with named type
                type:: { name: my_nullable_int, type: $int }
            "#),
        IslType::named("my_nullable_int", [IslConstraint::type_constraint(IslTypeRef::named("$int"))])
    ),
    case::type_constraint_with_self_reference_type(
        load_named_type(r#" // For a schema with self reference type
                type:: { name: my_int, type: my_int }
            "#),
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::named("my_int"))])
    ),
    case::type_constraint_with_nested_self_reference_type(
        load_named_type(r#" // For a schema with nested self reference type
                type:: { name: my_int, type: { type: my_int } }
            "#),
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))])
    ),
    case::type_constraint_with_nested_type(
        load_named_type(r#" // For a schema with nested types
                type:: { name: my_int, type: { type: int } }
            "#),
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]))])
    ),
    case::type_constraint_with_nested_multiple_types(
        load_named_type(r#" // For a schema with nested multiple types
                type:: { name: my_int, type: { type: int }, type: { type: my_int } }
            "#),
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))])), IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::Type(IslTypeRef::named("my_int"))]))])
    ),
    case::all_of_constraint(
        load_anonymous_type(r#" // For a schema with all_of type as below:
                { all_of: [{ type: int }] }
            "#),
        IslType::anonymous([IslConstraint::all_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))])])])
    ),
    case::any_of_constraint(
        load_anonymous_type(r#" // For a schema with any_of constraint as below:
                    { any_of: [{ type: int }, { type: decimal }] }
                "#),
        IslType::anonymous([IslConstraint::any_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("decimal"))])])])
    ),
    case::one_of_constraint(
        load_anonymous_type(r#" // For a schema with one_of constraint as below:
                    { one_of: [{ type: int }, { type: decimal }] }
                "#),
        IslType::anonymous([IslConstraint::one_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("decimal"))])])])
    ),
    case::not_constraint(
        load_anonymous_type(r#" // For a schema with not constraint as below:
                    { not: { type: int } }
                "#),
        IslType::anonymous([IslConstraint::not(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]))])
    ),
    case::ordered_elements_constraint(
        load_anonymous_type(r#" // For a schema with ordered_elements constraint as below:
                    { ordered_elements: [  symbol, { type: int, occurs: optional },  ] }
                "#),
        IslType::anonymous([IslConstraint::ordered_elements([IslTypeRef::named("symbol"), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int")), IslConstraint::Occurs(Range::optional())])])])
    ),
    case::fields_constraint(
        load_anonymous_type(r#" // For a schema with fields constraint as below:
                    { fields: { name: string, id: int} }
                "#),
        IslType::anonymous([IslConstraint::fields(vec![("name".to_owned(), IslTypeRef::named("string")), ("id".to_owned(), IslTypeRef::named("int"))].into_iter())]),
    ),
    case::contains_constraint(
        load_anonymous_type(r#" // For a schema with contains constraint as below:
                    { contains: [true, 1, "hello"] }
                "#),
        IslType::anonymous([IslConstraint::contains([true.into(), 1.into(), "hello".to_owned().into()])])
    ),
    case::container_length_constraint(
        load_anonymous_type(r#" // For a schema with container_length constraint as below:
                    { container_length: 3 }
                "#),
        IslType::anonymous([IslConstraint::container_length(3.into())])
    ),
    case::byte_length_constraint(
        load_anonymous_type(r#" // For a schema with byte_length constraint as below:
                    { byte_length: 3 }
                "#),
        IslType::anonymous([IslConstraint::byte_length(3.into())])
    ),
    case::codepoint_length_constraint(
        load_anonymous_type(r#" // For a schema with codepoint_length constraint as below:
                    { codepoint_length: 3 }
                "#),
        IslType::anonymous([IslConstraint::codepoint_length(3.into())])
    ),
    case::element_constraint(
        load_anonymous_type(r#" // For a schema with element constraint as below:
                    { element: int }
                "#),
        IslType::anonymous([IslConstraint::element(IslTypeRef::named("int"))])
    ),
    case::annotations_constraint(
        load_anonymous_type(r#" // For a schema with annotations constraint as below:
                        { annotations: closed::[red, blue, green] }
                    "#),
        IslType::anonymous([IslConstraint::annotations(vec!["closed"], vec![text_token("red").into(), text_token("blue").into(), text_token("green").into()])])
    ),
    case::precision_constraint(
        load_anonymous_type(r#" // For a schema with precision constraint as below:
                        { precision: 2 }
                    "#),
        IslType::anonymous([IslConstraint::precision(2.into())])
    ),
    case::scale_constraint(
        load_anonymous_type(r#" // For a schema with scale constraint as below:
                        { scale: 2 }
                    "#),
        IslType::anonymous([IslConstraint::scale((&IntegerValue::I64(2)).into())])
    ),
    )]
    fn owned_struct_to_isl_type(isl_type1: IslType, isl_type2: IslType) {
        // assert if both the IslType are same in terms of constraints and name
        assert_eq!(isl_type1, isl_type2);
    }

    // helper function to create a range
    fn load_range(text: &str) -> IonSchemaResult<Range> {
        Range::from_ion_element(
            &element_reader()
                .read_one(text.as_bytes())
                .expect("parsing failed unexpectedly"),
            RangeType::Any,
        )
    }

    // helper function to return OwnedElements for range `contains` tests
    fn elements<T: Into<OwnedElement> + std::clone::Clone>(values: &[T]) -> Vec<OwnedElement> {
        values.iter().cloned().map(|v| v.into()).collect()
    }

    #[rstest(
        range1,
        range2,
        case::range_with_integer(
            load_range(
                r#"
                    range::[min, 5]
                "#
            ),
            Range::range(
                RangeBoundaryValue::Min,
                RangeBoundaryValue::int_value(IntegerValue::I64(5), RangeBoundaryType::Inclusive)
            )
        ),
        case::range_with_float(
            load_range(
                r#"
                    range::[2e1, 5e1]
                "#
            ),
            Range::range(
                RangeBoundaryValue::float_value(2e1, RangeBoundaryType::Inclusive),
                RangeBoundaryValue::float_value(5e1, RangeBoundaryType::Inclusive)
            )
        ),
        case::range_with_decimal(
            load_range(
                r#"
                    range::[20.4, 50.5]
                "#
            ),
            Range::range(
                RangeBoundaryValue::decimal_value(Decimal::new(204, -1), RangeBoundaryType::Inclusive),
                RangeBoundaryValue::decimal_value(Decimal::new(505, -1), RangeBoundaryType::Inclusive)
            )
        ),
        case::range_with_timestamp(
            load_range(
                r#"
                    range::[2020-01-01T, 2021-01-01T]
                "#
            ),
            Range::range(
                RangeBoundaryValue::timestamp_value(Timestamp::with_year(2020).with_month(1).with_day(1).build().unwrap(), RangeBoundaryType::Inclusive),
                RangeBoundaryValue::timestamp_value(Timestamp::with_year(2021).with_month(1).with_day(1).build().unwrap(), RangeBoundaryType::Inclusive)
            )
        ),
        case::range_with_timestamp_precision(
            load_range(
            r#"
                        range::[year, month]
                    "#
            ),
            Range::range(
                RangeBoundaryValue::timestamp_precision_value(TimestampPrecisionValue::Year, RangeBoundaryType::Inclusive),
                RangeBoundaryValue::timestamp_precision_value(TimestampPrecisionValue::Month, RangeBoundaryType::Inclusive)
            )
        )
    )]
    fn owned_struct_to_range(range1: IonSchemaResult<Range>, range2: IonSchemaResult<Range>) {
        // determine that both the ranges are created with no errors
        assert!(range1.is_ok());
        assert!(range2.is_ok());

        // assert if both the ranges are same
        assert_eq!(range1.unwrap(), range2.unwrap());
    }

    #[rstest(
        range,
        case::range_with_min_max(load_range(
            r#"
                range::[min, max]
            "#
        )),
        case::range_with_max_lower_bound(load_range(
            r#"
                range::[max, 5]
            "#
        )),
        case::range_with_min_upper_bound(load_range(
            r#"
                range::[5, min]
            "#
        )),
        case::range_with_mismatched_bounds(load_range(
            r#"
                range::[5, 7.834]
            "#
        )),
        // TODO: uncomment below test case once we have a comparator for timestamp in ion-rust 
        // case::range_with_lower_bound_greater_than_upper_bound(load_range(
        //     r#"
        //         range::[10, 5]
        //     "#
        // ))
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
            "#
            ),
            elements(&[5, 0, 10]),
            elements(&[-5, 11])
        ),
        case::int_range_with_min(
            load_range(
            r#"
                range::[min, 10]
            "#
            ),
            elements(&[5, -5, 0]),
            elements(&[11])
        ),
        case::int_range_with_max(
            load_range(
            r#"
                range::[0, max]
            "#
            ),
            elements(&[5, 0, 11]),
            elements(&[-5])
        ),
        case::int_range_with_exclusive(
            load_range(
            r#"
                range::[exclusive::0, exclusive::10]
            "#
            ),
            elements(&[5, 9]),
            elements(&[-5, 0, 10])
        ),
        case::decimal_range(
            load_range(
            r#"
                range::[0.0, 10.0]
            "#
            ),
            elements(&[Decimal::new(55,-1), Decimal::new(0, 0), Decimal::new(100, -1)]),
            elements(&[Decimal::new(-55, -1), Decimal::new(115, -1)])
        ),
        case::decimal_range_with_min(
            load_range(
            r#"
                range::[min, 10.0]
            "#
            ),
            elements(&[Decimal::new(50, -1), Decimal::new(-55, -1), Decimal::new(0, 0)]),
            elements(&[Decimal::new(115, -1)])
        ),
        case::decimal_range_with_max(
            load_range(
            r#"
                range::[0.0, max]
            "#
            ),
            elements(&[Decimal::new(55, -1), Decimal::new(115, -1)]),
            elements(&[Decimal::new(-55, -1)])
        ),
        case::decimal_range_with_exclusive(
            load_range(
            r#"
                range::[exclusive::1.0, exclusive::10.0]
            "#
            ),
            elements(&[Decimal::new(50, -1), Decimal::new(95, -1)]),
            elements(&[Decimal::new(-55, -1), Decimal::new(10, -1), Decimal::new(100, -1)])
        ),
        case::float_range(
            load_range(
            r#"
                range::[1e2, 5e2]
            "#
            ),
            elements(&[2e2, 1e2, 5e2]),
            elements(&[-1e2,1e1, 6e2, f64::NAN, 0e0, -0e0])
        ),
        case::float_range_with_min(
            load_range(
            r#"
                range::[min, 2e5]
            "#
            ),
            elements(&[f64::NEG_INFINITY, 2.2250738585072014e-308, 2e5, -2e5, 0e0, -0e0]),
            elements(&[3e5, f64::NAN])
        ),
        case::float_range_with_max(
            load_range(
            r#"
                range::[1e5, max]
            "#
            ),
            elements(&[1e5, 5e5, 1e6, 1.7976931348623157e308, f64::INFINITY]),
            elements(&[-5e5, 1e2, f64::NAN])
        ),
        case::float_range_with_exclusive(
            load_range(
            r#"
                range::[exclusive::1e2, exclusive::5e2]
            "#
            ),
            elements(&[2e2]),
            elements(&[-1e2 ,1e1, 6e2, 1e2, 5e2, f64::NAN])
        ),
        case::timestamp_precision_range(
            load_range(
            r#"
                range::[minute, second]
            "#
            ),
            elements(&[Timestamp::with_ymd(2020, 1, 1).with_hms(0, 1, 0).build_at_offset(4 * 60).unwrap(),
                Timestamp::with_ymd(2020, 1, 1).with_hour_and_minute(0, 1).build_at_offset(4 * 60).unwrap()]),
            elements(&[Timestamp::with_year(2020).build().unwrap(),
                Timestamp::with_ymd(2020, 1, 1).with_hms(0, 1, 0).with_milliseconds(678).build_at_offset(4 * 60).unwrap()])
        ),
    )]
    fn range_contains(
        range: IonSchemaResult<Range>,
        valid_values: Vec<OwnedElement>,
        invalid_values: Vec<OwnedElement>,
    ) {
        // verify if the range contains given valid values
        for valid_value in valid_values {
            let range_contains_result = range.as_ref().unwrap().contains(&valid_value).unwrap();
            assert!(range_contains_result)
        }

        // verify that range doesn't contain the invalid values
        for invalid_value in invalid_values {
            let range_contains_result = range.as_ref().unwrap().contains(&invalid_value).unwrap();
            assert!(!range_contains_result)
        }
    }
}
