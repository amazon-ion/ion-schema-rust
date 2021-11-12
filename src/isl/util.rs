use crate::result::{invalid_schema_error, IonSchemaResult};
use ion_rs::types::decimal::Decimal;
use ion_rs::types::timestamp::Timestamp;
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::{AnyInt, Element, IntAccess, Sequence, SymbolToken};
use ion_rs::IonType;
use num_traits::Signed;
use std::convert::TryInto;

/// Represents ISL [Range]s where some constraints can be defined by a range
/// <RANGE<RANGE_TYPE>> ::= range::[ <EXCLUSIVITY><RANGE_TYPE>, <EXCLUSIVITY><RANGE_TYPE> ]
///                       | range::[ min, <EXCLUSIVITY><RANGE_TYPE> ]
///                       | range::[ <EXCLUSIVITY><RANGE_TYPE>, max ]
/// Grammar: <RANGE_TYPE> ::= <DECIMAL>
///                | <FLOAT>
///                | <INT>
///                | <NUMBER>
///                | <TIMESTAMP>
///                | <TIMESTAMP_PRECISION_VALUE>
/// For more information on [Range]: <https://amzn.github.io/ion-schema/docs/spec.html#constraints>
#[derive(Debug, Clone, PartialEq)]
pub enum Range {
    Decimal(RangeBoundaryValue, RangeBoundaryValue),
    Float(RangeBoundaryValue, RangeBoundaryValue),
    Integer(RangeBoundaryValue, RangeBoundaryValue),
    IntegerNonNegative(RangeBoundaryValue, RangeBoundaryValue),
    Timestamp(RangeBoundaryValue, RangeBoundaryValue),
}

impl Range {
    pub fn decimal_range(
        start: RangeBoundaryValue,
        end: RangeBoundaryValue,
    ) -> IonSchemaResult<Range> {
        Range::validate_range_boundaries(&start, &end)?;
        Ok(Range::Decimal(start, end))
    }

    pub fn float_range(
        start: RangeBoundaryValue,
        end: RangeBoundaryValue,
    ) -> IonSchemaResult<Range> {
        Range::validate_range_boundaries(&start, &end)?;
        Ok(Range::Float(start, end))
    }

    pub fn int_range(start: RangeBoundaryValue, end: RangeBoundaryValue) -> IonSchemaResult<Range> {
        Range::validate_range_boundaries(&start, &end)?;
        Ok(Range::Integer(start, end))
    }

    pub fn int_non_negative_range(
        start: RangeBoundaryValue,
        end: RangeBoundaryValue,
    ) -> IonSchemaResult<Range> {
        Range::validate_range_boundaries(&start, &end)?;
        Ok(Range::IntegerNonNegative(start, end))
    }

    pub fn timestamp_range(
        start: RangeBoundaryValue,
        end: RangeBoundaryValue,
    ) -> IonSchemaResult<Range> {
        Range::validate_range_boundaries(&start, &end)?;
        Ok(Range::Timestamp(start, end))
    }

    pub fn from_ion_element(value: &OwnedElement, is_non_negative: bool) -> IonSchemaResult<Range> {
        let range = try_to!(value.as_sequence());
        if range.len() != 2 {
            return invalid_schema_error(
                "Ranges must contain two values representing minimum and maximum ends of range.",
            );
        }

        // set start of the range
        let start = RangeBoundaryValue::from_ion_element(try_to!(range.get(0)), is_non_negative)?;

        // set end of the range
        let end = RangeBoundaryValue::from_ion_element(try_to!(range.get(1)), is_non_negative)?;

        // validate both range boundary values
        let range_type = Range::validate_range_boundaries(&start, &end)?;
        match range_type {
            RangeBoundaryValueType::Decimal(_) => Ok(Range::Decimal(start, end)),
            RangeBoundaryValueType::Float(_) => Ok(Range::Float(start, end)),
            RangeBoundaryValueType::Integer(_) => Ok(Range::Integer(start, end)),
            RangeBoundaryValueType::IntegerNonNegative(_) => {
                Ok(Range::IntegerNonNegative(start, end))
            }
            RangeBoundaryValueType::Timestamp(_) => Ok(Range::Timestamp(start, end)),
        }
    }

    // helper method to which validates a non negative integer range boundary value
    fn validate_non_negative_integer_range_boundary_value(
        value: &AnyInt,
    ) -> IonSchemaResult<usize> {
        match value.as_i64() {
            Some(v) => {
                if v > 0 {
                    match v.try_into() {
                        Err(_) => invalid_schema_error(format!(
                            "Expected non negative integer for range boundary values, found {}",
                            v
                        )),
                        Ok(non_negative_int_value) => Ok(non_negative_int_value),
                    }
                } else {
                    invalid_schema_error(format!(
                        "Expected non negative integer for range boundary values, found {}",
                        v
                    ))
                }
            }
            None => match value.as_big_int() {
                None => {
                    unreachable!("Expected range boundary values must be a non negative integer")
                }
                Some(v) => {
                    if v.is_negative() {
                        match v.try_into() {
                            Err(_) => invalid_schema_error(format!(
                                "Expected non negative integer for range boundary values, found {}",
                                v
                            )),
                            Ok(non_negative_int_value) => Ok(non_negative_int_value),
                        }
                    } else {
                        invalid_schema_error(format!(
                            "Expected non negative integer for range boundary values, found {}",
                            v
                        ))
                    }
                }
            },
        }
    }

    // this function validates the range boundaries and returns a range boundary value type,
    // which helps determine the range type (Decimal, integer, etc.)
    fn validate_range_boundaries(
        start: &RangeBoundaryValue,
        end: &RangeBoundaryValue,
    ) -> IonSchemaResult<RangeBoundaryValueType> {
        use RangeBoundaryValue::*;
        match (start, end) {
            (Min, Max) => {
                return invalid_schema_error("Range boundaries can not be min and max together (i.e. range::[min, max] is not allowed)")
            }
            (Min, Value(v, _)) => {
                Ok(v.to_owned())
            }
            (Value(v, _), Max) => {
                Ok(v.to_owned())
            }
            (Value(v1, _), Value(v2, _)) => {
                match (v1, v2) {
                    (RangeBoundaryValueType::Integer(_), RangeBoundaryValueType::Integer(_)) => {}
                    (RangeBoundaryValueType::IntegerNonNegative(_), RangeBoundaryValueType::IntegerNonNegative(_)) => {}
                    (RangeBoundaryValueType::Decimal(_), RangeBoundaryValueType::Decimal(_)) => {}
                    (RangeBoundaryValueType::Float(_), RangeBoundaryValueType::Float(_)) => {}
                    (RangeBoundaryValueType::Timestamp(_), RangeBoundaryValueType::Timestamp(_)) => {},
                    _ => {
                        return invalid_schema_error("Both range boundary values must be of same type")
                    }
                };
                // TODO: un-comment below code once the timestamp comparator for ion-rust is implemented
                // if start > end {
                //     return invalid_schema_error("Lower range boundary value can not be bigger than upper range boundary")
                // }
                Ok(v1.to_owned())
            }
            (Max, _) => {
                return invalid_schema_error("Lower range boundary value must not be max")
            }
            (_, Min) => {
                return invalid_schema_error("Upper range boundary value must not be min")
            }
        }
    }
}

/// Represents the type of range boundary value
#[derive(Debug, Clone, PartialEq)]
pub enum RangeBoundaryValueType {
    Decimal(Decimal),
    Float(f64),
    Integer(i64),
    IntegerNonNegative(usize),
    Timestamp(Timestamp),
}

/// Represents a range boundary value (i.e. min, max or a value in terms of [RangeBoundaryValueType])
#[derive(Debug, Clone, PartialEq)]
pub enum RangeBoundaryValue {
    Max,
    Min,
    Value(RangeBoundaryValueType, RangeBoundaryType),
}

impl RangeBoundaryValue {
    pub fn int_value(value: i64, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(RangeBoundaryValueType::Integer(value), range_boundary_type)
    }
    pub fn int_non_negative_value(value: usize, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(
            RangeBoundaryValueType::IntegerNonNegative(value),
            range_boundary_type,
        )
    }
    pub fn float_value(value: f64, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(RangeBoundaryValueType::Float(value), range_boundary_type)
    }
    pub fn timestamp_value(value: Timestamp, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(
            RangeBoundaryValueType::Timestamp(value),
            range_boundary_type,
        )
    }
    pub fn decimal_value(value: Decimal, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(RangeBoundaryValueType::Decimal(value), range_boundary_type)
    }

    fn from_ion_element(value: &OwnedElement, is_non_negative: bool) -> IonSchemaResult<Self> {
        let value_annotations: Vec<&OwnedSymbolToken> = value.annotations().collect();
        let range_boundary_type = if value_annotations.contains(&&text_token("exclusive")) {
            RangeBoundaryType::Exclusive
        } else {
            RangeBoundaryType::Inclusive
        };

        match value.ion_type() {
            IonType::Symbol => {
                let sym = try_to!(try_to!(value.as_sym()).text());
                match sym {
                    "min" => Ok(RangeBoundaryValue::Min),
                    "max" => Ok(RangeBoundaryValue::Max),
                    _ => {
                        return invalid_schema_error(format!(
                            "Range boundary value: {} is not supported",
                            sym
                        ))
                    }
                }
            }
            IonType::Integer => {
                if is_non_negative {
                    let value = Range::validate_non_negative_integer_range_boundary_value(
                        value.as_any_int().unwrap(),
                    )?
                    .to_owned();
                    Ok(RangeBoundaryValue::int_non_negative_value(
                        value,
                        range_boundary_type,
                    ))
                } else {
                    Ok(RangeBoundaryValue::int_value(
                        value.as_any_int().unwrap().as_i64().unwrap(),
                        range_boundary_type,
                    ))
                }
            }
            IonType::Decimal => Ok(RangeBoundaryValue::decimal_value(
                value.as_decimal().unwrap().to_owned(),
                range_boundary_type,
            )),
            IonType::Float => Ok(RangeBoundaryValue::float_value(
                value.as_f64().unwrap(),
                range_boundary_type,
            )),
            IonType::Timestamp => Ok(RangeBoundaryValue::timestamp_value(
                value.as_timestamp().unwrap().to_owned(),
                range_boundary_type,
            )),
            _ => return invalid_schema_error("Unsupported range type specified"),
        }
    }
}

/// Represents the range boundary types in terms of exclusivity (i.e. inclusive or exclusive)
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RangeBoundaryType {
    Inclusive,
    Exclusive,
}
