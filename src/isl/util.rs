use crate::isl::isl_constraint::{IslLength, IslOccurs};
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use ion_rs::types::decimal::Decimal;
use ion_rs::types::timestamp::Timestamp;
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::{AnyInt, Element, IntAccess, Sequence, SymbolToken};
use ion_rs::IonType;
use num_bigint::BigInt;
use num_traits::Signed;
use num_traits::Zero;
use std::convert::{TryFrom, TryInto};

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
    /// Provides a boolean value to specify whether the given value is within the range or not
    pub fn contains(&self, value: &OwnedElement) -> IonSchemaResult<bool> {
        use RangeBoundaryValue::*;
        use RangeBoundaryValueType::*;
        match self {
            Range::Integer(start, end) => {
                let value = value.as_any_int().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Integer ranges can only have integer value for validation",
                    )
                })?;
                let is_in_lower_bound = match start {
                    Min => true,
                    Value(start_value, boundary_type) => match start_value {
                        Integer(min_value) => {
                            match value {
                                AnyInt::I64(int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => &min_value.as_i64().unwrap() <= int_value,
                                    RangeBoundaryType::Exclusive => &min_value.as_i64().unwrap() < int_value,
                                }},
                                AnyInt::BigInt(big_int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => min_value.as_big_int().unwrap() <= big_int_value,
                                    RangeBoundaryType::Exclusive => min_value.as_big_int().unwrap() < big_int_value,
                                }}
                            }
                        },
                        _ => unreachable!("Integer range can only have integers as lower and upper range boundary value"),
                    },
                    Max => unreachable!("Cannot have 'Max' as the lower range boundary")
                };

                let is_in_upper_bound = match end {
                    Max => true,
                    Min => unreachable!("Cannot have 'Min' as the upper range boundary"),
                    Value(end_value, boundary_type) => match end_value {
                        Integer(max_value) => {
                            match value {
                                AnyInt::I64(int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => &max_value.as_i64().unwrap() >= int_value,
                                    RangeBoundaryType::Exclusive => &max_value.as_i64().unwrap() > int_value,
                                }},
                                AnyInt::BigInt(big_int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => max_value.as_big_int().unwrap() >= big_int_value,
                                    RangeBoundaryType::Exclusive => max_value.as_big_int().unwrap() > big_int_value,
                                }}
                            }
                        },
                        _ => unreachable!("Integer range can only have integers as lower and upper range boundary value"),
                    }
                };
                Ok(is_in_upper_bound && is_in_lower_bound)
            }
            Range::IntegerNonNegative(start, end) => {
                let value = value.as_any_int().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Integer ranges can only have integer value for validation",
                    )
                })?;
                let is_in_lower_bound = match start {
                    Min => match value {
                        AnyInt::I64(int_value) => &0 <= int_value,
                        AnyInt::BigInt(big_int_value) => &BigInt::zero() <= big_int_value,
                    },
                    Value(start_value, boundary_type) => match start_value {
                        IntegerNonNegative(min_value) => {
                            match value {
                                AnyInt::I64(int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => &min_value.as_i64().unwrap() <= int_value,
                                    RangeBoundaryType::Exclusive => &min_value.as_i64().unwrap() < int_value,
                                }},
                                AnyInt::BigInt(big_int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => min_value.as_big_int().unwrap() <= big_int_value,
                                    RangeBoundaryType::Exclusive => min_value.as_big_int().unwrap() < big_int_value,
                                }}
                            }
                        },
                        _ => unreachable!("Integer range can only have integers as lower and upper range boundary value"),
                    },
                    Max => unreachable!("Cannot have 'Max' as the lower range boundary")
                };

                let is_in_upper_bound = match end {
                    Max => true,
                    Min => unreachable!("Cannot have 'Min' as the upper range boundary"),
                    Value(end_value, boundary_type) => match end_value {
                        IntegerNonNegative(max_value) => {
                            match value {
                                AnyInt::I64(int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => &max_value.as_i64().unwrap() >= int_value,
                                    RangeBoundaryType::Exclusive => &max_value.as_i64().unwrap() > int_value,
                                }},
                                AnyInt::BigInt(big_int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => max_value.as_big_int().unwrap() >= big_int_value,
                                    RangeBoundaryType::Exclusive => max_value.as_big_int().unwrap() > big_int_value,
                                }}
                            }
                        },
                        _ => unreachable!("Integer range can only have integers as lower and upper range boundary value"),
                    }
                };
                Ok(is_in_upper_bound && is_in_lower_bound)
            }
            Range::Float(start, end) => {
                let value = &value.as_f64().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Float ranges can only have float value for validation",
                    )
                })?;
                let is_in_lower_bound = match start {
                    Min => true,
                    Value(start_value, boundary_type) => match start_value {
                        Float(min_value) => match boundary_type {
                            RangeBoundaryType::Inclusive => min_value <= value,
                            RangeBoundaryType::Exclusive => min_value < value,
                        },
                        _ => unreachable!("Float range can only have floats as lower and upper range boundary value"),
                    },
                    Max => unreachable!("Cannot have 'Max' as the lower range boundary")
                };

                let is_in_upper_bound = match end {
                    Max => true,
                    Min => unreachable!("Cannot have 'Min' as the upper range boundary"),
                    Value(end_value, boundary_type) => match end_value {
                        Float(max_value) => match boundary_type {
                            RangeBoundaryType::Inclusive => max_value >= value,
                            RangeBoundaryType::Exclusive => max_value > value,
                        },
                        _ => unreachable!("Float range can only have floats as lower and upper range boundary value"),
                    }
                };
                Ok(is_in_upper_bound && is_in_lower_bound)
            }
            Range::Decimal(start, end) => {
                let value = &value.as_decimal().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Decimal ranges can only have decimal value for validation",
                    )
                })?;
                let is_in_lower_bound = match start {
                    Min => true,
                    Value(start_value, boundary_type) => match start_value {
                        Decimal(min_value) => match boundary_type {
                            RangeBoundaryType::Inclusive => min_value <= value,
                            RangeBoundaryType::Exclusive => min_value < value,
                        },
                        _ => unreachable!("Decimal range can only have decimals as lower and upper range boundary value"),
                    },
                    Max => unreachable!("Cannot have 'Max' as the lower range boundary")
                };

                let is_in_upper_bound = match end {
                    Max => true,
                    Min => unreachable!("Cannot have 'Min' as the upper range boundary"),
                    Value(end_value, boundary_type) => match end_value {
                        Decimal(max_value) => match boundary_type {
                            RangeBoundaryType::Inclusive => max_value >= value,
                            RangeBoundaryType::Exclusive => max_value > value,
                        },
                        _ => unreachable!("Decimal range can only have decimals as lower and upper range boundary value"),
                    }
                };
                Ok(is_in_upper_bound && is_in_lower_bound)
            }
            Range::Timestamp(start, end) => {
                // TODO: Implement this section once the timestamp comparator for ion-rust is implemented
                todo!()
            }
        }
    }

    pub fn range(start: RangeBoundaryValue, end: RangeBoundaryValue) -> IonSchemaResult<Range> {
        use RangeBoundaryValue::*;
        use RangeBoundaryValueType::*;

        // validate the range boundary values : `start` and `end`
        let range_boundary_value_type = match (&start, &end) {
            (Min, Max) => {
                return invalid_schema_error("Range boundaries can not be min and max together (i.e. range::[min, max] is not allowed)")
            }
            (Min, Value(v, _)) => {
                v.to_owned()
            }
            (Value(v, _), Max) => {
                v.to_owned()
            }
            (Value(v1, _), Value(v2, _)) => {
                match (v1, v2) {
                    (Integer(_), Integer(_)) => {}
                    (IntegerNonNegative(_),IntegerNonNegative(_)) => {}
                    (Decimal(_), Decimal(_)) => {}
                    (Float(_), Float(_)) => {}
                    (Timestamp(_), Timestamp(_)) => {},
                    _ => {
                        return invalid_schema_error("Both range boundary values must be of same type")
                    }
                };
                // TODO: un-comment below code once the timestamp comparator for ion-rust is implemented
                // if start > end {
                //     return invalid_schema_error("Lower range boundary value can not be bigger than upper range boundary")
                // }
                v1.to_owned()
            }
            (Max, _) => {
                return invalid_schema_error("Lower range boundary value must not be max")
            }
            (_, Min) => {
                return invalid_schema_error("Upper range boundary value must not be min")
            }
        };

        // create a range based on the range_boundary_value_type from above
        Ok(match range_boundary_value_type {
            Decimal(_) => Range::Decimal(start, end),
            Integer(_) => Range::Integer(start, end),
            Float(_) => Range::Float(start, end),
            Timestamp(_) => Range::Timestamp(start, end),
            IntegerNonNegative(_) => Range::IntegerNonNegative(start, end),
        })
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

        // validate both range boundary values and returns created `Range`
        Range::range(start, end)
    }

    // helper method to which validates a non negative integer range boundary value
    pub fn validate_non_negative_integer_range_boundary_value(
        value: &AnyInt,
    ) -> IonSchemaResult<()> {
        match value.as_i64() {
            Some(v) => {
                if v >= 0 {
                    Ok(())
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
                    if !v.is_negative() {
                        Ok(())
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

    /// Provides required non negative integer range
    /// required range: `range::[1,1]`
    pub fn required() -> IonSchemaResult<Range> {
        Range::range(
            RangeBoundaryValue::int_non_negative_value(
                AnyInt::I64(1),
                RangeBoundaryType::Inclusive,
            ),
            RangeBoundaryValue::int_non_negative_value(
                AnyInt::I64(1),
                RangeBoundaryType::Inclusive,
            ),
        )
    }

    /// Provides optional non negative integer range
    /// optional range: `range::[0,1]`
    pub fn optional() -> IonSchemaResult<Range> {
        Range::range(
            RangeBoundaryValue::int_non_negative_value(
                AnyInt::I64(0),
                RangeBoundaryType::Inclusive,
            ),
            RangeBoundaryValue::int_non_negative_value(
                AnyInt::I64(1),
                RangeBoundaryType::Inclusive,
            ),
        )
    }
}

/// Provides `Range` for given `AnyInt`
impl TryFrom<&AnyInt> for Range {
    type Error = IonSchemaError;

    fn try_from(int_value: &AnyInt) -> IonSchemaResult<Self> {
        Range::range(
            RangeBoundaryValue::int_non_negative_value(
                int_value.to_owned(),
                RangeBoundaryType::Inclusive,
            ),
            RangeBoundaryValue::int_non_negative_value(
                int_value.to_owned(),
                RangeBoundaryType::Inclusive,
            ),
        )
    }
}

/// Provides `Range` for given ISL `occurs` constraint
impl TryFrom<&IslOccurs> for Range {
    type Error = IonSchemaError;

    fn try_from(isl_occurs: &IslOccurs) -> IonSchemaResult<Self> {
        match isl_occurs {
            IslOccurs::Int(int_value) => int_value.try_into(),
            IslOccurs::Range(range) => Ok(range.to_owned()),
            IslOccurs::Required => Range::required(),
            IslOccurs::Optional => Range::optional(),
        }
    }
}

/// Provides `Range` for given ISL length related constraint
impl TryFrom<&IslLength> for Range {
    type Error = IonSchemaError;

    fn try_from(isl_length: &IslLength) -> IonSchemaResult<Self> {
        match isl_length {
            IslLength::Int(int_value) => int_value.try_into(),
            IslLength::Range(range) => Ok(range.to_owned()),
        }
    }
}

/// Represents the type of range boundary value
#[derive(Debug, Clone, PartialEq)]
pub enum RangeBoundaryValueType {
    Decimal(Decimal),
    Float(f64),
    Integer(AnyInt),
    IntegerNonNegative(AnyInt),
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
    pub fn int_value(value: AnyInt, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(RangeBoundaryValueType::Integer(value), range_boundary_type)
    }
    pub fn int_non_negative_value(value: AnyInt, range_boundary_type: RangeBoundaryType) -> Self {
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
                    Range::validate_non_negative_integer_range_boundary_value(
                        value.as_any_int().unwrap(),
                    )?;
                    Ok(RangeBoundaryValue::int_non_negative_value(
                        value.as_any_int().unwrap().to_owned(),
                        range_boundary_type,
                    ))
                } else {
                    Ok(RangeBoundaryValue::int_value(
                        value.as_any_int().unwrap().to_owned(),
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
