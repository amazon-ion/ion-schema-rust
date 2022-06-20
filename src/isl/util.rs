use crate::external::ion_rs::Integer;
use crate::isl::util::Number as NumberValue;
use crate::isl::util::TimestampPrecision as TimestampPrecisionValue;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use ion_rs::external::bigdecimal::BigDecimal;
use ion_rs::types::decimal::Decimal;
use ion_rs::types::integer::IntAccess;
use ion_rs::types::timestamp::{Precision, Timestamp};
use ion_rs::value::owned::{text_token, OwnedElement};
use ion_rs::value::{Element, Sequence, SymbolToken};
use ion_rs::{Integer as IntegerValue, IonType};
use num_bigint::BigInt;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::str::FromStr;

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
    Number(RangeBoundaryValue, RangeBoundaryValue),
    Decimal(RangeBoundaryValue, RangeBoundaryValue),
    Float(RangeBoundaryValue, RangeBoundaryValue),
    Integer(RangeBoundaryValue, RangeBoundaryValue),
    IntegerNonNegative(RangeBoundaryValue, RangeBoundaryValue),
    Timestamp(RangeBoundaryValue, RangeBoundaryValue),
    TimestampPrecision(RangeBoundaryValue, RangeBoundaryValue),
}

impl Range {
    /// Provides a boolean value to specify whether the given value is within the range or not
    pub fn contains(&self, value: &OwnedElement) -> IonSchemaResult<bool> {
        use RangeBoundaryValue::*;
        use RangeBoundaryValueType::*;
        match self {
            Range::Integer(start, end) => {
                let value = value.as_integer().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Integer ranges can only have integer value for validation",
                    )
                })?;
                let is_in_lower_bound = match start {
                    Min => true,
                    Value(start_value, boundary_type) => match start_value {
                        Integer(min_value) => {
                            match value {
                                IntegerValue::I64(int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => &min_value.as_i64().unwrap() <= int_value,
                                    RangeBoundaryType::Exclusive => &min_value.as_i64().unwrap() < int_value,
                                }},
                                IntegerValue::BigInt(big_int_value) => { match boundary_type {
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
                                IntegerValue::I64(int_value) => { match boundary_type {
                                    RangeBoundaryType::Inclusive => &max_value.as_i64().unwrap() >= int_value,
                                    RangeBoundaryType::Exclusive => &max_value.as_i64().unwrap() > int_value,
                                }},
                                IntegerValue::BigInt(big_int_value) => { match boundary_type {
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
                let value = value.as_integer().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Integer ranges can only have integer value for validation",
                    )
                })?;

                let non_negative_integer_value =
                    Range::validate_non_negative_integer_range_boundary_value(
                        value,
                        &RangeType::NonNegativeInteger,
                    )?;
                let is_in_lower_bound = match start {
                    Min => true, // this will always be true as non_negative_integer_Value is validated to be a `usize`
                    Value(start_value, boundary_type) => match start_value {
                        IntegerNonNegative(min_value) => {
                            match boundary_type {
                                RangeBoundaryType::Inclusive => min_value <= &non_negative_integer_value,
                                RangeBoundaryType::Exclusive => min_value < &non_negative_integer_value,
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
                            match boundary_type {
                                RangeBoundaryType::Inclusive => max_value >= &non_negative_integer_value,
                                RangeBoundaryType::Exclusive => max_value > &non_negative_integer_value,
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
            Range::TimestampPrecision(start, end) => {
                let value = &value.as_timestamp().ok_or_else(|| {
                    invalid_schema_error_raw(
                        "Timestamp Precision ranges can only have timestamp value for validation",
                    )
                })?;

                let value = TimestampPrecisionValue::from_timestamp(value);
                let is_in_lower_bound = match start {
                    Min => true,
                    Value(start_value, boundary_type) => match start_value {
                        TimestampPrecision(min_value) => match boundary_type {
                            RangeBoundaryType::Inclusive => min_value <= &value,
                            RangeBoundaryType::Exclusive => min_value < &value,
                        },
                        _ => unreachable!("TimestampPrecision range can only have timestamp precisions as lower and upper range boundary value"),
                    },
                    Max => unreachable!("Cannot have 'Max' as the lower range boundary")
                };

                let is_in_upper_bound = match end {
                    Max => true,
                    Min => unreachable!("Cannot have 'Min' as the upper range boundary"),
                    Value(end_value, boundary_type) => match end_value {
                        TimestampPrecision(max_value) => match boundary_type {
                            RangeBoundaryType::Inclusive => max_value >= &value,
                            RangeBoundaryType::Exclusive => max_value > &value,
                        },
                        _ => unreachable!("TimestampPrecision range can only have timestamp precisions as lower and upper range boundary value"),
                    }
                };
                Ok(is_in_upper_bound && is_in_lower_bound)
            }
            Range::Number(start, end) => {
                let value: NumberValue = match value.ion_type() {
                    IonType::Integer => value.as_integer().unwrap().into(),
                    IonType::Float => value.as_f64().unwrap().try_into()?,
                    IonType::Decimal => value.as_decimal().unwrap().try_into()?,
                    _ => {
                        return invalid_schema_error(
                            "Number ranges can only have number value for validation",
                        );
                    }
                };

                let is_in_lower_bound = match start {
                    Min => true,
                    Value(start_value, boundary_type) => match start_value {
                        Number(min_value) => {
                            match boundary_type {
                            RangeBoundaryType::Inclusive => min_value.big_decimal_value() <= value.big_decimal_value(),
                            RangeBoundaryType::Exclusive => min_value.big_decimal_value() < value.big_decimal_value(),
                        }
                        },
                        _ => unreachable!("Number range can only have numbers as lower and upper range boundary value"),
                    },
                    Max => unreachable!("Cannot have 'Max' as the lower range boundary")
                };

                let is_in_upper_bound = match end {
                    Max => true,
                    Min => unreachable!("Cannot have 'Min' as the upper range boundary"),
                    Value(end_value, boundary_type) => match end_value {
                        Number(max_value) => {
                                match boundary_type {
                                    RangeBoundaryType::Inclusive => max_value.big_decimal_value() >= value.big_decimal_value(),
                                    RangeBoundaryType::Exclusive => max_value.big_decimal_value() > value.big_decimal_value(),
                                }
                            }
                        _ => unreachable!("Number range can only have numbers as lower and upper range boundary value"),
                    }
                };
                Ok(is_in_upper_bound && is_in_lower_bound)
            }
        }
    }

    // allowing to use function name that is same as struct name in order to have less verbose method name
    // which is easier to understand for user
    #[allow(clippy::self_named_constructors)]
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
                    (TimestampPrecision(_), TimestampPrecision(_)) => {},
                    (Number(_), Number(_))=> {},
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
            TimestampPrecision(_) => Range::TimestampPrecision(start, end),
            Number(_) => Range::Number(start, end),
        })
    }

    pub fn from_ion_element(value: &OwnedElement, range_type: RangeType) -> IonSchemaResult<Range> {
        // if an integer value is passed here then convert it into a range
        // eg. if `1` is passed as value then return a range [1,1]
        return if let Some(integer_value) = value.as_integer() {
            match range_type {
                RangeType::Precision | RangeType::NonNegativeInteger => {
                    let non_negative_integer_value =
                        Range::validate_non_negative_integer_range_boundary_value(
                            value.as_integer().unwrap(),
                            &range_type,
                        )?;
                    Ok(non_negative_integer_value.into())
                }
                RangeType::TimestampPrecision => invalid_schema_error(format!(
                    "Timestamp precision ranges can not be constructed from value of type {}",
                    value.ion_type()
                )),
                RangeType::Any => Ok(integer_value.into()),
                RangeType::NumberOrTimestamp => {
                    Range::number_range(integer_value.into(), integer_value.into())
                }
            }
        } else if let Some(timestamp_precision) = value.as_str() {
            if range_type == RangeType::TimestampPrecision {
                timestamp_precision.try_into()
            } else {
                invalid_schema_error(format!(
                    "{:?} ranges can not be constructed from value of type {}",
                    range_type,
                    value.ion_type()
                ))
            }
        } else if let Some(range) = value.as_sequence() {
            // verify if the value has annotation range
            if !value.annotations().any(|a| a == &text_token("range")) {
                return invalid_schema_error(
                    "An element representing range must have annotation `range::` with it.",
                );
            }

            if range.len() != 2 {
                return invalid_schema_error(
                    "Ranges must contain two values representing minimum and maximum ends of range.",
                );
            }

            // set start of the range
            let start = RangeBoundaryValue::from_ion_element(try_to!(range.get(0)), &range_type)?;

            // set end of the range
            let end = RangeBoundaryValue::from_ion_element(try_to!(range.get(1)), &range_type)?;

            // validate both range boundary values and returns created `Range`
            Range::range(start, end)
        } else {
            invalid_schema_error(format!(
                "Ranges can not be constructed for type {}",
                value.ion_type()
            ))
        };
    }

    // helper method to which validates a non negative integer range boundary value
    pub fn validate_non_negative_integer_range_boundary_value(
        value: &IntegerValue,
        range_type: &RangeType,
    ) -> IonSchemaResult<usize> {
        // minimum precision must be greater than or equal to 1
        // for more information: https://amzn.github.io/ion-schema/docs/spec.html#precision
        let min_value = if range_type == &RangeType::Precision {
            1
        } else {
            0
        };
        match value.as_i64() {
            Some(v) => {
                if v >= min_value {
                    match v.try_into() {
                        Err(_) => invalid_schema_error(format!(
                            "Expected non negative integer greater than {} for range boundary values, found {}",
                            min_value, v
                        )),
                        Ok(non_negative_int_value) => Ok(non_negative_int_value),
                    }
                } else {
                    invalid_schema_error(format!(
                        "Expected non negative integer greater than {} for range boundary values, found {}",
                        min_value, v
                    ))
                }
            }
            None => match value.as_big_int() {
                None => {
                    unreachable!("Expected range boundary values must be a non negative integer")
                }
                Some(v) => {
                    if v >= &BigInt::from(min_value) {
                        match v.try_into() {
                            Err(_) => invalid_schema_error(format!(
                                "Expected non negative integer greater than {} for range boundary values, found {}",
                                min_value,v
                            )),
                            Ok(non_negative_int_value) => Ok(non_negative_int_value),
                        }
                    } else {
                        invalid_schema_error(format!(
                            "Expected non negative integer greater than {} for range boundary values, found {}",
                            min_value, v
                        ))
                    }
                }
            },
        }
    }

    /// Provides number range with given min and max values
    pub fn number_range(min_value: Number, max_value: Number) -> IonSchemaResult<Range> {
        Range::range(
            RangeBoundaryValue::number_value(min_value, RangeBoundaryType::Inclusive),
            RangeBoundaryValue::number_value(max_value, RangeBoundaryType::Inclusive),
        )
    }

    /// Provides integer range with given min and max values
    pub fn integer_range(
        min_value: IntegerValue,
        max_value: IntegerValue,
    ) -> IonSchemaResult<Range> {
        Range::range(
            RangeBoundaryValue::int_value(min_value, RangeBoundaryType::Inclusive),
            RangeBoundaryValue::int_value(max_value, RangeBoundaryType::Inclusive),
        )
    }

    /// Provides required non negative integer range
    /// required range: `range::[1,1]`
    pub fn required() -> Range {
        Range::IntegerNonNegative(
            RangeBoundaryValue::int_non_negative_value(1, RangeBoundaryType::Inclusive),
            RangeBoundaryValue::int_non_negative_value(1, RangeBoundaryType::Inclusive),
        )
    }

    /// Provides optional non negative integer range
    /// optional range: `range::[0,1]`
    pub fn optional() -> Range {
        Range::IntegerNonNegative(
            RangeBoundaryValue::int_non_negative_value(0, RangeBoundaryType::Inclusive),
            RangeBoundaryValue::int_non_negative_value(1, RangeBoundaryType::Inclusive),
        )
    }
}

/// Provides `Range` for given `usize`
impl From<usize> for Range {
    fn from(non_negative_int_value: usize) -> Self {
        Range::IntegerNonNegative(
            RangeBoundaryValue::int_non_negative_value(
                non_negative_int_value,
                RangeBoundaryType::Inclusive,
            ),
            RangeBoundaryValue::int_non_negative_value(
                non_negative_int_value,
                RangeBoundaryType::Inclusive,
            ),
        )
    }
}

/// Provides `Range` for given `Integer`
impl From<&IntegerValue> for Range {
    fn from(int_value: &IntegerValue) -> Self {
        Range::Integer(
            RangeBoundaryValue::int_value(int_value.to_owned(), RangeBoundaryType::Inclusive),
            RangeBoundaryValue::int_value(int_value.to_owned(), RangeBoundaryType::Inclusive),
        )
    }
}

/// Provides `Range` for given `&str`
impl TryFrom<&str> for Range {
    type Error = IonSchemaError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let timestamp_precision = match value {
            "year" => TimestampPrecision::Year,
            "month" => TimestampPrecision::Month,
            "day" => TimestampPrecision::Day,
            "minute" | "hour" => TimestampPrecision::Minute,
            "second" => TimestampPrecision::Second,
            "millisecond" => TimestampPrecision::Millisecond,
            "microsecond" => TimestampPrecision::Microsecond,
            "nanosecond" => TimestampPrecision::Nanosecond,
            _ => {
                return invalid_schema_error(format!(
                    "Timestamp precision value: {} is not allowed",
                    value
                ))
            }
        };
        Ok(Range::TimestampPrecision(
            RangeBoundaryValue::timestamp_precision_value(
                timestamp_precision.to_owned(),
                RangeBoundaryType::Inclusive,
            ),
            RangeBoundaryValue::timestamp_precision_value(
                timestamp_precision,
                RangeBoundaryType::Inclusive,
            ),
        ))
    }
}

/// Represents the type of range boundary value
#[derive(Debug, Clone, PartialEq)]
pub enum RangeBoundaryValueType {
    Decimal(Decimal),
    Float(f64),
    Integer(IntegerValue),
    IntegerNonNegative(usize),
    Number(Number),
    Timestamp(Timestamp),
    TimestampPrecision(TimestampPrecision),
}

/// Represents number boundary values
/// A number can be float, integer or decimal
#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    big_decimal_value: BigDecimal,
}

impl Number {
    pub fn new(big_decimal_value: BigDecimal) -> Self {
        Self { big_decimal_value }
    }

    pub fn big_decimal_value(&self) -> &BigDecimal {
        &self.big_decimal_value
    }
}

impl TryFrom<f64> for Number {
    type Error = IonSchemaError;

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        // Note: could not use BigDecimal's `try_from` method here as that uses `DIGITS` instead of `MANTISSA_DIGITS`.
        // `DIGITS` gives an approximate number of significant digits, which failed a test from ion-schema-tests test suite
        Ok(Number {
            big_decimal_value: BigDecimal::from_str(&format!(
                "{:.PRECISION$e}",
                value,
                PRECISION = f64::MANTISSA_DIGITS as usize
            ))
            .map_err(|err| {
                invalid_schema_error_raw(format!("Cannot convert f64 to BigDecimal for {}", value))
            })?,
        })
    }
}

impl TryFrom<&Decimal> for Number {
    type Error = IonSchemaError;

    fn try_from(value: &Decimal) -> Result<Self, Self::Error> {
        Ok(Number {
            big_decimal_value: value.to_owned().try_into()?,
        })
    }
}

impl From<&IntegerValue> for Number {
    fn from(value: &IntegerValue) -> Self {
        Number {
            big_decimal_value: match value {
                Integer::I64(int_val) => int_val.to_owned().into(),
                Integer::BigInt(big_int_val) => big_int_val.to_owned().into(),
            },
        }
    }
}

/// Represents a range boundary value (i.e. min, max or a value in terms of [RangeBoundaryValueType])
#[derive(Debug, Clone, PartialEq)]
pub enum RangeBoundaryValue {
    Max,
    Min,
    Value(RangeBoundaryValueType, RangeBoundaryType),
}

impl RangeBoundaryValue {
    pub fn number_value(value: Number, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(RangeBoundaryValueType::Number(value), range_boundary_type)
    }

    pub fn int_value(value: IntegerValue, range_boundary_type: RangeBoundaryType) -> Self {
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

    pub fn timestamp_precision_value(
        value: TimestampPrecision,
        range_boundary_type: RangeBoundaryType,
    ) -> Self {
        RangeBoundaryValue::Value(
            RangeBoundaryValueType::TimestampPrecision(value),
            range_boundary_type,
        )
    }

    pub fn decimal_value(value: Decimal, range_boundary_type: RangeBoundaryType) -> Self {
        RangeBoundaryValue::Value(RangeBoundaryValueType::Decimal(value), range_boundary_type)
    }

    fn from_ion_element(value: &OwnedElement, range_type: &RangeType) -> IonSchemaResult<Self> {
        let range_boundary_type = if value.annotations().any(|x| x == &text_token("exclusive")) {
            RangeBoundaryType::Exclusive
        } else {
            RangeBoundaryType::Inclusive
        };

        match value.ion_type() {
            IonType::Symbol => {
                use TimestampPrecision::*;
                let sym = try_to!(try_to!(value.as_sym()).text());
                match sym {
                    "min" => Ok(RangeBoundaryValue::Min),
                    "max" => Ok(RangeBoundaryValue::Max),
                    "year" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Year),
                        range_boundary_type,
                    )),
                    "month" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Month),
                        range_boundary_type,
                    )),
                    "day" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Day),
                        range_boundary_type,
                    )),
                    "minute" | "hour" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Minute),
                        range_boundary_type,
                    )),
                    "second" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Second),
                        range_boundary_type,
                    )),
                    "millisecond" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Millisecond),
                        range_boundary_type,
                    )),
                    "microsecond" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Microsecond),
                        range_boundary_type,
                    )),
                    "nanosecond" => Ok(RangeBoundaryValue::Value(
                        RangeBoundaryValueType::TimestampPrecision(Nanosecond),
                        range_boundary_type,
                    )),
                    _ => {
                        return invalid_schema_error(format!(
                            "Range boundary value: {} is not supported",
                            sym
                        ))
                    }
                }
            }
            IonType::Integer => match range_type {
                RangeType::Precision | RangeType::NonNegativeInteger => {
                    let non_negative_integer_value =
                        Range::validate_non_negative_integer_range_boundary_value(
                            value.as_integer().unwrap(),
                            range_type,
                        )?;
                    Ok(RangeBoundaryValue::int_non_negative_value(
                        non_negative_integer_value,
                        range_boundary_type,
                    ))
                }
                RangeType::Any => Ok(RangeBoundaryValue::int_value(
                    value.as_integer().unwrap().to_owned(),
                    range_boundary_type,
                )),
                RangeType::TimestampPrecision => invalid_schema_error(
                    "Timestamp precision ranges can not be constructed for integer boundary values",
                ),
                RangeType::NumberOrTimestamp => Ok(RangeBoundaryValue::number_value(
                    value.as_integer().unwrap().into(),
                    range_boundary_type,
                )),
            },
            IonType::Decimal => match range_type {
                RangeType::NumberOrTimestamp => Ok(RangeBoundaryValue::number_value(
                    value.as_decimal().unwrap().try_into()?,
                    range_boundary_type,
                )),
                RangeType::Any => Ok(RangeBoundaryValue::decimal_value(
                    value.as_decimal().unwrap().to_owned(),
                    range_boundary_type,
                )),
                _ => invalid_schema_error(format!(
                    "{:?} ranges can not be constructed for decimal boundary values",
                    range_type
                )),
            },
            IonType::Float => match range_type {
                RangeType::NumberOrTimestamp => Ok(RangeBoundaryValue::number_value(
                    value.as_f64().unwrap().try_into()?,
                    range_boundary_type,
                )),
                RangeType::Any => Ok(RangeBoundaryValue::float_value(
                    value.as_f64().unwrap(),
                    range_boundary_type,
                )),
                _ => invalid_schema_error(format!(
                    "{:?} ranges can not be constructed for float boundary values",
                    range_type
                )),
            },
            IonType::Timestamp => match range_type {
                RangeType::NumberOrTimestamp | RangeType::Any => {
                    Ok(RangeBoundaryValue::timestamp_value(
                        value.as_timestamp().unwrap().to_owned(),
                        range_boundary_type,
                    ))
                }
                _ => invalid_schema_error(format!(
                    "{:?} ranges can not be constructed for timestamp boundary values",
                    range_type
                )),
            },
            _ => invalid_schema_error("Unsupported range type specified"),
        }
    }
}

/// Represents the range boundary types in terms of exclusivity (i.e. inclusive or exclusive)
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RangeBoundaryType {
    Inclusive,
    Exclusive,
}

/// Represents if the range is non negative integer range or not
/// This will be used while creating an integer range from OwnedElement
/// to explicitly state if its non negative or not
#[derive(Debug, Clone, PartialEq)]
pub enum RangeType {
    Precision, // used by precision constraint to specify non negative integer precision with minimum value as `1`
    NonNegativeInteger, // used by byte_length, container_length and codepoint_length to specify non negative integer range
    TimestampPrecision, // used by timestamp_precision to specify timestamp precision range
    NumberOrTimestamp,  // used by valid_values constraint
    Any,                // used for any range types (e.g. Integer, Float, Timestamp, Decimal)
}

/// Represents an annotation for [annotations] constraint.
/// Grammar: <ANNOTATION> ::= <SYMBOL>
///                | required::<SYMBOL>
///                | optional::<SYMBOL>
/// [annotations]: https://amzn.github.io/ion-schema/docs/spec.html#annotations
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    value: String,
    is_required: bool, // Specifies whether an annotation's occurrence is required or optional
}

impl Annotation {
    pub fn new(value: String, is_required: bool) -> Self {
        Self { value, is_required }
    }

    pub fn value(&self) -> &String {
        &self.value
    }

    pub fn is_required(&self) -> bool {
        self.is_required
    }

    // Returns a bool value that represents if an annotation is required or not
    pub(crate) fn is_annotation_required(value: &OwnedElement, list_level_required: bool) -> bool {
        if value.annotations().any(|a| a.text().unwrap() == "required") {
            true
        } else if list_level_required {
            // if the value is annotated with `optional` then it overrides the list-level `required` behavior
            !value.annotations().any(|a| a.text().unwrap() == "optional")
        } else {
            // for any value the default annotation is `optional`
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TimestampPrecision {
    Year,
    Month,
    Day,
    Minute,
    Second,
    Millisecond,
    Microsecond,
    Nanosecond,
    OtherFractionalSeconds(i64),
}

impl TimestampPrecision {
    pub fn from_timestamp(timestamp_value: &Timestamp) -> TimestampPrecision {
        use TimestampPrecision::*;
        let precision_value = timestamp_value.precision();
        match precision_value {
            Precision::Year => Year,
            Precision::Month => Month,
            Precision::Day => Day,
            Precision::HourAndMinute => Minute,
            // `unwrap_or(0)` is a default to set second as timestamp precision.
            // currently `fractional_seconds_scale` doesn't return 0 for Precision::Second,
            // once that is fixed in ion-rust we can remove unwrap_or from here
            Precision::Second => match timestamp_value.fractional_seconds_scale().unwrap_or(0) {
                0 => Second,
                3 => Millisecond,
                6 => Microsecond,
                9 => Nanosecond,
                scale => OtherFractionalSeconds(scale),
            },
        }
    }
}

impl PartialOrd for TimestampPrecision {
    fn partial_cmp(&self, other: &TimestampPrecision) -> Option<Ordering> {
        use TimestampPrecision::*;
        let self_value = match self {
            Year => -4,
            Month => -3,
            Day => -2,
            Minute => -1,
            Second => 0,
            Millisecond => 3,
            Microsecond => 6,
            Nanosecond => 9,
            OtherFractionalSeconds(scale) => *scale,
        };

        let other_value = match other {
            Year => -4,
            Month => -3,
            Day => -2,
            Minute => -1,
            Second => 0,
            Millisecond => 3,
            Microsecond => 6,
            Nanosecond => 9,
            OtherFractionalSeconds(scale) => *scale,
        };

        Some(self_value.cmp(&other_value))
    }
}

/// Represents a valid value to be ued within `valid_values` constraint
/// ValidValue could either be a range or OwnedElement
/// Grammar: <VALID_VALUE> ::= <VALUE>
///                | <RANGE<TIMESTAMP>>
///                | <RANGE<NUMBER>>
/// [valid_values]: https://amzn.github.io/ion-schema/docs/spec.html#valid_values
#[derive(Debug, Clone, PartialEq)]
pub enum ValidValue {
    Range(Range),
    Element(OwnedElement),
}

impl TryFrom<&OwnedElement> for ValidValue {
    type Error = IonSchemaError;

    fn try_from(value: &OwnedElement) -> Result<Self, Self::Error> {
        let mut annotations = value.annotations();
        if annotations.any(|a| a == &text_token("range")) {
            Ok(ValidValue::Range(Range::from_ion_element(
                value,
                RangeType::NumberOrTimestamp,
            )?))
        } else if annotations.any(|a| a != &text_token("range")) {
            invalid_schema_error(
                "Annotations are not allowed for valid_values constraint except `range` annotation",
            )
        } else {
            Ok(ValidValue::Element(value.to_owned()))
        }
    }
}
