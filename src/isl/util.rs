use crate::result::{invalid_schema_error, IonSchemaResult};
use ion_rs::types::decimal::Decimal;
use ion_rs::types::timestamp::Timestamp;
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::{AnyInt, Element, IntAccess, Sequence, SymbolToken};
use ion_rs::IonType;
use num_bigint::BigInt;
use num_traits::Signed;
use num_traits::Zero;

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
                match (start, end) {
                    (Min, Max) => {
                        unreachable!("Range boundaries can not be min and max together (i.e. range::[min, max] is not allowed)")
                    },
                    (Min, Value(end_value, boundary_type)) => {
                        match end_value {
                            Integer(range_end_value) => {
                                return match range_end_value {
                                    AnyInt::I64(int_end_value) => {
                                        Ok(int_end_value > &value.as_any_int().unwrap().as_i64().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && int_end_value == &value.as_any_int().unwrap().as_i64().unwrap()))
                                    }
                                    AnyInt::BigInt(big_int_end_value) => {
                                        Ok(big_int_end_value > &value.as_any_int().unwrap().as_big_int().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && big_int_end_value == value.as_any_int().unwrap().as_big_int().unwrap()))
                                    }
                                }
                            },
                            _ => unreachable!("Integer range can only have integers as lower and upper range boundary value")
                        }
                    },
                    (Value(start_value, boundary_type), Max) => {
                        match start_value {
                            Integer(range_start_value) => {
                                return match range_start_value {
                                    AnyInt::I64(int_start_value) => {
                                        Ok(int_start_value < &value.as_any_int().unwrap().as_i64().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && int_start_value == &value.as_any_int().unwrap().as_i64().unwrap()))
                                    }
                                    AnyInt::BigInt(big_int_start_value) => {
                                        Ok(big_int_start_value < &value.as_any_int().unwrap().as_big_int().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && big_int_start_value == value.as_any_int().unwrap().as_big_int().unwrap()))
                                    }
                                }
                            },
                            _ => unreachable!("Integer range can only have integers as lower and upper range boundary value")
                        }
                    },
                    (Value(start_value, start_boundary_type), Value(end_value, end_boundary_type)) => {
                        match (start_value, end_value) {
                            (Integer(range_start_value), Integer(range_end_value)) => {
                                return match (range_start_value, range_end_value) {
                                    (AnyInt::I64(int_start_value), AnyInt::I64(int_end_value)) => {
                                        let range_start_validation = int_start_value < &value.as_any_int().unwrap().as_i64().unwrap() || (*start_boundary_type == RangeBoundaryType::Inclusive && int_start_value == &value.as_any_int().unwrap().as_i64().unwrap());
                                        let range_end_validation = int_end_value > &value.as_any_int().unwrap().as_i64().unwrap() || (*end_boundary_type == RangeBoundaryType::Inclusive && int_end_value == &value.as_any_int().unwrap().as_i64().unwrap());
                                        Ok(range_start_validation && range_end_validation)
                                    }
                                    (AnyInt::BigInt(big_int_end_value), AnyInt::BigInt(big_int_start_value)) => {
                                        let range_start_validation = big_int_start_value < &value.as_any_int().unwrap().as_big_int().unwrap() || (*start_boundary_type == RangeBoundaryType::Inclusive && big_int_start_value == value.as_any_int().unwrap().as_big_int().unwrap());
                                        let range_end_validation = big_int_end_value > &value.as_any_int().unwrap().as_big_int().unwrap() || (*end_boundary_type == RangeBoundaryType::Inclusive && big_int_end_value == value.as_any_int().unwrap().as_big_int().unwrap());
                                        Ok(range_start_validation && range_end_validation)
                                    }
                                    _ => unreachable!("Both range boundary value should be of same types")
                                }
                            },
                            _ => unreachable!("Integer range can only have integers as lower and upper range boundary value")
                        }
                    }
                    (Max, _) => {
                        unreachable!("Lower range boundary value must not be max")
                    }
                    (_, Min) => {
                        unreachable!("Upper range boundary value must not be min")
                    }
                }
            },
            Range::IntegerNonNegative(start, end) => {
                match (start, end) {
                    (Min, Max) => {
                        unreachable!("Range boundaries can not be min and max together (i.e. range::[min, max] is not allowed)")
                    },
                    (Min, Value(end_value, boundary_type)) => {
                        match end_value {
                            Integer(range_end_value) => {
                                return match range_end_value {
                                    AnyInt::I64(int_end_value) => {
                                        Ok(0 <= value.as_any_int().unwrap().as_i64().unwrap() && int_end_value > &value.as_any_int().unwrap().as_i64().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && int_end_value == &value.as_any_int().unwrap().as_i64().unwrap()))
                                    }
                                    AnyInt::BigInt(big_int_end_value) => {
                                        Ok(BigInt::zero() <= *value.as_any_int().unwrap().as_big_int().unwrap() && big_int_end_value > &value.as_any_int().unwrap().as_big_int().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && big_int_end_value == value.as_any_int().unwrap().as_big_int().unwrap()))
                                    }
                                }
                            },
                            _ => unreachable!("Integer range can only have integers as lower and upper range boundary value")
                        }
                    },
                    (Value(start_value, boundary_type), Max) => {
                        match start_value {
                            Integer(range_start_value) => {
                                return match range_start_value {
                                    AnyInt::I64(int_start_value) => {
                                        Ok(int_start_value < &value.as_any_int().unwrap().as_i64().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && int_start_value == &value.as_any_int().unwrap().as_i64().unwrap()))
                                    }
                                    AnyInt::BigInt(big_int_start_value) => {
                                        Ok(big_int_start_value < &value.as_any_int().unwrap().as_big_int().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && big_int_start_value == value.as_any_int().unwrap().as_big_int().unwrap()))
                                    }
                                }
                            },
                            _ => unreachable!("Integer range can only have integers as lower and upper range boundary value")
                        }
                    },
                    (Value(start_value, start_boundary_type), Value(end_value, end_boundary_type)) => {
                        match (start_value, end_value) {
                            (Integer(range_start_value), Integer(range_end_value)) => {
                                return match (range_start_value, range_end_value) {
                                    (AnyInt::I64(int_start_value), AnyInt::I64(int_end_value)) => {
                                        let range_start_validation = int_start_value < &value.as_any_int().unwrap().as_i64().unwrap() || (*start_boundary_type == RangeBoundaryType::Inclusive && int_start_value == &value.as_any_int().unwrap().as_i64().unwrap());
                                        let range_end_validation = int_end_value > &value.as_any_int().unwrap().as_i64().unwrap() || (*end_boundary_type == RangeBoundaryType::Inclusive && int_end_value == &value.as_any_int().unwrap().as_i64().unwrap());
                                        Ok(range_start_validation && range_end_validation)
                                    }
                                    (AnyInt::BigInt(big_int_end_value), AnyInt::BigInt(big_int_start_value)) => {
                                        let range_start_validation = big_int_start_value < &value.as_any_int().unwrap().as_big_int().unwrap() || (*start_boundary_type == RangeBoundaryType::Inclusive && big_int_start_value == value.as_any_int().unwrap().as_big_int().unwrap());
                                        let range_end_validation = big_int_end_value > &value.as_any_int().unwrap().as_big_int().unwrap() || (*end_boundary_type == RangeBoundaryType::Inclusive && big_int_end_value == value.as_any_int().unwrap().as_big_int().unwrap());
                                        Ok(range_start_validation && range_end_validation)
                                    }
                                    _ =>
                                        unreachable!("Both range boundary value should be of same types")

                                }
                            },
                            _ => unreachable!("Integer range can only have integers as lower and upper range boundary value")
                        }
                    }
                    (Max, _) => {
                        unreachable!("Lower range boundary value must not be max")
                    }
                    (_, Min) => {
                        unreachable!("Upper range boundary value must not be min")
                    }
                }
            },
            Range::Float(start, end) => {
                match (start, end) {
                    (Min, Max) => {
                        unreachable!("Range boundaries can not be min and max together (i.e. range::[min, max] is not allowed)")
                    },
                    (Min, Value(end_value, boundary_type)) => {
                        match end_value {
                            Float(float_end_value) => {
                                Ok(float_end_value > &value.as_f64().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && float_end_value == &value.as_f64().unwrap()))
                            }
                            _ => unreachable!("Float range can only have floats as lower and upper range boundary value")
                        }
                    }
                    (Value(start_value, boundary_type), Max) => {
                        match start_value {
                            Float(float_start_value) => {
                                Ok(float_start_value < &value.as_f64().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && float_start_value == &value.as_f64().unwrap()))
                            }
                            _ => unreachable!("Float range can only have floats as lower and upper range boundary value")
                        }
                    },
                    (Value(start_value, start_boundary_type), Value(end_value, end_boundary_type)) => {
                        match (start_value, end_value) {
                            (Float(range_start_value), Float(range_end_value)) => {
                                let range_start_validation = range_start_value < &value.as_f64().unwrap() || (*start_boundary_type == RangeBoundaryType::Inclusive && range_start_value == &value.as_f64().unwrap());
                                let range_end_validation = range_end_value > &value.as_f64().unwrap() || (*end_boundary_type == RangeBoundaryType::Inclusive && range_end_value == &value.as_f64().unwrap());
                                Ok(range_start_validation && range_end_validation)
                            },
                            _ => unreachable!("Float range can only have floats as lower and upper range boundary value")
                        }
                    }
                    (Max, _) => {
                        unreachable!("Lower range boundary value must not be max")
                    }
                    (_, Min) => {
                        unreachable!("Upper range boundary value must not be min")
                    }
                }
            }
            Range::Decimal(start, end) => {
                match (start, end) {
                    (Min, Max) => {
                        unreachable!("Range boundaries can not be min and max together (i.e. range::[min, max] is not allowed)")
                    },
                    (Min, Value(end_value, boundary_type)) => {
                        match end_value {
                            Decimal(decimal_end_value) => {
                                Ok(decimal_end_value > &value.as_decimal().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && decimal_end_value == value.as_decimal().unwrap()))
                            }
                            _ => unreachable!("Decimal range can only have decimals as lower and upper range boundary value")
                        }
                    }
                    (Value(start_value, boundary_type), Max) => {
                        match start_value {
                            Decimal(decimal_start_value) => {
                                Ok(decimal_start_value < &value.as_decimal().unwrap() || (*boundary_type == RangeBoundaryType::Inclusive && decimal_start_value == value.as_decimal().unwrap()))
                            }
                            _ => unreachable!("Decimal range can only have decimals as lower and upper range boundary value")
                        }
                    },
                    (Value(start_value, start_boundary_type), Value(end_value, end_boundary_type)) => {
                        match (start_value, end_value) {
                            (Decimal(range_start_value), Decimal(range_end_value)) => {
                                let range_start_validation = range_start_value < &value.as_decimal().unwrap() || (*start_boundary_type == RangeBoundaryType::Inclusive && range_start_value == value.as_decimal().unwrap());
                                let range_end_validation = range_end_value > &value.as_decimal().unwrap() || (*end_boundary_type == RangeBoundaryType::Inclusive && range_end_value == value.as_decimal().unwrap());
                                Ok(range_start_validation && range_end_validation)
                            },
                            _ => unreachable!("Decimal range can only have decimals as lower and upper range boundary value")
                        }
                    }
                    (Max, _) => {
                        unreachable!("Lower range boundary value must not be max")
                    }
                    (_, Min) => {
                        unreachable!("Upper range boundary value must not be min")
                    }
                }
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
    fn validate_non_negative_integer_range_boundary_value(value: &AnyInt) -> IonSchemaResult<()> {
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
