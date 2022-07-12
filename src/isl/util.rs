use crate::isl::isl_range::{Range, RangeType};
use crate::result::{invalid_schema_error, IonSchemaError};
use ion_rs::types::timestamp::Precision;
use ion_rs::value::owned::{text_token, OwnedElement};
use ion_rs::value::{Element, SymbolToken};
use ion_rs::Timestamp;
use std::cmp::Ordering;

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

impl TryFrom<&str> for TimestampPrecision {
    type Error = IonSchemaError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
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
                    "Invalid timestamp precision specified {}",
                    value
                ))
            }
        })
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
        if value.annotations().any(|a| a == &text_token("range")) {
            Ok(ValidValue::Range(Range::from_ion_element(
                value,
                RangeType::NumberOrTimestamp,
            )?))
        } else if value.annotations().any(|a| a != &text_token("range")) {
            invalid_schema_error(
                "Annotations are not allowed for valid_values constraint except `range` annotation",
            )
        } else {
            Ok(ValidValue::Element(value.to_owned()))
        }
    }
}
