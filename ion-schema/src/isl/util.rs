use crate::isl::isl_range::{Range, RangeBoundaryType, RangeBoundaryValue, RangeImpl, RangeType};
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use ion_rs::types::timestamp::Precision;
use ion_rs::value::owned::{text_token, Element};
use ion_rs::value::IonElement;
use ion_rs::Timestamp;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

/// Represents an annotation for `annotations` constraint.
/// Grammar: <ANNOTATION> ::= <SYMBOL>
///                | required::<SYMBOL>
///                | optional::<SYMBOL>
/// `annotations`: `<https://amzn.github.io/ion-schema/docs/spec.html#annotations>`
#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub(crate) fn is_annotation_required(value: &Element, list_level_required: bool) -> bool {
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl Display for TimestampPrecision {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self {
            TimestampPrecision::Year => write!(f, "year"),
            TimestampPrecision::Month => write!(f, "month"),
            TimestampPrecision::Day => write!(f, "day"),
            TimestampPrecision::Minute => write!(f, "minute"),
            TimestampPrecision::Second => write!(f, "second"),
            TimestampPrecision::Millisecond => write!(f, "millisecond"),
            TimestampPrecision::Microsecond => write!(f, "microsecond"),
            TimestampPrecision::Nanosecond => write!(f, "nanosecond"),
            TimestampPrecision::OtherFractionalSeconds(scale) => {
                write!(f, "fractional second (10e{})", scale * -1)
            }
        }
    }
}

/// Represents a valid value to be ued within `valid_values` constraint
/// ValidValue could either be a range or Element
/// Grammar: <VALID_VALUE> ::= <VALUE>
///                | <RANGE<TIMESTAMP>>
///                | <RANGE<NUMBER>>
/// `valid_values`: `<https://amzn.github.io/ion-schema/docs/spec.html#valid_values>`
#[derive(Debug, Clone, PartialEq)]
pub enum ValidValue {
    Range(Range),
    Element(Element),
}

impl TryFrom<&Element> for ValidValue {
    type Error = IonSchemaError;

    fn try_from(value: &Element) -> Result<Self, Self::Error> {
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

impl Display for ValidValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ValidValue::Range(range) => write!(f, "{}", range),
            ValidValue::Element(element) => write!(f, "{}", element),
        }
    }
}

/// Represent a timestamp offset
/// Offset value is stored in minutes as i32 value and unknown offset are stored as None
/// For example, "+07::00" wil be stored as 420
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimestampOffset {
    offset: Option<i32>,
}

impl TimestampOffset {
    pub fn new(offset: Option<i32>) -> Self {
        Self { offset }
    }

    pub fn offset(&self) -> &Option<i32> {
        &self.offset
    }

    // helper method to convert from a string to offset in minutes
    fn offset_minutes(s: &str, range: Range) -> IonSchemaResult<i32> {
        let int = s
            .parse::<i32>()
            .map_err(|e| invalid_schema_error_raw(format!("invalid timestamp offset {}", s)))?;
        if !range.contains(&(int as i64).into()) {
            return invalid_schema_error(format!("invalid timestamp offset {}", int));
        }
        Ok(int)
    }
}

impl TryFrom<&str> for TimestampOffset {
    type Error = IonSchemaError;

    fn try_from(string_value: &str) -> Result<Self, Self::Error> {
        // unknown offset will be stored as None
        if string_value == "-00:00" {
            Ok(TimestampOffset::new(None))
        } else {
            if string_value.len() != 6 || string_value.chars().nth(3).unwrap() != ':' {
                return invalid_schema_error(
                    "`timestamp_offset` values must be of the form \"[+|-]hh:mm\"",
                );
            }
            let sign = match string_value.chars().next().unwrap() {
                '-' => -1,
                '+' => 1,
                _ => {
                    return invalid_schema_error(format!(
                        "Unrecognized `timestamp_offset` sign '{}'",
                        string_value.chars().next().unwrap()
                    ))
                }
            };
            // translate to offset in +/- minutes
            let hours = TimestampOffset::offset_minutes(
                string_value.get(1..3).unwrap(),
                Range::NonNegativeInteger(RangeImpl::new(
                    RangeBoundaryValue::Value(0, RangeBoundaryType::Inclusive),
                    RangeBoundaryValue::Value(23, RangeBoundaryType::Inclusive),
                )?),
            )?;
            let minutes = TimestampOffset::offset_minutes(
                string_value.get(4..6).unwrap(),
                Range::NonNegativeInteger(RangeImpl::new(
                    RangeBoundaryValue::Value(0, RangeBoundaryType::Inclusive),
                    RangeBoundaryValue::Value(59, RangeBoundaryType::Inclusive),
                )?),
            )?;
            Ok(TimestampOffset::new(Some(sign * (hours * 60 + minutes))))
        }
    }
}

impl From<Option<i32>> for TimestampOffset {
    fn from(value: Option<i32>) -> Self {
        Self { offset: value }
    }
}
