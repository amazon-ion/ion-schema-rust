use crate::ion_extension::ElementExtensions;
use crate::isl::ranges::{NumberRange, TimestampRange};
use crate::isl::IslVersion;
use crate::isl_require;
use crate::result::{invalid_schema_error, IonSchemaError, IonSchemaResult};
use ion_rs::TimestampPrecision as Precision;
use ion_rs::{Element, IonResult, Value, ValueWriter, WriteAsIon};
use ion_rs::{IonType, Timestamp};
use num_traits::abs;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Display, Formatter};

/// Represents an annotation for `annotations` constraint.
/// ```ion
/// Grammar: <ANNOTATION> ::= <SYMBOL>
///                | required::<SYMBOL>
///                | optional::<SYMBOL>
/// ```
/// `annotations`: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    isl_version: IslVersion,
    value: String,
    is_required: bool, // Specifies whether an annotation's occurrence is required or optional
}

impl Annotation {
    pub fn new(value: String, is_required: bool, isl_version: IslVersion) -> Self {
        Self {
            value,
            is_required,
            isl_version,
        }
    }

    pub fn value(&self) -> &String {
        &self.value
    }

    pub fn is_required(&self) -> bool {
        self.is_required
    }

    // Returns a bool value that represents if an annotation is required or not
    pub(crate) fn is_annotation_required(value: &Element, list_level_required: bool) -> bool {
        if value.annotations().contains("required") {
            true
        } else if list_level_required {
            // if the value is annotated with `optional` then it overrides the list-level `required` behavior
            !value.annotations().contains("optional")
        } else {
            // for any value the default annotation is `optional`
            false
        }
    }
}

impl WriteAsIon for Annotation {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        if self.isl_version == IslVersion::V1_0 {
            if self.is_required {
                writer
                    .with_annotations(["required"])?
                    .write_symbol(self.value.as_str())
            } else {
                writer
                    .with_annotations(["optional"])?
                    .write_symbol(self.value.as_str())
            }
        } else {
            writer.write_symbol(self.value.as_str())
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

    fn string_value(&self) -> String {
        match self {
            TimestampPrecision::Year => "year".to_string(),
            TimestampPrecision::Month => "month".to_string(),
            TimestampPrecision::Day => "day".to_string(),
            TimestampPrecision::Minute => "minute".to_string(),
            TimestampPrecision::Second => "second".to_string(),
            TimestampPrecision::Millisecond => "millisecond".to_string(),
            TimestampPrecision::Microsecond => "microsecond".to_string(),
            TimestampPrecision::Nanosecond => "nanosecond".to_string(),
            TimestampPrecision::OtherFractionalSeconds(i) => format!("fractional second (10e{i})"),
        }
    }

    pub(crate) fn int_value(&self) -> i64 {
        use TimestampPrecision::*;
        match self {
            Year => -4,
            Month => -3,
            Day => -2,
            Minute => -1,
            Second => 0,
            Millisecond => 3,
            Microsecond => 6,
            Nanosecond => 9,
            OtherFractionalSeconds(scale) => *scale,
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
            "minute" => TimestampPrecision::Minute,
            "second" => TimestampPrecision::Second,
            "millisecond" => TimestampPrecision::Millisecond,
            "microsecond" => TimestampPrecision::Microsecond,
            "nanosecond" => TimestampPrecision::Nanosecond,
            _ => {
                return invalid_schema_error(format!(
                    "Invalid timestamp precision specified {value}"
                ))
            }
        })
    }
}

impl PartialOrd for TimestampPrecision {
    fn partial_cmp(&self, other: &TimestampPrecision) -> Option<Ordering> {
        let self_value = self.int_value();
        let other_value = other.int_value();

        Some(self_value.cmp(&other_value))
    }
}

impl WriteAsIon for TimestampPrecision {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        writer.write_symbol(self.string_value().as_str())
    }
}

impl From<TimestampPrecision> for Element {
    fn from(value: TimestampPrecision) -> Self {
        Element::symbol(value.string_value())
    }
}

impl Display for TimestampPrecision {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(&self.string_value())
    }
}

/// Represents a valid value to be ued within `valid_values` constraint
/// ValidValue could either be a range or Element
/// ```ion
/// Grammar: <VALID_VALUE> ::= <VALUE>
///                | <RANGE<TIMESTAMP>>
///                | <RANGE<NUMBER>>
/// ```
/// `valid_values`: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#valid_values>`
#[derive(Debug, Clone, PartialEq)]
pub enum ValidValue {
    NumberRange(NumberRange),
    TimestampRange(TimestampRange),
    Element(Value),
}

impl ValidValue {
    pub fn from_ion_element(element: &Element, isl_version: IslVersion) -> IonSchemaResult<Self> {
        let annotation = element.annotations();
        if element.annotations().contains("range") {
            isl_require!(annotation.len() == 1 => "Unexpected annotation(s) on valid values argument: {element}")?;
            // Does it contain any timestamps
            let has_timestamp = element.as_sequence().map_or(false, |s| {
                s.elements().any(|it| it.ion_type() == IonType::Timestamp)
            });
            let range = if has_timestamp {
                ValidValue::TimestampRange(TimestampRange::from_ion_element(element, |e| {
                    e.as_timestamp()
                        .filter(|t| isl_version != IslVersion::V1_0 || t.offset().is_some())
                })?)
            } else {
                ValidValue::NumberRange(NumberRange::from_ion_element(
                    element,
                    Element::any_number_as_decimal,
                )?)
            };
            Ok(range)
        } else {
            isl_require!(annotation.is_empty() => "Unexpected annotation(s) on valid values argument: {element}")?;
            Ok(ValidValue::Element(element.value().to_owned()))
        }
    }
}

impl Display for ValidValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ValidValue::Element(element) => write!(f, "{element}"),
            ValidValue::NumberRange(r) => write!(f, "{r}"),
            ValidValue::TimestampRange(r) => write!(f, "{r}"),
        }
    }
}

impl WriteAsIon for ValidValue {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        match self {
            ValidValue::Element(value) => writer.write(value),
            ValidValue::NumberRange(r) => writer.write(r),
            ValidValue::TimestampRange(r) => writer.write(r),
        }
    }
}

impl From<NumberRange> for ValidValue {
    fn from(number_range: NumberRange) -> Self {
        ValidValue::NumberRange(number_range)
    }
}

impl From<TimestampRange> for ValidValue {
    fn from(timestamp_range: TimestampRange) -> Self {
        ValidValue::TimestampRange(timestamp_range)
    }
}

impl<T: Into<Value>> From<T> for ValidValue {
    fn from(value: T) -> Self {
        ValidValue::Element(value.into())
    }
}

/// Represent a timestamp offset
/// Known timestamp offset value is stored in minutes as i32 value
/// For example, "+07::00" wil be stored as `TimestampOffset::Known(420)`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimestampOffset {
    Known(i32), // represents known timestamp offset in minutes
    Unknown,    // represents unknown timestamp offset "-00:00"
}

impl TryFrom<&str> for TimestampOffset {
    type Error = IonSchemaError;

    fn try_from(string_value: &str) -> Result<Self, Self::Error> {
        // unknown offset will be stored as None
        if string_value == "-00:00" {
            Ok(TimestampOffset::Unknown)
        } else {
            if string_value.len() != 6 || string_value.chars().nth(3).unwrap() != ':' {
                return invalid_schema_error(
                    "`timestamp_offset` values must be of the form \"[+|-]hh:mm\"",
                );
            }
            // convert string offset value into an i32 value of offset in minutes
            let h = &string_value[1..3];
            let m = &string_value[4..6];
            let sign = match &string_value[..1] {
                "-" => -1,
                "+" => 1,
                _ => {
                    return invalid_schema_error(format!(
                        "unrecognized `timestamp_offset` sign '{}'",
                        &string_value[..1]
                    ))
                }
            };
            match (h.parse::<i32>(), m.parse::<i32>()) {
                (Ok(hours), Ok(minutes))
                    if (0..24).contains(&hours) && (0..60).contains(&minutes) =>
                {
                    Ok(TimestampOffset::Known(sign * (hours * 60 + minutes)))
                }
                _ => invalid_schema_error(format!("invalid timestamp offset {string_value}")),
            }
        }
    }
}

impl From<Option<i32>> for TimestampOffset {
    fn from(value: Option<i32>) -> Self {
        use TimestampOffset::*;
        match value {
            None => Unknown,
            Some(offset_in_minutes) => Known(offset_in_minutes),
        }
    }
}

impl Display for TimestampOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TimestampOffset::*;
        match &self {
            Unknown => write!(f, "-00:00"),
            Known(offset) => {
                let sign = if offset < &0 { "-" } else { "+" };
                let hours = abs(*offset) / 60;
                let minutes = abs(*offset) - hours * 60;
                write!(f, "{sign}{hours:02}:{minutes:02}")
            }
        }
    }
}

impl WriteAsIon for TimestampOffset {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        match &self {
            TimestampOffset::Known(offset) => {
                let sign = if offset < &0 { "-" } else { "+" };
                let hours = abs(*offset) / 60;
                let minutes = abs(*offset) - hours * 60;
                writer.write_string(format!("{sign}{hours:02}:{minutes:02}"))
            }
            TimestampOffset::Unknown => writer.write_string("-00:00"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ieee754InterchangeFormat {
    Binary16,
    Binary32,
    Binary64,
}

impl TryFrom<&str> for Ieee754InterchangeFormat {
    type Error = IonSchemaError;
    fn try_from(string_value: &str) -> Result<Self, Self::Error> {
        use Ieee754InterchangeFormat::*;
        match string_value {
            "binary16" => Ok(Binary16),
            "binary32" => Ok(Binary32),
            "binary64" => Ok(Binary64),
            _ => invalid_schema_error(format!(
                "unrecognized `ieee754_float` value {}",
                &string_value
            )),
        }
    }
}

impl Display for Ieee754InterchangeFormat {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ieee754InterchangeFormat::Binary16 => "binary16",
                Ieee754InterchangeFormat::Binary32 => "binary32",
                Ieee754InterchangeFormat::Binary64 => "binary64",
            }
        )
    }
}

impl WriteAsIon for Ieee754InterchangeFormat {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        writer.write_symbol(match self {
            Ieee754InterchangeFormat::Binary16 => "binary16",
            Ieee754InterchangeFormat::Binary32 => "binary32",
            Ieee754InterchangeFormat::Binary64 => "binary64",
        })
    }
}
