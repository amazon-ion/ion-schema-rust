// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

//! Range types used in Ion Schema.
//!
//! ### Why so many range types?
//! - [`UsizeRange`] is used for `*_length` constraints and `occurs`
//! - [`U64Range`] is used for the `precision` constraint because [`Decimal`] precision is measured using `u64`.
//! - [`I64Range`] is used for `exponent` and `scale` constraints
//! - [`TimestampPrecisionRange`] is used for `timestamp_precision` constraint
//! - [`NumberRange`] and [`TimestampRange`] are used for valid_values constraint

// It would be ideal to use [`NonZeroU64`][std::num::NonZeroU64] for the `precision` constraint, but
// `NonZeroU64` is difficult to work with because it doesn't implement core ops (such as `Add`) and
// as a result, it cannot even implement the checked ops traits (such as `CheckedAdd`) from the
// `num_traits` crate. See https://github.com/rust-num/num-traits/issues/274.

use crate::isl::ranges::base::RangeValidation;
use crate::isl::util::TimestampPrecision;
use crate::{invalid_schema_error, invalid_schema_error_raw, isl_require};
use crate::{IonSchemaResult, WriteToIsl};
use ion_rs::element::writer::ElementWriter;
use ion_rs::element::Element;
use ion_rs::Decimal;
use ion_rs::{IonType, IonWriter, Timestamp};
use num_traits::{CheckedAdd, One};
use std::fmt::{Display, Formatter};

/// An end (upper or lower) of a [`Range`].
#[derive(Debug, PartialEq, Clone)]
pub enum Limit<T> {
    Unbounded,
    Inclusive(T),
    Exclusive(T),
}

impl<T: PartialOrd> Limit<T> {
    /// Checks if a value is above this [`Limit`], assuming that this [`Limit`] is being used as the lower end of a [`Range`].
    fn is_above<V: Into<T> + Clone>(&self, other: &V) -> bool {
        let other = &other.clone().into();
        match self {
            Limit::Unbounded => true,
            Limit::Exclusive(this) => this > other,
            Limit::Inclusive(this) => this >= other,
        }
    }

    /// Checks if a value is below this [`Limit`], assuming that this [`Limit`] is being used as the upper end of a [`Range`].
    fn is_below<V: Into<T> + Clone>(&self, other: &V) -> bool {
        let other = &other.clone().into();
        match self {
            Limit::Unbounded => true,
            Limit::Exclusive(this) => this < other,
            Limit::Inclusive(this) => this <= other,
        }
    }
}
impl<T: Display> Limit<T> {
    fn fmt_for_display(&self, f: &mut Formatter<'_>, unbounded_text: &str) -> std::fmt::Result {
        match self {
            Limit::Unbounded => f.write_str(unbounded_text),
            Limit::Inclusive(value) => value.fmt(f),
            Limit::Exclusive(value) => write!(f, "exclusive::{value}"),
        }
    }
}

pub type UsizeRange = base::Range<usize>;
impl RangeValidation<usize> for UsizeRange {
    fn is_empty(start: &Limit<usize>, end: &Limit<usize>) -> bool {
        base::is_int_range_empty(start, end)
    }
}
impl UsizeRange {
    /// Constructs a degenerate range that only contains 1.
    pub fn one() -> Self {
        Self::new_single_value(1)
    }

    /// Constructs an inclusive range from 0 to 1.
    pub fn zero_or_one() -> Self {
        Self::new_inclusive(0, 1).expect("This is safe to unwrap because 0 <= 1")
    }

    pub fn inclusive_endpoints(&self) -> (usize, usize) {
        // There is no danger of under/overflow because of the range is known to be non-empty, which
        // implies that the lower limit cannot be Exclusive(usize::MAX) and the upper limit cannot
        // be Exclusive(0)
        let lower = match self.lower() {
            Limit::Unbounded => 0,
            Limit::Inclusive(x) => *x,
            Limit::Exclusive(x) => x + 1,
        };
        let upper = match self.upper() {
            Limit::Unbounded => usize::MAX,
            Limit::Inclusive(x) => *x,
            Limit::Exclusive(x) => x - 1,
        };
        (lower, upper)
    }
}

pub type U64Range = base::Range<u64>;
impl RangeValidation<u64> for U64Range {
    fn is_empty(start: &Limit<u64>, end: &Limit<u64>) -> bool {
        base::is_int_range_empty(start, end)
    }
}

pub type I64Range = base::Range<i64>;
impl RangeValidation<i64> for I64Range {
    fn is_empty(start: &Limit<i64>, end: &Limit<i64>) -> bool {
        base::is_int_range_empty(start, end)
    }
}

pub type NumberRange = base::Range<Decimal>;
impl RangeValidation<Decimal> for NumberRange {}

pub type TimestampRange = base::Range<Timestamp>;
impl RangeValidation<Timestamp> for TimestampRange {}

pub type TimestampPrecisionRange = base::Range<TimestampPrecision>;
impl RangeValidation<TimestampPrecision> for TimestampPrecisionRange {}

// usize does not implement Into<Element>
// TODO: Remove after https://github.com/amazon-ion/ion-rust/issues/573 is released
impl WriteToIsl for UsizeRange {
    fn write_to<W: IonWriter>(&self, writer: &mut W) -> IonSchemaResult<()> {
        match &self.lower() {
            Limit::Inclusive(value) if self.lower() == self.upper() => {
                writer.write_int(&value.to_owned().into())?;
                Ok(())
            }
            _ => {
                writer.set_annotations(["range"]);
                writer.step_in(IonType::List)?;
                match &self.lower() {
                    Limit::Unbounded => writer.write_symbol("min")?,
                    Limit::Inclusive(value) => writer.write_int(&(*value).into())?,
                    Limit::Exclusive(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_int(&(*value).into())?;
                    }
                }
                match &self.upper() {
                    Limit::Unbounded => writer.write_symbol("max")?,
                    Limit::Inclusive(value) => writer.write_int(&(*value).into())?,
                    Limit::Exclusive(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_int(&(*value).into())?;
                    }
                }
                writer.step_out()?;
                Ok(())
            }
        }
    }
}

// u64 does not implement Into<Element>
// TODO: Remove after https://github.com/amazon-ion/ion-rust/issues/573 is released
impl WriteToIsl for U64Range {
    fn write_to<W: IonWriter>(&self, writer: &mut W) -> IonSchemaResult<()> {
        match &self.lower() {
            Limit::Inclusive(value) if self.lower() == self.upper() => {
                writer.write_int(&(*value).into())?;
                Ok(())
            }
            _ => {
                writer.set_annotations(["range"]);
                writer.step_in(IonType::List)?;
                match &self.lower() {
                    Limit::Unbounded => writer.write_symbol("min")?,
                    Limit::Inclusive(value) => writer.write_int(&(*value).into())?,
                    Limit::Exclusive(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_int(&(*value).into())?;
                    }
                }
                match &self.upper() {
                    Limit::Unbounded => writer.write_symbol("max")?,
                    Limit::Inclusive(value) => writer.write_int(&(*value).into())?,
                    Limit::Exclusive(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_int(&(*value).into())?;
                    }
                }
                writer.step_out()?;
                Ok(())
            }
        }
    }
}

/// This module contains the generic base for all of the "real" range types that we expose.
mod base {
    use super::*;

    /// Trait to allow type-specific implementations to inject type-specific logic into the
    /// constructor for [`Range`].
    pub trait RangeValidation<T: PartialOrd> {
        /// Checks if two limits would result in an empty range.
        fn is_empty(start: &Limit<T>, end: &Limit<T>) -> bool {
            match (start, end) {
                (Limit::Inclusive(lower), Limit::Inclusive(upper)) => lower > upper,
                (Limit::Exclusive(lower), Limit::Exclusive(upper))
                | (Limit::Exclusive(lower), Limit::Inclusive(upper))
                | (Limit::Inclusive(lower), Limit::Exclusive(upper)) => lower >= upper,
                _ => false,
            }
        }
    }

    /// Checks if two limits would result in an empty range of integers. Returns true if there are
    /// no integer values between `start` and `end`, taking into consideration the exclusivity of
    /// the limits.
    pub fn is_int_range_empty<T: CheckedAdd + One + PartialOrd>(
        start: &Limit<T>,
        end: &Limit<T>,
    ) -> bool {
        match (start, end) {
            (Limit::Inclusive(lower), Limit::Inclusive(upper)) => lower > upper,
            (Limit::Exclusive(lower), Limit::Inclusive(upper))
            | (Limit::Inclusive(lower), Limit::Exclusive(upper)) => lower >= upper,
            (Limit::Exclusive(lower), Limit::Exclusive(upper)) => {
                // Checking for e.g. range::[exclusive::1, exclusive::2] which is empty.
                let adjusted_lower = lower.checked_add(&T::one());
                // If the _lower_ bound wraps around when we add one, then we know it's empty.
                if adjusted_lower.is_none() {
                    return true;
                }
                adjusted_lower.unwrap() >= *upper
            }
            _ => false,
        }
    }

    /// Represents an interval of values where the upper and lower ends can be open, closed, or unbounded.
    ///
    /// At least one of the ends must be [`Limit::Exclusive`] or [`Limit::Inclusive`]. A `Range` may not be
    /// empty (i.e. there must be at least one value for which [`contains`] returns `true`).
    #[derive(Debug, Clone, PartialEq)]
    pub struct Range<T> {
        lower: Limit<T>,
        upper: Limit<T>,
    }

    // Note the trait bound! This allows us to inject a different non-empty check depending on the
    // realized type of `T`.
    impl<T: PartialOrd + Clone> Range<T>
    where
        Self: RangeValidation<T>,
    {
        /// Creates a new range.
        /// At least one limit must be bounded, and the range must be non-empty.
        pub fn new(start: Limit<T>, end: Limit<T>) -> IonSchemaResult<Self> {
            isl_require!(start != Limit::Unbounded || end != Limit::Unbounded  => "range may not contain both 'min' and 'max'")?;
            isl_require!(!Self::is_empty(&start, &end) => "")?;
            Ok(Self {
                lower: start,
                upper: end,
            })
        }

        /// Creates a new range with inclusive endpoints.
        /// [start] must be less than or equal to [end].
        pub fn new_inclusive(start: T, end: T) -> IonSchemaResult<Self> {
            Self::new(Limit::Inclusive(start), Limit::Inclusive(end))
        }

        /// Creates a new range containing exactly one value.
        pub fn new_single_value(value: T) -> Self {
            // This is safe to unwrap because we know both limits will be Closed and start == end.
            Self::new_inclusive(value.clone(), value).unwrap()
        }

        pub fn lower(&self) -> &Limit<T> {
            &self.lower
        }

        pub fn upper(&self) -> &Limit<T> {
            &self.upper
        }

        /// Checks whether the given value is contained within this range.
        pub fn contains<V: Into<T> + Clone>(&self, value: &V) -> bool {
            self.lower.is_below(value) && self.upper.is_above(value)
        }

        /// Reads a [`Range`] from an [`Element`] of Ion Schema Language.
        pub fn from_ion_element<F: Fn(&Element) -> Option<T>>(
            element: &Element,
            value_fn: F,
        ) -> IonSchemaResult<Range<T>> {
            if element.annotations().contains("range") {
                isl_require!(element.ion_type() == IonType::List => "range must be a non-null list; found: {element}")?;
                isl_require!(!element.is_null() => "range must be a non-null list; found: {element}")?;
                let seq = element.as_sequence().unwrap();
                isl_require!(seq.len() == 2 => "range must have a lower and upper bound; found: {element}")?;

                let lower_limit =
                    Self::read_range_bound(element, seq.get(0).unwrap(), "min", &value_fn)?;
                let upper_limit =
                    Self::read_range_bound(element, seq.get(1).unwrap(), "max", &value_fn)?;

                Self::new(lower_limit, upper_limit)
            } else {
                let value = value_fn(element);
                if let Some(value) = value {
                    Ok(Self::new_single_value(value))
                } else {
                    invalid_schema_error(format!("invalid value for range: {element}"))
                }
            }
        }

        fn read_range_bound<F: Fn(&Element) -> Option<T>>(
            element: &Element,
            boundary_element: &Element,
            unbounded_text: &str,
            value_fn: F,
        ) -> IonSchemaResult<Limit<T>> {
            let limit = if boundary_element.as_symbol()
                == Some(&ion_rs::Symbol::from(unbounded_text))
            {
                isl_require!(boundary_element.annotations().is_empty() => "'{unbounded_text}' may not have annotations: {element}")?;
                Limit::Unbounded
            } else {
                let upper_value: T = value_fn(boundary_element).ok_or_else(|| {
                    invalid_schema_error_raw(format!("invalid value for range boundary: {element}"))
                })?;
                if boundary_element.annotations().contains("exclusive") {
                    isl_require!(boundary_element.annotations().len() == 1 => "invalid annotation(s) on range boundary {element}")?;
                    Limit::Exclusive(upper_value)
                } else {
                    isl_require!(boundary_element.annotations().is_empty() => "invalid annotation(s) on range boundary {element}")?;
                    Limit::Inclusive(upper_value)
                }
            };
            Ok(limit)
        }
    }

    impl<T: Clone + PartialEq + PartialOrd> From<T> for Range<T>
    where
        Self: RangeValidation<T>,
    {
        fn from(value: T) -> Self {
            Range::new_single_value(value)
        }
    }

    impl<T: Into<Element> + Clone + PartialEq> WriteToIsl for &Range<T> {
        fn write_to<W: IonWriter>(&self, writer: &mut W) -> IonSchemaResult<()> {
            match &self.lower {
                Limit::Inclusive(value) if self.lower == self.upper => {
                    writer.write_element(&value.clone().into())?;
                    Ok(())
                }
                _ => {
                    writer.set_annotations(["range"]);
                    writer.step_in(IonType::List)?;
                    match &self.lower {
                        Limit::Unbounded => writer.write_symbol("min")?,
                        Limit::Inclusive(value) => writer.write_element(&value.clone().into())?,
                        Limit::Exclusive(value) => {
                            writer.set_annotations(["exclusive"]);
                            writer.write_element(&value.clone().into())?;
                        }
                    }
                    match &self.upper {
                        Limit::Unbounded => writer.write_symbol("max")?,
                        Limit::Inclusive(value) => writer.write_element(&value.clone().into())?,
                        Limit::Exclusive(value) => {
                            writer.set_annotations(["exclusive"]);
                            writer.write_element(&value.clone().into())?;
                        }
                    }
                    writer.step_out()?;
                    Ok(())
                }
            }
        }
    }

    impl<T: PartialEq + Display> Display for Range<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self.lower {
                Limit::Inclusive(value) if self.lower == self.upper => value.fmt(f),
                _ => {
                    f.write_str("range::[")?;
                    self.lower.fmt_for_display(f, "min")?;
                    f.write_str(",")?;
                    self.upper.fmt_for_display(f, "max")?;
                    f.write_str("]")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    //! These test are for the generic functionality of Range.
    //! More specific test cases are handled as part of ion-schema-tests.

    use crate::isl::ranges::base::Range;
    use crate::isl::ranges::{I64Range, Limit};
    use crate::IonSchemaResult;
    use ion_rs::element::Element;
    use ion_rs::types::IntAccess;
    use rstest::*;

    #[rstest(
        case::range_with_min_max("range::[min, max]"),
        case::range_with_max_lower_bound("range::[max, 5]"),
        case::range_with_min_upper_bound("range::[5, min]"),
        case::range_with_lower_bound_greater_than_upper_bound("range::[10, 5]"),
        case::empty_range_with_exclusive_lower_bound("range::[exclusive::5, 5]"),
        case::empty_range_with_exclusive_upper_bound("range::[5, exclusive::5]"),
        case::empty_range_with_mutually_excluding_bounds("range::[exclusive::5, exclusive::5]"),
        case::range_with_unknown_annotation("range::[0, foo::5]"),
        case::range_without_range_annotation("[5, 10]")
    )]
    fn invalid_ranges_from_isl(#[case] range_ion: &str) {
        let range =
            I64Range::from_ion_element(&Element::read_one(range_ion).unwrap(), Element::as_i64);
        assert!(range.is_err());
    }

    #[rstest(
        case::range_with_both_limits_unbounded(Range::new(Limit::Unbounded, Limit::Unbounded)),
        case::range_with_lower_bound_greater_than_upper_bound(Range::new(
            Limit::Inclusive(3),
            Limit::Inclusive(1)
        )),
        case::range_with_lower_bound_greater_than_upper_bound(Range::new_inclusive(3, 1)),
        case::empty_range_with_exclusive_lower_bound(Range::new(
            Limit::Exclusive(3),
            Limit::Inclusive(3)
        )),
        case::empty_range_with_exclusive_upper_bound(Range::new(
            Limit::Inclusive(3),
            Limit::Exclusive(3)
        ))
    )]
    fn invalid_ranges_from_constructor(#[case] range_result: IonSchemaResult<Range<i64>>) {
        assert!(range_result.is_err());
    }

    #[rstest(
        case::lower_is_unbounded(
            Range::new(Limit::Unbounded, Limit::Inclusive(5)),
            vec![-128, 0, 5],
            vec![6, 127],
        ),
        case::upper_is_unbounded(
            Range::new(Limit::Inclusive(5), Limit::Unbounded),
            vec![5, 6, 127],
            vec![-128, 0, 4],
        ),
        case::lower_bound_is_exclusive(
            Range::new(Limit::Exclusive(5), Limit::Inclusive(10)),
            vec![6, 9, 10],
            vec![0, 5, 11],
        ),
        case::upper_bound_is_exclusive(
            Range::new(Limit::Inclusive(5), Limit::Exclusive(10)),
            vec![5, 6, 9],
            vec![0, 4, 10],
        )
    )]
    fn range_contains(
        #[case] range: IonSchemaResult<Range<i64>>,
        #[case] valid_values: Vec<i64>,
        #[case] invalid_values: Vec<i64>,
    ) {
        for valid_value in valid_values {
            let range_contains_result = range.as_ref().unwrap().contains(&valid_value);
            assert!(range_contains_result)
        }
        for invalid_value in invalid_values {
            let range_contains_result = range.as_ref().unwrap().contains(&invalid_value);
            assert!(!range_contains_result)
        }
    }

    #[rstest(
        case::a_very_simple_case("range::[0,1]", Range::new(Limit::Inclusive(0), Limit::Inclusive(1)).unwrap()),
        case::lower_is_unbounded("range::[min,1]", Range::new(Limit::Unbounded, Limit::Inclusive(1)).unwrap()),
        case::upper_is_unbounded("range::[1,max]", Range::new(Limit::Inclusive(1), Limit::Unbounded).unwrap()),
        case::lower_equals_upper("1", Range::new_single_value(1)),
        case::lower_is_exclusive("range::[exclusive::0,2]", Range::new(Limit::Exclusive(0), Limit::Inclusive(2)).unwrap()),
        case::upper_is_exclusive("range::[0,exclusive::2]", Range::new(Limit::Inclusive(0), Limit::Exclusive(2)).unwrap()),
        // In some cases, the range can be elided to a number
        case::upper_is_exclusive("1", Range::new(Limit::Inclusive(1), Limit::Inclusive(1)).unwrap()),
    )]
    fn range_display(#[case] expected: &str, #[case] range: Range<i64>) {
        assert_eq!(expected, format!("{range}"));
    }
}
