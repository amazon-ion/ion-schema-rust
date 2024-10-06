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
use crate::IonSchemaResult;
use crate::{invalid_schema_error, invalid_schema_error_raw, isl_require};
use ion_rs::Decimal;
use ion_rs::{Element, IonResult, ValueWriter, WriteAsIon};
use ion_rs::{IonType, Timestamp};
use num_traits::{CheckedAdd, One};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

/// An end (upper or lower) of a [`Range`].
#[derive(Debug, PartialEq, Clone)]
pub enum Limit<T> {
    /// Indicates that the end of a range has no limit or appears to have no limit.
    /// For example, when `NumberRange::lower() == Unbounded`, there is no actual limit to the lower
    /// end of the range—it is effectively negative infinity. On the other hand, for a finite type
    /// such as `i64`, when `I64Range::upper() == Unbounded`, it appears that there is no limit to
    /// the upper end of the range because then the upper limit of the range is effectively the
    /// maximum value that can be represented by `i64`.
    ///
    /// `Unbounded` is represented in Ion Schema Language as `min` or `max`, depending on the
    /// position in which it occurs.
    Min,
    Max,
    /// Indicates that the end of the range includes the given value.
    Inclusive(T),
    /// Indicates that the end of the range excludes the given value.
    Exclusive(T),
}

impl<T: PartialOrd> PartialEq<T> for Limit<T> {
    fn eq(&self, other: &T) -> bool {
        match self {
            Limit::Inclusive(x) => x == other,
            _ => false,
        }
    }
}

impl<T: PartialOrd> PartialOrd<T> for Limit<T> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        match self {
            Limit::Min => Some(Ordering::Less),
            Limit::Max => Some(Ordering::Greater),
            Limit::Inclusive(x) => x.partial_cmp(other),
            Limit::Exclusive(x) => {
                // Exclusive limits can never be equal—only lesser or greater
                match x.partial_cmp(other) {
                    Some(Ordering::Equal) => None,
                    order => order,
                }
            }
        }
    }
}

impl<T: Display> Display for Limit<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Limit::Min => "min".fmt(f),
            Limit::Max => "max".fmt(f),
            Limit::Inclusive(value) => value.fmt(f),
            Limit::Exclusive(value) => write!(f, "exclusive::{value}"),
        }
    }
}

impl<T: WriteAsIon> WriteAsIon for Limit<T> {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        match self {
            // TODO: change Unbounded to Min and Max
            Limit::Min => writer.write_symbol("min"),
            Limit::Max => writer.write_symbol("max"),
            Limit::Inclusive(t) => writer.write(t),
            Limit::Exclusive(t) => writer.with_annotations(["exclusive"])?.write(t),
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
            Limit::Min => 0,
            Limit::Inclusive(x) => *x,
            Limit::Exclusive(x) => x + 1,
            Limit::Max => unreachable!(),
        };
        let upper = match self.upper() {
            Limit::Max => usize::MAX,
            Limit::Inclusive(x) => *x,
            Limit::Exclusive(x) => x - 1,
            Limit::Min => unreachable!(),
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
impl RangeValidation<TimestampPrecision> for TimestampPrecisionRange {
    fn is_empty(start: &Limit<TimestampPrecision>, end: &Limit<TimestampPrecision>) -> bool {
        match (start, end) {
            (Limit::Inclusive(lower), Limit::Inclusive(upper)) => lower > upper,
            (Limit::Exclusive(lower), Limit::Inclusive(upper))
            | (Limit::Inclusive(lower), Limit::Exclusive(upper)) => lower >= upper,
            (Limit::Exclusive(lower), Limit::Exclusive(upper)) => {
                let start_value = lower.int_value();
                let end_value = upper.int_value();

                // Checking for e.g. range::[exclusive::1, exclusive::2] which is empty.
                let adjusted_lower = start_value + 1;
                adjusted_lower >= end_value
            }
            _ => false,
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
            isl_require!(start != Limit::Min || end != Limit::Max  => "range may not contain both 'min' and 'max'")?;
            isl_require!(start != Limit::Max => "range may not be empty (start of range may not be 'max')")?;
            isl_require!(end != Limit::Min => "range may not be empty (end of range cannot be 'min')")?;
            isl_require!(!Self::is_empty(&start, &end) => "range may not be empty")?;
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
            let other = value.clone().into();
            self.lower <= other && self.upper >= other
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

                let lower_limit = Self::read_range_bound(element, seq.get(0).unwrap(), &value_fn)?;
                let upper_limit = Self::read_range_bound(element, seq.get(1).unwrap(), &value_fn)?;

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
            value_fn: F,
        ) -> IonSchemaResult<Limit<T>> {
            let is_exclusive = boundary_element.annotations().contains("exclusive");
            isl_require!(boundary_element.annotations().len() == if is_exclusive {1} else {0} => "invalid annotation(s) on range boundary {element}")?;

            match boundary_element.as_symbol().map(|s| s.text()) {
                Some(Some("min")) => {
                    isl_require!(!is_exclusive => "'min' may not be exclusive")?;
                    Ok(Limit::Min)
                }
                Some(Some("max")) => {
                    isl_require!(!is_exclusive => "'max' may not be exclusive")?;
                    Ok(Limit::Max)
                }
                _ => {
                    let limit_value: T = value_fn(boundary_element).ok_or_else(|| {
                        invalid_schema_error_raw(format!(
                            "invalid value for range boundary: {element}"
                        ))
                    })?;
                    if is_exclusive {
                        Ok(Limit::Exclusive(limit_value))
                    } else {
                        Ok(Limit::Inclusive(limit_value))
                    }
                }
            }
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

    impl<T: PartialEq + Display> Display for Range<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self.lower {
                Limit::Inclusive(value) if self.lower == self.upper => value.fmt(f),
                _ => f.write_fmt(format_args!("range::[{},{}]", self.lower, self.upper)),
            }
        }
    }

    impl<T: WriteAsIon + PartialEq> WriteAsIon for Range<T> {
        fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
            match &self.lower {
                Limit::Inclusive(value) if self.lower == self.upper => writer.write(value),
                _ => writer
                    .with_annotations(["range"])?
                    .write([&self.lower, &self.upper]),
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
    use ion_rs::Element;
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
        case::range_with_both_limits_unbounded(Range::new(Limit::Min, Limit::Max)),
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
            Range::new(Limit::Min, Limit::Inclusive(5)),
            vec![-128, 0, 5],
            vec![6, 127],
        ),
        case::upper_is_unbounded(
            Range::new(Limit::Inclusive(5), Limit::Max),
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
        let range = range.unwrap();
        for valid_value in valid_values {
            let range_contains_result = range.contains(&valid_value);
            assert!(
                range_contains_result,
                "Value {valid_value} was not in {range}"
            )
        }
        for invalid_value in invalid_values {
            let range_contains_result = range.contains(&invalid_value);
            assert!(
                !range_contains_result,
                "Value {invalid_value} was unexpectedly in {range}"
            )
        }
    }

    #[rstest(
        case::a_very_simple_case("range::[0,1]", Range::new(Limit::Inclusive(0), Limit::Inclusive(1)).unwrap()),
        case::lower_is_unbounded("range::[min,1]", Range::new(Limit::Min, Limit::Inclusive(1)).unwrap()),
        case::upper_is_unbounded("range::[1,max]", Range::new(Limit::Inclusive(1), Limit::Max).unwrap()),
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
