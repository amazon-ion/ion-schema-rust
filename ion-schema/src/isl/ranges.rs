use crate::isl::util::TimestampPrecision;
use crate::{invalid_schema_error, invalid_schema_error_raw, isl_require};
use crate::{IonSchemaResult, WriteToIsl};
use ion_rs::element::writer::ElementWriter;
use ion_rs::element::Element;
use ion_rs::Decimal;
use ion_rs::{IonType, IonWriter, Timestamp};
use std::fmt::{Display, Formatter};

/// The end of a [`Range`].
#[derive(Debug, PartialEq, Clone)]
pub enum Limit<T> {
    Unbounded,
    Closed(T),
    Open(T),
}

impl<T: RangeOverType> Limit<T> {
    /// Checks if a value is above this [`Limit`], assuming that this [`Limit`] is being used as the lower end of a [`Range`].
    fn is_above<V: Into<T> + Clone>(&self, other: &V) -> bool {
        let other = &other.clone().into();
        match self {
            Limit::Unbounded => true,
            Limit::Open(this) => this > other,
            Limit::Closed(this) => this >= other,
        }
    }

    /// Checks if a value is below this [`Limit`], assuming that this [`Limit`] is being used as the upper end of a [`Range`].
    fn is_below<V: Into<T> + Clone>(&self, other: &V) -> bool {
        let other = &other.clone().into();
        match self {
            Limit::Unbounded => true,
            Limit::Open(this) => this < other,
            Limit::Closed(this) => this <= other,
        }
    }
}
impl<T: Display> Limit<T> {
    fn fmt_for_display(&self, f: &mut Formatter<'_>, unbounded_text: &str) -> std::fmt::Result {
        match self {
            Limit::Unbounded => f.write_str(unbounded_text),
            Limit::Closed(value) => value.fmt(f),
            Limit::Open(value) => write!(f, "exclusive::{value}"),
        }
    }
}

/// Defines a type that can be "ranged over" using a [`Range`].
pub trait RangeOverType: PartialEq + PartialOrd + Clone
where
    Self: Sized,
{
    /// When constructing a range, should an Open limit be adjusted into a closed limit?
    /// Default implementation is to do nothing to the limit.
    fn get_adjustment_for_open_lower_bound(self) -> Limit<Self> {
        Limit::Open(self)
    }
    /// When constructing a range, should an Open limit be adjusted into a closed limit?
    /// Default implementation is to do nothing to the limit.
    fn get_adjustment_for_open_upper_bound(self) -> Limit<Self> {
        Limit::Open(self)
    }
}

impl RangeOverType for Decimal {}
impl RangeOverType for Timestamp {}
impl RangeOverType for TimestampPrecision {}
impl RangeOverType for usize {
    fn get_adjustment_for_open_lower_bound(self) -> Limit<Self> {
        Limit::Closed(self + 1)
    }
    fn get_adjustment_for_open_upper_bound(self) -> Limit<Self> {
        Limit::Closed(self - 1)
    }
}
impl RangeOverType for u64 {
    fn get_adjustment_for_open_lower_bound(self) -> Limit<Self> {
        Limit::Closed(self + 1)
    }
    fn get_adjustment_for_open_upper_bound(self) -> Limit<Self> {
        Limit::Closed(self - 1)
    }
}
impl RangeOverType for i64 {
    fn get_adjustment_for_open_lower_bound(self) -> Limit<Self> {
        Limit::Closed(self + 1)
    }
    fn get_adjustment_for_open_upper_bound(self) -> Limit<Self> {
        Limit::Closed(self - 1)
    }
}

/// Represents an interval of values where the upper and lower ends can be open, closed, or unbounded.
///
/// At least one of the ends must be [`Limit::Open`] or [`Limit::Closed`]. A `Range` may not be
/// empty (i.e. there must be at least one value for which [`contains`] returns `true`).
#[derive(Debug, Clone, PartialEq)]
pub struct Range<T> {
    lower: Limit<T>,
    upper: Limit<T>,
}

impl<T: RangeOverType> Range<T> {
    /// Creates a new range.
    /// At least one limit must be bounded, and the range must be non-empty.
    pub fn new(start: Limit<T>, end: Limit<T>) -> IonSchemaResult<Self> {
        let start = match start {
            Limit::Open(value) => value.get_adjustment_for_open_lower_bound(),
            other => other,
        };
        let end = match end {
            Limit::Open(value) => value.get_adjustment_for_open_upper_bound(),
            other => other,
        };
        isl_require!(Self::is_bounded_and_non_empty(&start, &end) => "Range must be non-empty and bounded on at least one end.")?;
        Ok(Self {
            lower: start,
            upper: end,
        })
    }

    /// Creates a new range with inclusive endpoints.
    /// [start] must be less than or equal to [end].
    pub fn new_inclusive(start: T, end: T) -> IonSchemaResult<Self> {
        Self::new(Limit::Closed(start), Limit::Closed(end))
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

            let lower_limit = read_range_bound(element, seq.get(0).unwrap(), "min", &value_fn)?;
            let upper_limit = read_range_bound(element, seq.get(1).unwrap(), "max", &value_fn)?;

            Self::new(lower_limit, upper_limit)
        } else {
            let value = value_fn(element);
            if let Some(value) = value {
                Ok(Self::new_single_value(value))
            } else {
                invalid_schema_error(format!("Invalid value for range: {element}"))
            }
        }
    }

    /// Checks whether the given value is contained within this range.
    pub fn contains<V: Into<T> + Clone>(&self, value: &V) -> bool {
        self.lower.is_below(value) && self.upper.is_above(value)
    }

    fn is_bounded_and_non_empty(start: &Limit<T>, end: &Limit<T>) -> bool {
        match (start, end) {
            (Limit::Unbounded, Limit::Unbounded) => false,
            (Limit::Unbounded, _) => true,
            (_, Limit::Unbounded) => true,
            (Limit::Closed(lower), Limit::Closed(upper)) => lower <= upper,
            (Limit::Open(lower), Limit::Open(upper))
            | (Limit::Open(lower), Limit::Closed(upper))
            | (Limit::Closed(lower), Limit::Open(upper)) => lower < upper,
        }
    }
}

fn read_range_bound<T, F: Fn(&Element) -> Option<T>>(
    element: &Element,
    boundary_element: &Element,
    unbounded_text: &str,
    value_fn: F,
) -> IonSchemaResult<Limit<T>> {
    let limit = if boundary_element.as_symbol() == Some(&ion_rs::Symbol::from(unbounded_text)) {
        isl_require!(boundary_element.annotations().is_empty() => "'{unbounded_text}' may not have annotations: {element}")?;
        Limit::Unbounded
    } else {
        let upper_value: T = value_fn(boundary_element).ok_or_else(|| {
            invalid_schema_error_raw(format!("Invalid value for range boundary: {element}"))
        })?;
        if boundary_element.annotations().contains("exclusive") {
            isl_require!(boundary_element.annotations().len() == 1 => "Invalid annotation(s) on range boundary {element}")?;
            Limit::Open(upper_value)
        } else {
            isl_require!(boundary_element.annotations().is_empty() => "Invalid annotation(s) on range boundary {element}")?;
            Limit::Closed(upper_value)
        }
    };
    Ok(limit)
}

impl<T: Clone + PartialEq + PartialOrd + RangeOverType> From<T> for Range<T> {
    fn from(value: T) -> Self {
        Range::new_single_value(value)
    }
}

pub type UsizeRange = Range<usize>;
impl UsizeRange {
    /// Constructs an inclusive range from 0 to 1.
    pub fn zero_or_one() -> Self {
        Self::new_inclusive(0, 1).expect("This is safe to unwrap because 0 <= 1")
    }

    pub fn inclusive_endpoints(&self) -> (usize, usize) {
        let lower = match self.lower {
            Limit::Unbounded => 0,
            Limit::Closed(x) => x,
            Limit::Open(x) => x + 1,
        };
        let upper = match self.upper {
            Limit::Unbounded => usize::MAX,
            Limit::Closed(x) => x,
            Limit::Open(x) => x - 1,
        };
        (lower, upper)
    }
}

// TODO: consider changing to Range<NonZeroU64> since this is only used for the `precision` constraint
pub type U64Range = Range<u64>;
pub type I64Range = Range<i64>;
pub type NumberRange = Range<Decimal>;
pub type TimestampRange = Range<Timestamp>;
pub type TimestampPrecisionRange = Range<TimestampPrecision>;

impl<T: Into<Element> + Clone + PartialEq> WriteToIsl for &Range<T> {
    fn write_to<W: IonWriter>(&self, writer: &mut W) -> IonSchemaResult<()> {
        match &self.lower {
            Limit::Closed(value) if self.lower == self.upper => {
                writer.write_element(&value.clone().into())?;
                Ok(())
            }
            _ => {
                writer.set_annotations(["range"]);
                writer.step_in(IonType::List)?;
                match &self.lower {
                    Limit::Unbounded => writer.write_symbol("min")?,
                    Limit::Closed(value) => writer.write_element(&value.clone().into())?,
                    Limit::Open(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_element(&value.clone().into())?;
                    }
                }
                match &self.upper {
                    Limit::Unbounded => writer.write_symbol("max")?,
                    Limit::Closed(value) => writer.write_element(&value.clone().into())?,
                    Limit::Open(value) => {
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

// usize does not implement Into<Element>
// TODO: Remove after https://github.com/amazon-ion/ion-rust/issues/573 is released
impl WriteToIsl for UsizeRange {
    fn write_to<W: IonWriter>(&self, writer: &mut W) -> IonSchemaResult<()> {
        match &self.lower {
            Limit::Closed(value) if self.lower == self.upper => {
                writer.write_int(&value.to_owned().into())?;
                Ok(())
            }
            _ => {
                writer.set_annotations(["range"]);
                writer.step_in(IonType::List)?;
                match &self.lower {
                    Limit::Unbounded => writer.write_symbol("min")?,
                    Limit::Closed(value) => writer.write_int(&(*value).into())?,
                    Limit::Open(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_int(&(*value).into())?;
                    }
                }
                match &self.upper {
                    Limit::Unbounded => writer.write_symbol("max")?,
                    Limit::Closed(value) => writer.write_int(&(*value).into())?,
                    Limit::Open(value) => {
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
        match &self.lower {
            Limit::Closed(value) if self.lower == self.upper => {
                writer.write_int(&(*value).into())?;
                Ok(())
            }
            _ => {
                writer.set_annotations(["range"]);
                writer.step_in(IonType::List)?;
                match &self.lower {
                    Limit::Unbounded => writer.write_symbol("min")?,
                    Limit::Closed(value) => writer.write_int(&(*value).into())?,
                    Limit::Open(value) => {
                        writer.set_annotations(["exclusive"]);
                        writer.write_int(&(*value).into())?;
                    }
                }
                match &self.upper {
                    Limit::Unbounded => writer.write_symbol("max")?,
                    Limit::Closed(value) => writer.write_int(&(*value).into())?,
                    Limit::Open(value) => {
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

impl<T: PartialEq + Display> Display for Range<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.lower {
            Limit::Closed(value) if self.lower == self.upper => value.fmt(f),
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

#[cfg(test)]
mod tests {
    //! These test are for the generic functionality of Range.
    //! More specific test cases are handled as part of ion-schema-tests.

    use crate::isl::ranges::{Limit, Range, RangeOverType};
    use crate::IonSchemaResult;
    use ion_rs::element::Element;
    use ion_rs::types::IntAccess;
    use rstest::*;

    impl RangeOverType for i8 {
        fn get_adjustment_for_open_lower_bound(self) -> Limit<Self> {
            Limit::Closed(self + 1)
        }
        fn get_adjustment_for_open_upper_bound(self) -> Limit<Self> {
            Limit::Closed(self - 1)
        }
    }

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
            Range::<i64>::from_ion_element(&Element::read_one(range_ion).unwrap(), |e| e.as_i64());
        assert!(range.is_err());
    }

    #[rstest(
        case::range_with_both_limits_unbounded(Range::new(Limit::Unbounded, Limit::Unbounded)),
        case::range_with_lower_bound_greater_than_upper_bound(Range::new(
            Limit::Closed(3),
            Limit::Closed(1)
        )),
        case::range_with_lower_bound_greater_than_upper_bound(Range::new_inclusive(3, 1)),
        case::empty_range_with_exclusive_lower_bound(Range::new(
            Limit::Open(3),
            Limit::Closed(3)
        )),
        case::empty_range_with_exclusive_upper_bound(Range::new(
            Limit::Closed(3),
            Limit::Open(3)
        ))
    )]
    fn invalid_ranges_from_constructor(#[case] range_result: IonSchemaResult<Range<i8>>) {
        assert!(range_result.is_err());
    }

    #[rstest(
        case::lower_is_unbounded(
            Range::new(Limit::Unbounded, Limit::Closed(5)),
            vec![-128, 0, 5],
            vec![6, 127],
        ),
        case::upper_is_unbounded(
            Range::new(Limit::Closed(5), Limit::Unbounded),
            vec![5, 6, 127],
            vec![-128, 0, 4],
        ),
        case::lower_bound_is_exclusive(
            Range::new(Limit::Open(5), Limit::Closed(10)),
            vec![6, 9, 10],
            vec![0, 5, 11],
        ),
        case::upper_bound_is_exclusive(
            Range::new(Limit::Closed(5), Limit::Open(10)),
            vec![5, 6, 9],
            vec![0, 4, 10],
        )
    )]
    fn range_contains(
        #[case] range: IonSchemaResult<Range<i8>>,
        #[case] valid_values: Vec<i8>,
        #[case] invalid_values: Vec<i8>,
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
        case::a_very_simple_case("range::[0,1]", Range::new(Limit::Closed(0), Limit::Closed(1)).unwrap()),
        case::lower_is_unbounded("range::[min,1]", Range::new(Limit::Unbounded, Limit::Closed(1)).unwrap()),
        case::upper_is_unbounded("range::[1,max]", Range::new(Limit::Closed(1), Limit::Unbounded).unwrap()),
        case::lower_equals_upper("1", Range::new_single_value(1)),
        // In some cases, the "exclusive" can be elided
        case::lower_is_exclusive("range::[1,2]", Range::new(Limit::Open(0), Limit::Closed(2)).unwrap()),
        case::upper_is_exclusive("range::[0,1]", Range::new(Limit::Closed(0), Limit::Open(2)).unwrap()),
        // In some cases, the range can be elided to a number
        case::upper_is_exclusive("1", Range::new(Limit::Closed(1), Limit::Closed(1)).unwrap()),
    )]
    fn range_display(#[case] expected: &str, #[case] range: Range<i8>) {
        assert_eq!(expected, format!("{range}"));
    }
}
