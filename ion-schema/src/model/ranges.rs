// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{LoaderContext, ReadFromIsl, WriteAsIsl, WriteContext};
use crate::ion_extension::ElementExtensions;
use crate::result::{invalid_schema, isl_require, IonSchemaError, IonSchemaResult};
use crate::IslVersion;
use hidden::RangeBoundType;
use ion_rs::{Annotatable, Element, SequenceWriter, ValueWriter, WriteAsIon};
use std::fmt::Debug;
use std::ops::{
    Bound, Bound::*, Range, RangeBounds, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive,
};

/// RangeBoundType is a marker trait that can only be implemented in this crate.
mod hidden {
    use std::fmt::Debug;

    pub trait RangeBoundType
    where
        Self: PartialOrd + Debug + Clone,
    {
    }
    impl<T> RangeBoundType for T where T: PartialOrd + Debug + Clone {}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct IonSchemaRange<T> {
    min: Bound<T>,
    max: Bound<T>,
}

impl<T: RangeBoundType> IonSchemaRange<T> {
    // See https://github.com/amazon-ion/ion-schema/issues/140, which should be resolved before we
    // make this public.
    pub(crate) fn try_new(min: Bound<T>, max: Bound<T>) -> Result<Self, IonSchemaError> {
        match (&min, &max) {
            (Unbounded, Unbounded) => Err(()),
            (Excluded(x), Included(y)) if x >= y => Err(()),
            (Included(x), Excluded(y)) if x >= y => Err(()),
            (Excluded(x), Excluded(y)) if x >= y => Err(()),
            (Included(x), Included(y)) if x > y => Err(()),
            _ => Ok(()),
        }
        .map_err(|_| invalid_schema!("Invalid range: {min:?} to {max:?}").into())
        .map(|_| IonSchemaRange { min, max })
    }

    // For testing only.
    fn new_unchecked(min: Bound<T>, max: Bound<T>) -> Self {
        Self::try_new(min, max).unwrap()
    }

    pub fn min(&self) -> &Bound<T> {
        &self.min
    }

    pub fn max(&self) -> &Bound<T> {
        &self.max
    }

    pub fn contains(&self, value: &T) -> bool {
        let is_above_min = match &self.min {
            Unbounded => true,
            Excluded(min) => value > min,
            Included(min) => value >= min,
        };
        let is_below_max = match &self.max {
            Unbounded => true,
            Excluded(max) => value < max,
            Included(max) => value <= max,
        };
        is_above_min && is_below_max
    }
}

impl<T> IonSchemaRange<T> {
    pub(crate) fn map_bounds<F: Fn(T) -> U, U: RangeBoundType>(
        self,
        transform: F,
    ) -> IonSchemaRange<U> {
        let IonSchemaRange { min, max } = self;
        IonSchemaRange {
            min: min.map(&transform),
            max: max.map(&transform),
        }
    }
}

impl<V: IslVersion, T: RangeBoundType + WriteAsIon> WriteAsIsl<V> for IonSchemaRange<T> {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        if ctx.minimize_ranges && self.min == self.max {
            if let Included(value) = &self.min {
                writer.write(value)?;
                return Ok(());
            }
        }
        let mut list_writer = writer.with_annotations(["range"])?.list_writer()?;
        match &self.min {
            Excluded(x) => list_writer.write(x.annotated_with(["exclusive"]))?,
            Included(x) => list_writer.write(x)?,
            Unbounded => list_writer.write_symbol("min")?,
        };
        match &self.max {
            Excluded(x) => list_writer.write(x.annotated_with(["exclusive"]))?,
            Included(x) => list_writer.write(x)?,
            Unbounded => list_writer.write_symbol("max")?,
        };
        list_writer.close()?;

        Ok(())
    }
}

const MIN: &str = "min";
const MAX: &str = "max";
const EXCLUSIVE: &str = "exclusive";
const RANGE: &str = "range";

impl<V: IslVersion, T: RangeBoundType + ReadFromIsl<V>> ReadFromIsl<V> for IonSchemaRange<T> {
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        let optional_annotation = ion.one_optional_annotation()?;
        if optional_annotation == Some(RANGE) {
            let Some(seq) = ion.as_list() else {
                invalid_schema!("range must be a non-null list; found: {ion}")?
            };

            isl_require!(seq.len() == 2 => "range must have a lower and upper bound; found: {ion}")?;
            // We can unwrap these without panicking
            let lower = seq.get(0).unwrap();
            let upper = seq.get(1).unwrap();

            let lower = match (lower.as_symbol_text(), lower.one_optional_annotation()?) {
                (Some(MIN), Some(EXCLUSIVE)) => invalid_schema!("'min' may not be exclusive")?,
                (Some(MIN), None) => Unbounded,
                (_, Some(EXCLUSIVE)) => Excluded(T::try_read(lower, ctx)?),
                (_, None) => Included(T::try_read(lower, ctx)?),
                _ => invalid_schema!("invalid annotation on range bound: {lower}")?,
            };
            let upper = match (upper.as_symbol_text(), upper.one_optional_annotation()?) {
                (Some(MAX), Some(EXCLUSIVE)) => invalid_schema!("'max' may not be exclusive")?,
                (Some(MAX), None) => Unbounded,
                (_, Some(EXCLUSIVE)) => Excluded(T::try_read(upper, ctx)?),
                (_, None) => Included(T::try_read(upper, ctx)?),
                _ => invalid_schema!("invalid annotation on range bound: {upper}")?,
            };

            Ok(IonSchemaRange::try_new(lower, upper)?)
        } else {
            isl_require!(optional_annotation.is_none() => "invalid annotation on range; found: {ion}")?;
            let value: T =
                T::try_read(ion, ctx).map_err(|_| invalid_schema!("invalid range value: {ion}"))?;
            Ok(IonSchemaRange::from(value))
        }
    }
}

impl<T: RangeBoundType> From<&T> for IonSchemaRange<T> {
    fn from(value: &T) -> Self {
        IonSchemaRange {
            min: Included(value.clone()),
            max: Included(value.clone()),
        }
    }
}
impl<T: RangeBoundType> From<T> for IonSchemaRange<T> {
    fn from(value: T) -> Self {
        IonSchemaRange {
            min: Included(value.clone()),
            max: Included(value),
        }
    }
}

// TODO: Once https://github.com/amazon-ion/ion-schema/issues/140 is resolved, revisit this and
//       possibly replace with a `TryFrom` implementation.
macro_rules! from_range {
    ($range_type:ident) => {
        impl<T: RangeBoundType> From<$range_type<T>> for IonSchemaRange<T> {
            fn from(value: $range_type<T>) -> Self {
                let min = value.start_bound().cloned();
                let max = value.end_bound().cloned();
                IonSchemaRange { min, max }
            }
        }
    };
}
from_range!(Range);
from_range!(RangeTo);
from_range!(RangeFrom);
from_range!(RangeInclusive);
from_range!(RangeToInclusive);

#[cfg(test)]
mod tests {
    use crate::internal_traits::*;
    use crate::model::ranges::IonSchemaRange;
    use crate::result::IonSchemaResult;
    use crate::ISL_2_0;
    use ion_rs::v1_0::Text;
    use ion_rs::{Element, SequenceWriter, Writer};
    use rstest::rstest;
    use std::ops::Bound;
    use std::ops::Bound::*;

    #[test]
    fn from_range() {
        let expected = IonSchemaRange::try_new(Included(1), Excluded(3)).unwrap();
        let range = 1..3;
        assert_eq!(expected, range.into())
    }

    #[test]
    fn from_range_from() {
        let expected = IonSchemaRange::try_new(Included(1), Unbounded).unwrap();
        let range = 1..;
        assert_eq!(expected, range.into())
    }

    #[test]
    fn from_range_to() {
        let expected = IonSchemaRange::try_new(Unbounded, Excluded(3)).unwrap();
        let range = ..3;
        assert_eq!(expected, range.into())
    }

    #[test]
    fn from_range_inclusive() {
        let expected = IonSchemaRange::try_new(Included(1), Included(3)).unwrap();
        let range = 1..=3;
        assert_eq!(expected, range.into())
    }

    #[test]
    fn from_range_to_inclusive() {
        let expected = IonSchemaRange::try_new(Unbounded, Included(3)).unwrap();
        let range = ..=3;
        assert_eq!(expected, range.into())
    }

    #[rstest]
    #[case::simple("range::[1, 2]", Included(1), Included(2))]
    #[case::exclusive_lower_bound("range::[exclusive::1, 2]", Excluded(1), Included(2))]
    #[case::exclusive_upper_bound("range::[1, exclusive::2]", Included(1), Excluded(2))]
    #[case::min_lower_bound("range::[min, 2]", Unbounded, Included(2))]
    #[case::max_upper_bound("range::[1, max]", Included(1), Unbounded)]
    #[case::singleton("1", Included(1), Included(1))]
    fn range_write_as_isl(
        #[case] expected_ion: &str,
        #[case] min: Bound<isize>,
        #[case] max: Bound<isize>,
    ) {
        let buffer = Vec::new();
        let mut writer = Writer::new(Text, buffer).unwrap();
        let ctx = WriteContext::<ISL_2_0>::new();

        let range = IonSchemaRange::try_new(min, max).unwrap();
        range.write_as_isl(writer.value_writer(), &ctx).unwrap();
        let output = writer.close().unwrap();

        let actual_element = Element::read_one(output);
        let expected_element = Element::read_one(expected_ion);

        assert_eq!(expected_element, actual_element);
    }

    #[rstest]
    #[case::simple("range::[1, 2]", Included(1), Included(2))]
    #[case::exclusive_lower_bound("range::[exclusive::1, 2]", Excluded(1), Included(2))]
    #[case::exclusive_upper_bound("range::[1, exclusive::2]", Included(1), Excluded(2))]
    #[case::min_lower_bound("range::[min, 2]", Unbounded, Included(2))]
    #[case::max_upper_bound("range::[1, max]", Included(1), Unbounded)]
    #[case::singleton("1", Included(1), Included(1))]
    #[case::singleton_range("range::[1, 1]", Included(1), Included(1))]
    fn range_try_read_ok(
        #[case] ion: &str,
        #[case] expected_min: Bound<usize>,
        #[case] expected_max: Bound<usize>,
    ) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = IonSchemaRange::try_read(&element, &load_ctx);
        assert_eq!(result, IonSchemaRange::try_new(expected_min, expected_max))
    }

    #[rstest]
    #[case::range_must_have_a_range_annotation("[1, 2]")]
    #[case::range_may_not_be_repeated("range::range::[1, 2]")]
    #[case::no_extraneous_annotations("range::foo::[1, 2]")]
    #[case::range_must_be_a_list("range::(1 2)")]
    #[case::range_must_be_a_list("range::1")]
    #[case::singleton_values_may_not_be_annotated("foo::1")]
    #[case::min_cannot_be_exclusive("range::[exclusive::min, 2]")]
    #[case::max_cannot_be_exclusive("range::[1, exclusive::max]")]
    #[case::lower_bound_cannot_be_max("range::[max, 1]")]
    #[case::upper_bound_cannot_be_min("range::[1, min]")]
    #[case::min_cannot_be_greater_than_max("range::[2, 1]")]
    #[case::range_cannot_be_unbounded_at_both_ends("range::[min, max]")]
    fn range_try_read_err(#[case] ion: &str) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result: IonSchemaResult<IonSchemaRange<usize>> =
            IonSchemaRange::try_read(&element, &load_ctx);
        assert!(result.is_err())
    }
}
