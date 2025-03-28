// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ReadFromIsl, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_extension::ElementExtensions;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::ranges::IonSchemaRange;
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationInfo, ViolationRecorder};
use ion_rs::{Element, IonResult, TimestampPrecision as TSPrecision, ValueWriter, WriteAsIon};
use std::fmt::{Debug, Display, Formatter};
use std::ops::ControlFlow;
use TimestampPrecisionValue::*;

/// Argument for the [TimestampPrecision] constraint.
#[derive(Copy, Clone, Debug, PartialOrd, PartialEq)]
pub enum TimestampPrecisionValue {
    Year = -4,
    Month = -3,
    Day = -2,
    Minute = -1,
    Second = 0,
    Millisecond = 3,
    Microsecond = 6,
    Nanosecond = 9,
}
impl TimestampPrecisionValue {
    const YEAR: &'static str = "year";
    const MONTH: &'static str = "month";
    const DAY: &'static str = "day";
    const MINUTE: &'static str = "minute";
    const SECOND: &'static str = "second";
    const MILLIS: &'static str = "millisecond";
    const MICROS: &'static str = "microsecond";
    const NANOS: &'static str = "nanosecond";

    /// Returns the [TimestampPrecisionValue] corresponding to a given [i64] value.
    ///
    /// This is for internal use only, and will panic if used incorrectly.
    fn from_i64(value: i64) -> Self {
        match value {
            -4 => Year,
            -3 => Month,
            -2 => Day,
            -1 => Minute,
            0 => Second,
            3 => Millisecond,
            6 => Microsecond,
            9 => Nanosecond,
            i => unreachable!("reaching this branch indicates a programming error; value was {i}"),
        }
    }

    fn to_i64(self) -> i64 {
        self as i64
    }

    fn as_str(&self) -> &'static str {
        match self {
            Year => TimestampPrecisionValue::YEAR,
            Month => TimestampPrecisionValue::MONTH,
            Day => TimestampPrecisionValue::DAY,
            Minute => TimestampPrecisionValue::MINUTE,
            Second => TimestampPrecisionValue::SECOND,
            Millisecond => TimestampPrecisionValue::MILLIS,
            Microsecond => TimestampPrecisionValue::MICROS,
            Nanosecond => TimestampPrecisionValue::NANOS,
        }
    }
}

impl<V: IslVersion> ReadFromIsl<V> for TimestampPrecisionValue {
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        match ion.as_symbol_text() {
            Some(TimestampPrecisionValue::YEAR) => Ok(Year),
            Some(TimestampPrecisionValue::MONTH) => Ok(Month),
            Some(TimestampPrecisionValue::DAY) => Ok(Day),
            Some(TimestampPrecisionValue::MINUTE) => Ok(Minute),
            Some(TimestampPrecisionValue::SECOND) => Ok(Second),
            Some(TimestampPrecisionValue::MILLIS) => Ok(Millisecond),
            Some(TimestampPrecisionValue::MICROS) => Ok(Microsecond),
            Some(TimestampPrecisionValue::NANOS) => Ok(Nanosecond),
            _ => invalid_schema_error(format!("not a valid timestamp precision value: {ion}")),
        }
    }
}

impl Display for TimestampPrecisionValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl WriteAsIon for TimestampPrecisionValue {
    fn write_as_ion<V: ValueWriter>(&self, writer: V) -> IonResult<()> {
        writer.write_symbol(self.as_str())
    }
}

/// Represents the `timestamp_precision` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_precision
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#timestamp_precision
#[derive(Debug, PartialEq, Clone)]
pub struct TimestampPrecision {
    range: IonSchemaRange<i64>,
}
impl_type_ref_walker!(TimestampPrecision);

impl TimestampPrecision {
    pub(crate) fn new<T: Into<IonSchemaRange<TimestampPrecisionValue>>>(range: T) -> Self {
        let tsp_range: IonSchemaRange<TimestampPrecisionValue> = range.into();
        Self {
            range: tsp_range.map_bounds(|tsp| tsp.to_i64()),
        }
    }

    fn new_unchecked<T: TryInto<IonSchemaRange<TimestampPrecisionValue>>>(range: T) -> Self
    where
        <T as TryInto<IonSchemaRange<TimestampPrecisionValue>>>::Error: Debug,
    {
        let tsp_range: IonSchemaRange<TimestampPrecisionValue> = range.try_into().unwrap();
        Self {
            range: tsp_range.map_bounds(|tsp| tsp.to_i64()),
        }
    }

    pub fn range(&self) -> IonSchemaRange<TimestampPrecisionValue> {
        self.range.map_bounds(TimestampPrecisionValue::from_i64)
    }
}

impl ConstraintName for TimestampPrecision {
    const CONSTRAINT_NAME: &'static str = "timestamp_precision";
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn timestamp_precision<T: Into<IonSchemaRange<TimestampPrecisionValue>>>(
        self,
        range: T,
    ) -> Self {
        let constraint = TimestampPrecision::new(range);
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for TimestampPrecision {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        let Some(timestamp) = value.as_timestamp() else {
            return recorder.accept(ViolationInfo::new(
                self.into(),
                value.clone(),
                format!(
                    "{} is invalid for 'timestamp_precision'",
                    value.ion_schema_type()
                ),
            ));
        };
        let precision = match timestamp.precision() {
            TSPrecision::Year => Year.to_i64(),
            TSPrecision::Month => Month.to_i64(),
            TSPrecision::Day => Day.to_i64(),
            TSPrecision::HourAndMinute => Minute.to_i64(),
            TSPrecision::Second => {
                if let Some(fractional_precision) = timestamp.fractional_seconds_scale() {
                    fractional_precision
                } else {
                    Second.to_i64()
                }
            }
        };
        let range = self.range;
        if !range.contains(&precision) {
            return recorder.accept(ViolationInfo::new(
                self.into(),
                value.clone(),
                format!("expected a timestamp precision of {range:?}; found {timestamp:?}",),
            ));
        }
        ControlFlow::Continue(())
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TimestampPrecision {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        self.range
            .map_bounds(TimestampPrecisionValue::from_i64)
            .write_as_isl(writer, ctx)
    }
}

impl<V: IslVersion> ReadConstraint<V> for TimestampPrecision {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        let range = IonSchemaRange::try_read(ion, ctx)?;
        Ok(Some(TimestampPrecision::new(range)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::internal_traits::{LoaderContext, WriteContext};

    use std::collections::Bound;

    use crate::model::ranges::IonSchemaRange;
    use crate::model::TypeDefinitionBuilder;
    use crate::result::IonSchemaResult;
    use crate::{ISL_1_0, ISL_2_0};
    use ion_rs::v1_0::Text;
    use ion_rs::{Element, SequenceWriter, Writer};
    use rstest::rstest;
    // Most of the range logic is covered in IonSchemaRange test cases.
    // These primarily cover the things that are specific to timestamp precision.
    // Validation will be covered by ion-schema-tests

    #[test]
    fn test_builder() -> IonSchemaResult<()> {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .timestamp_precision(Year..=Day)
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![TimestampPrecision::new(IonSchemaRange::try_new(
                Bound::Included(Year),
                Bound::Included(Day)
            )?)
            .into()]
        );
        Ok(())
    }

    #[rstest]
    #[case::year("year", TimestampPrecision::new_unchecked(Year))]
    #[case::year("month", TimestampPrecision::new_unchecked(Month))]
    #[case::year("day", TimestampPrecision::new_unchecked(Day))]
    #[case::year("minute", TimestampPrecision::new_unchecked(Minute))]
    #[case::year("second", TimestampPrecision::new_unchecked(Second))]
    #[case::year("millisecond", TimestampPrecision::new_unchecked(Millisecond))]
    #[case::year("microsecond", TimestampPrecision::new_unchecked(Microsecond))]
    #[case::year("nanosecond", TimestampPrecision::new_unchecked(Nanosecond))]
    #[case::year("range::[min, minute]", TimestampPrecision::new_unchecked(..=Minute))]
    #[case::year("range::[minute, max]", TimestampPrecision::new_unchecked(Minute..))]
    fn timestamp_precision_write_as_isl(
        #[case] expected_ion: &str,
        #[case] constraint: TimestampPrecision,
    ) {
        let buffer = Vec::new();
        let mut writer = Writer::new(Text, buffer).unwrap();
        let ctx = WriteContext::<ISL_2_0>::new();

        constraint
            .write_as_isl(writer.value_writer(), &ctx)
            .unwrap();
        let output = writer.close().unwrap();

        let actual_element = Element::read_one(output);
        let expected_element = Element::read_one(expected_ion);

        assert_eq!(expected_element, actual_element);
    }

    #[rstest]
    #[case::year("year", TimestampPrecision::new_unchecked(Year))]
    #[case::year("month", TimestampPrecision::new_unchecked(Month))]
    #[case::year("day", TimestampPrecision::new_unchecked(Day))]
    #[case::year("minute", TimestampPrecision::new_unchecked(Minute))]
    #[case::year("second", TimestampPrecision::new_unchecked(Second))]
    #[case::year("millisecond", TimestampPrecision::new_unchecked(Millisecond))]
    #[case::year("microsecond", TimestampPrecision::new_unchecked(Microsecond))]
    #[case::year("nanosecond", TimestampPrecision::new_unchecked(Nanosecond))]
    #[case::year("range::[min, minute]", TimestampPrecision::new_unchecked(..=Minute))]
    #[case::year("range::[minute, max]", TimestampPrecision::new_unchecked(Minute..))]
    #[case::year("range::[year, exclusive::minute]", TimestampPrecision::new_unchecked(Year..Minute))]
    fn timestamp_precision_try_read_ok(#[case] ion: &str, #[case] expected: TimestampPrecision) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = TimestampPrecision::read_constraint(&element, &load_ctx);
        assert_eq!(result, Ok(Some(expected)))
    }

    #[rstest]
    #[case::cannot_use_min_outside_of_range("min")]
    #[case::cannot_use_max_outside_of_range("max")]
    #[case::single_precision_cannot_be_an_integer("0")]
    #[case::precision_range_cannot_have_an_integer("range::[0, max]")]
    #[case::range_may_not_be_repeated("range::range::[day, day]")]
    #[case::no_extraneous_annotations("range::foo::[day, minute]")]
    #[case::range_must_be_a_list("range::(day minute)")]
    #[case::singleton_values_may_not_be_annotated("range::day")]
    #[case::min_cannot_be_exclusive("range::[exclusive::min, day]")]
    #[case::max_cannot_be_exclusive("range::[day, exclusive::max]")]
    #[case::lower_bound_cannot_be_max("range::[max, day]")]
    #[case::upper_bound_cannot_be_min("range::[day, min]")]
    #[case::range_cannot_be_unbounded_at_both_ends("range::[min, max]")]
    fn timestamp_precision_try_read_err(#[case] ion: &str) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result: IonSchemaResult<Option<TimestampPrecision>> =
            TimestampPrecision::read_constraint(&element, &load_ctx);
        assert!(result.is_err())
    }
}
