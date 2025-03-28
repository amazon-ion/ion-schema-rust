// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::{invalid_schema_error, IonSchemaError, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::collections::HashSet;
use std::ops::ControlFlow;

impl ConstraintName for TimestampOffset {
    const CONSTRAINT_NAME: &'static str = "timestamp_offset";
}

/// Represents the `timestamp_offset` constraint (ref.  [ISL 1.0],[ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_offset
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#timestamp_offset
#[derive(Debug, Clone, PartialEq)]
pub struct TimestampOffset {
    values: HashSet<TimestampOffsetValue>,
}
impl_type_ref_walker!(TimestampOffset);

impl TimestampOffset {
    fn new(values: Vec<TimestampOffsetValue>) -> Self {
        Self {
            values: values.into_iter().collect(),
        }
    }

    pub fn values(&self) -> impl Iterator<Item = &TimestampOffsetValue> {
        self.values.iter()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn timestamp_offset<
        T: TryInto<TimestampOffsetValue, Error = IonSchemaError>,
        I: IntoIterator<Item = T>,
    >(
        self,
        values: I,
    ) -> Self {
        let values: IonSchemaResult<Vec<_>> =
            values.into_iter().map(|value| value.try_into()).collect();
        let constraint = TimestampOffset::new(values.unwrap());
        self.with_constraint(constraint.into())
    }

    pub fn try_timestamp_offset<
        I: IntoIterator<Item = impl TryInto<TimestampOffsetValue, Error = IonSchemaError>>,
    >(
        self,
        values: I,
    ) -> IonSchemaResult<Self> {
        let values: IonSchemaResult<Vec<_>> =
            values.into_iter().map(|value| value.try_into()).collect();
        let constraint = TimestampOffset::new(values?);
        Ok(self.with_constraint(constraint.into()))
    }
}

impl ValidateInternal for TimestampOffset {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!()
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TimestampOffset {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for TimestampOffset {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

/// Represent a timestamp offset
/// Known timestamp offset value is stored in minutes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TimestampOffsetValue {
    minutes: Option<i16>,
}
impl TimestampOffsetValue {
    const UNKNOWN: TimestampOffsetValue = TimestampOffsetValue { minutes: None };
    const UTC: TimestampOffsetValue = TimestampOffsetValue { minutes: Some(0) };

    /// The total number of minutes
    fn minutes(&self) -> Option<i16> {
        self.minutes
    }
}

impl TryFrom<Option<i16>> for TimestampOffsetValue {
    type Error = IonSchemaError;

    fn try_from(value: Option<i16>) -> Result<Self, Self::Error> {
        match value {
            Some(invalid_minutes) if !(-1439..1440).contains(&invalid_minutes) => {
                invalid_schema_error(format!("timestamp offset minutes must be in the range -1439..1440; was {invalid_minutes}"))
            }
            minutes => Ok(TimestampOffsetValue{minutes}),
        }
    }
}

impl TryFrom<&str> for TimestampOffsetValue {
    type Error = IonSchemaError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[cfg(test)]
mod tests {

    use crate::model::constraints::{AnyConstraint, TimestampOffset, TimestampOffsetValue};
    use crate::model::TypeDefinitionBuilder;
    use crate::ISL_1_0;

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .timestamp_offset([None, Some(-60), Some(0), Some(60), Some(120)])
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::TimestampOffset(TimestampOffset {
                values: [
                    TimestampOffsetValue { minutes: None },
                    TimestampOffsetValue { minutes: Some(-60) },
                    TimestampOffsetValue { minutes: Some(0) },
                    TimestampOffsetValue { minutes: Some(60) },
                    TimestampOffsetValue { minutes: Some(120) },
                ]
                .into_iter()
                .collect(),
            })]
        );
    }

    #[test]
    fn test_partial_eq() {
        let neg_60 = TimestampOffsetValue::try_from(Some(-60)).unwrap();
        let pos_60 = TimestampOffsetValue::try_from(Some(-60)).unwrap();
        let unknown = TimestampOffsetValue::UNKNOWN;
        let utc = TimestampOffsetValue::UTC;

        // Duplicates shouldn't matter since they are redundant.
        assert_eq!(
            TimestampOffset::new(vec![neg_60, neg_60, pos_60, pos_60]),
            TimestampOffset::new(vec![neg_60, pos_60]),
        );

        // Order shouldn't matter
        assert_eq!(
            TimestampOffset::new(vec![pos_60, neg_60, utc, unknown]),
            TimestampOffset::new(vec![neg_60, pos_60, unknown, utc]),
        );

        // Different elements should matter
        assert_ne!(
            TimestampOffset::new(vec![pos_60, neg_60, utc]),
            TimestampOffset::new(vec![neg_60, pos_60, unknown]),
        );
    }
}
