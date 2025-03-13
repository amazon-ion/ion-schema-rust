use crate::internal_traits::{
    LoaderContext, ReadFromIsl, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_extension::ElementExtensions;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::ranges::IonSchemaRange;
use crate::model::TypeDefinitionBuilder;
use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Decimal, Element, SequenceWriter, Timestamp, ValueWriter};
use ion_rs::{Value as IonValue, Value};
use std::fmt::Debug;
use std::ops::{ControlFlow, RangeBounds};

impl ConstraintName for ValidValues {
    const CONSTRAINT_NAME: &'static str = "valid_values";
}

/// The `valid_values` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#valid_values
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#valid_values
#[derive(Clone, Debug, PartialEq)]
pub struct ValidValues {
    // TODO: Use a Set once Timestamp and Decimal implement Hash
    values: Vec<ValidValuesArgument>,
}

impl ValidValues {
    pub fn values(&self) -> impl Iterator<Item = &ValidValuesArgument> {
        self.values.iter()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    /// Adds a `valid_values` constraint to this type definition.
    pub fn valid_values<T: Into<ValidValuesArgument>, I: IntoIterator<Item = T>>(
        self,
        values: I,
    ) -> Self {
        let values = values.into_iter().map(|it| it.into()).collect();
        let constraint = ValidValues { values };
        self.with_constraint(constraint.into())
    }

    /// Adds a `valid_values` constraint with a single range argument to this type definition.
    pub fn valid_number_range<
        T: Into<Decimal> + Clone,
        R: RangeBounds<T> + Into<IonSchemaRange<T>>,
    >(
        self,
        range: R,
    ) -> Self {
        let isl_range = range.into();
        let arg = ValidValuesArgument::NumericRange(isl_range.map_bounds(T::into));
        self.valid_values([arg])
    }
}

impl ValidateInternal for ValidValues {
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

impl<V: IslVersion> WriteAsIsl<V> for ValidValues {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        // Decimal Ranges cannot be collapsed to a single value because they are used in
        // ValidValues, where `1.` requires an exact match, whereas `range::[1., 1.]`
        // allows any numeric value that would be placed at 1 on a number line. Timestamp ranges
        // cannot be collapsed for the same reason.
        // Hence, we create a copy of the context for passing to the write implementation for ranges.
        let mut ctx = WriteContext::clone(ctx);
        ctx.minimize_ranges = false;

        // If it's just a single range, skip the containing list.
        if self.values.len() == 1 {
            let the_value = self.values.first().unwrap();
            match the_value {
                ValidValuesArgument::TimestampRange(range) => {
                    range.write_as_isl(writer, &ctx)?;
                    return Ok(());
                }
                ValidValuesArgument::NumericRange(range) => {
                    range.write_as_isl(writer, &ctx)?;
                    return Ok(());
                }
                ValidValuesArgument::Value(_) => {}
            }
        }

        let mut list = writer.list_writer()?;
        for v in self.values() {
            v.write_as_isl(list.value_writer(), &ctx)?;
        }
        list.close()?;
        Ok(())
    }
}

impl<V: IslVersion> ReadConstraint<V> for ValidValues {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        // It can be a single range, or a list of arguments. Either way, it must be a list.
        // First, we'll try reading it as a range.
        let values = if (ion.one_optional_annotation()?).is_some() {
            vec![read_one_range(ion, ctx)?]
        } else {
            let c: IonSchemaResult<_> = ion
                .expect_list()?
                .elements()
                .map(|ion| read_one_argument(ion, ctx))
                .collect();
            c?
        };
        Ok(Some(ValidValues { values }))
    }
}

fn read_one_argument<V: IslVersion>(
    ion: &Element,
    ctx: &LoaderContext<V>,
) -> IonSchemaResult<ValidValuesArgument> {
    if (ion.one_optional_annotation()?).is_some() {
        read_one_range(ion, ctx)
    } else {
        Ok(ValidValuesArgument::Value(ion.value().clone()))
    }
}

fn read_one_range<V: IslVersion>(
    ion: &Element,
    ctx: &LoaderContext<V>,
) -> IonSchemaResult<ValidValuesArgument> {
    let range = if let Ok(range) = IonSchemaRange::try_read(ion, ctx) {
        ValidValuesArgument::TimestampRange(range)
    } else {
        let range = IonSchemaRange::try_read(ion, ctx)?.map_bounds(|NumberRangeValue(dec)| dec);
        ValidValuesArgument::NumericRange(range)
    };
    Ok(range)
}

// ValidValuesArgument

/// An argument for the [`ValidValues`] constraint.
///
/// This implementation does not preserve the original encoding of number ranges. They are always
/// converted to decimal ranges even if the source ISL uses float or integer values.
#[derive(Debug, PartialEq, Clone)]
pub enum ValidValuesArgument {
    Value(IonValue),
    TimestampRange(IonSchemaRange<Timestamp>),
    NumericRange(IonSchemaRange<Decimal>),
}

impl<V: IslVersion> WriteAsIsl<V> for ValidValuesArgument {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        use ValidValuesArgument::*;
        match self {
            Value(v) => writer.write(v)?,
            TimestampRange(range) => range.write_as_isl(writer, ctx)?,
            NumericRange(range) => range.write_as_isl(writer, ctx)?,
        };
        Ok(())
    }
}

impl<T: Into<IonValue>> From<T> for ValidValuesArgument {
    fn from(value: T) -> Self {
        ValidValuesArgument::Value(value.into())
    }
}
impl From<IonSchemaRange<Decimal>> for ValidValuesArgument {
    fn from(value: IonSchemaRange<Decimal>) -> Self {
        ValidValuesArgument::NumericRange(value)
    }
}
impl From<IonSchemaRange<Timestamp>> for ValidValuesArgument {
    fn from(value: IonSchemaRange<Timestamp>) -> Self {
        ValidValuesArgument::TimestampRange(value)
    }
}

/// Helper type to allow reading any type of numeric value for valid values number ranges.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct NumberRangeValue(Decimal);

impl<V: IslVersion> ReadFromIsl<V> for NumberRangeValue {
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        let value = match ion.value() {
            Value::Int(i) => NumberRangeValue(Decimal::from(*i)),
            Value::Float(f) if f.is_finite() => NumberRangeValue(Decimal::try_from(*f)?),
            Value::Decimal(d) => NumberRangeValue(*d),
            other => invalid_schema_error(format!("Not a valid number range boundary: {other}"))?,
        };
        Ok(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::model::constraints::test_read_constraint;
    use crate::model::{IonSchemaRange, TypeDefinitionBuilder};
    use crate::test_harness::*;
    use crate::{ISL_1_0, ISL_2_0};
    use ion_rs::{Decimal, IonType, Value};

    #[test]
    fn test_builder_valid_values() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .valid_values([Value::Float(1.0), Value::String("foo".into())])
            .build();
        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![ValidValues {
                values: vec![Value::Float(1.0).into(), Value::String("foo".into()).into(),]
            }
            .into()]
        );

        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .valid_values(["foo", "bar"])
            .build();
        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![ValidValues {
                values: vec![
                    Value::String("foo".into()).into(),
                    Value::String("bar".into()).into(),
                ]
            }
            .into()]
        );
    }

    #[test]
    fn test_builder_valid_number_range() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .valid_number_range(1..=10)
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![ValidValues {
                values: vec![IonSchemaRange::from(Decimal::from(1)..=Decimal::from(10)).into()]
            }
            .into()]
        );
    }

    test_harness!(
        use write_as_isl;
        ISL_1_0, ISL_2_0 {
            #[case::empty(
                Ok("[]"),
                ValidValues { values: vec![] }
            )]
            #[case::single_int_value(
                Ok("[1]"),
                ValidValues { values: vec![ValidValuesArgument::from(1)] }
            )]
            #[case::single_string_value(
                Ok("[\"abc\"]"),
                ValidValues { values: vec![ValidValuesArgument::from("abc")] }
            )]
            #[case::multiple_values(
                Ok("[\"abc\", 1, null]"),
                ValidValues { values: vec![ValidValuesArgument::from("abc"), ValidValuesArgument::from(1), ValidValuesArgument::from(IonType::Null)] }
            )]
            #[case::multiple_values_with_range(
                Ok("[\"abc\", 1, range::[1., 2.]]"),
                ValidValues { values: vec![
                    ValidValuesArgument::from("abc"),
                    ValidValuesArgument::from(1),
                    ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())
                ]}
            )]
            #[case::single_number_range_should_not_be_in_a_list(
                Ok("range::[1., 2.]"),
                ValidValues { values: vec![ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())] }
            )]
            #[case::single_timestamp_range_should_not_be_in_a_list(
                Ok("range::[2024T, 2025T]"),
                ValidValues { values: vec![ValidValuesArgument::TimestampRange((Timestamp::with_year(2024).build().unwrap()..=Timestamp::with_year(2025).build().unwrap()).into())] }
            )]
            #[case::singleton_number_range_should_not_be_minimized(
                Ok("range::[1., 1.]"),
                ValidValues { values: vec![ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(1)).into())] }
            )]
            #[case::singleton_timestamp_range_should_not_be_minimized(
                Ok("range::[2025T, 2025T]"),
                ValidValues { values: vec![ValidValuesArgument::TimestampRange((Timestamp::with_year(2025).build().unwrap()..=Timestamp::with_year(2025).build().unwrap()).into())] }
            )]
        }
    );

    test_harness!(
        use test_read_constraint;
        ISL_1_0, ISL_2_0 {
            #[case::empty("[]", Ok(Some(ValidValues { values: vec![] })))]
            #[case::empty_list_annotated_with_range("range::[]", err::<Option<ValidValues>>())]
            #[case::annotated_value_in_list("[foo::1]", err::<Option<ValidValues>>())]
            #[case::single_int_value(
                "[1]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::from(1)] }))
            )]
            #[case::single_string_value(
                "[\"abc\"]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::from("abc")] }))
            )]
            #[case::multiple_values(
                "[\"abc\", 1, null]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::from("abc"), ValidValuesArgument::from(1), ValidValuesArgument::from(IonType::Null)] }))
            )]
            #[case::multiple_values_with_range(
                "[\"abc\", 1, range::[1., 2.]]",
                Ok(Some(ValidValues { values: vec![
                    ValidValuesArgument::from("abc"),
                    ValidValuesArgument::from(1),
                    ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())
                ]}))
            )]
            #[case::single_number_range_in_a_list(
                "[range::[1., 2.]]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())] }))
            )]
            #[case::single_number_range_not_in_a_list(
                "range::[1., 2.]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())] }))
            )]
            #[case::single_number_range_with_ints(
                "range::[1, 2]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())] }))
            )]
            #[case::single_number_range_with_floats(
                "range::[1e0, 2e0]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::NumericRange((Decimal::from(1)..=Decimal::from(2)).into())] }))
            )]
            #[case::single_timestamp_range(
                "range::[2024T, 2025T]",
                Ok(Some(ValidValues { values: vec![ValidValuesArgument::TimestampRange((Timestamp::with_year(2024).build().unwrap()..=Timestamp::with_year(2025).build().unwrap()).into())] }))
            )]
        }
    );
}
