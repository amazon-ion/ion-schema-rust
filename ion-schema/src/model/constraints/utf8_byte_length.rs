// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ReadFromIsl, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::ranges::IonSchemaRange;
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationInfo, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

/// Represents the `utf8_byte_length` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#utf8_byte_length
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#utf8_byte_length
#[derive(Debug, PartialEq, Clone)]
pub struct Utf8ByteLength {
    range: IonSchemaRange<usize>,
}
impl_type_ref_walker!(Utf8ByteLength);

impl Utf8ByteLength {
    pub(crate) fn new<T: Into<IonSchemaRange<usize>>>(range: T) -> Self {
        Self {
            range: range.into(),
        }
    }

    pub fn range(&self) -> IonSchemaRange<usize> {
        self.range
    }
}

impl ConstraintName for Utf8ByteLength {
    const CONSTRAINT_NAME: &'static str = "utf8_byte_length";
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn utf8_byte_length<T: Into<IonSchemaRange<usize>>>(self, range: T) -> Self {
        let constraint = Utf8ByteLength::new(range.into());
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Utf8ByteLength {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        let Some(text) = value.as_text() else {
            return recorder.accept(ViolationInfo::new(
                self.into(),
                value.clone(),
                format!(
                    "{} is invalid for 'utf8_byte_length'",
                    value.ion_schema_type()
                ),
            ));
        };
        let length = text.len();
        if !self.range.contains(&length) {
            return recorder.accept(ViolationInfo::new(
                self.into(),
                value.clone(),
                format!(
                    "expected a UTF-8 encoded byte length of {:?}; found {length}",
                    self.range
                ),
            ));
        }
        ControlFlow::Continue(())
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Utf8ByteLength {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        self.range.write_as_isl(writer, ctx)
    }
}

impl<V: IslVersion> ReadConstraint<V> for Utf8ByteLength {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        let range = IonSchemaRange::try_read(ion, ctx)?;
        Ok(Some(Utf8ByteLength { range }))
    }
}

#[cfg(test)]
mod tests {
    use crate::model::constraints::{AnyConstraint, Utf8ByteLength};
    use crate::model::ranges::IonSchemaRange;
    use crate::model::TypeDefinitionBuilder;
    use crate::result::IonSchemaResult;
    use crate::ISL_1_0;
    use std::ops::Bound;

    // Testing Note:
    //   - ReadFromIsl and WriteToIsl delegate any of the non-trivial logic to `IonSchemaRange`.
    //   - ValidateInternal is tested by ion-schema-tests

    #[test]
    fn test_builder() -> IonSchemaResult<()> {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .utf8_byte_length(5)
            .build();
        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Utf8ByteLength(Utf8ByteLength::new(
                IonSchemaRange::try_new(Bound::Included(5), Bound::Included(5))?
            ))]
        );

        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .utf8_byte_length(1..10)
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Utf8ByteLength(Utf8ByteLength::new(
                IonSchemaRange::try_new(Bound::Included(1), Bound::Excluded(10))?
            ))]
        );
        Ok(())
    }
}
