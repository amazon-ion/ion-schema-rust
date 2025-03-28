// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::{IonSchemaRange, TypeDefinitionBuilder};
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for Scale {
    const CONSTRAINT_NAME: &'static str = "scale";
}

/// Represents the `scale` constraint (ref. [ISL 1.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#scale
#[derive(Debug, PartialEq, Clone)]
pub struct Scale {
    range: IonSchemaRange<isize>,
}
impl_type_ref_walker!(Scale);

impl Scale {
    pub(crate) fn new<T: Into<IonSchemaRange<isize>>>(range: T) -> Self {
        Self {
            range: range.into(),
        }
    }

    pub fn range(&self) -> IonSchemaRange<isize> {
        self.range
    }
}

impl TypeDefinitionBuilder<ISL_1_0> {
    pub fn scale<T: Into<IonSchemaRange<isize>>>(self, range: T) -> Self {
        let constraint = Scale::new(range.into());
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Scale {
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

impl WriteAsIsl<ISL_1_0> for Scale {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_1_0>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl WriteAsIsl<ISL_2_0> for Scale {}

impl ReadConstraint<ISL_1_0> for Scale {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_1_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

impl ReadConstraint<ISL_2_0> for Scale {}
