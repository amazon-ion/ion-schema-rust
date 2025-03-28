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

impl ConstraintName for Exponent {
    const CONSTRAINT_NAME: &'static str = "exponent";
}

/// Represents the `exponent` constraint (ref. [ISL 2.0]).
///
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#exponent
#[derive(Debug, PartialEq, Clone)]
pub struct Exponent {
    range: IonSchemaRange<isize>,
}
impl_type_ref_walker!(Exponent);

impl Exponent {
    pub(crate) fn new<T: Into<IonSchemaRange<isize>>>(range: T) -> Self {
        Self {
            range: range.into(),
        }
    }

    pub fn range(&self) -> IonSchemaRange<isize> {
        self.range
    }
}

impl TypeDefinitionBuilder<ISL_2_0> {
    pub fn exponent<T: Into<IonSchemaRange<isize>>>(self, range: T) -> Self {
        let constraint = Exponent::new(range.into());
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Exponent {
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

impl WriteAsIsl<ISL_1_0> for Exponent {}

impl WriteAsIsl<ISL_2_0> for Exponent {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_2_0>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl ReadConstraint<ISL_1_0> for Exponent {}

impl ReadConstraint<ISL_2_0> for Exponent {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}
