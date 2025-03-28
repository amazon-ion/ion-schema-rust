// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::{IonSchemaRange, TypeDefinitionBuilder};
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for CodepointLength {
    const CONSTRAINT_NAME: &'static str = "codepoint_length";
}

/// Represents the `codepoint_length` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#codepoint_length
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#codepoint_length
#[derive(Debug, PartialEq, Clone)]
pub struct CodepointLength {
    range: IonSchemaRange<usize>,
}
impl_type_ref_walker!(CodepointLength);

impl CodepointLength {
    pub(crate) fn new<T: Into<IonSchemaRange<usize>>>(range: T) -> Self {
        Self {
            range: range.into(),
        }
    }

    pub fn range(&self) -> IonSchemaRange<usize> {
        self.range
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn codepoint_length<T: Into<IonSchemaRange<usize>>>(self, range: T) -> Self {
        let constraint = CodepointLength::new(range.into());
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for CodepointLength {
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

impl<V: IslVersion> WriteAsIsl<V> for CodepointLength {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for CodepointLength {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}
