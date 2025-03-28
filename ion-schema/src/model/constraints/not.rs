// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_schema_version::Versioned;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::{TypeDefinitionBuilder, VersionedTypeArgument};
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for Not {
    const CONSTRAINT_NAME: &'static str = "not";
}

/// Represents the `not` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#not
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#not
#[derive(Debug, PartialEq, Clone)]
pub struct Not {
    type_argument: TypeArgument,
}
impl_type_ref_walker!(Not, type_argument);

impl Not {
    pub(crate) fn new<T: Into<TypeArgument>>(type_argument: T) -> Self {
        Self {
            type_argument: type_argument.into(),
        }
    }

    pub fn type_argument(&self) -> &TypeArgument {
        &self.type_argument
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn not<T: Into<VersionedTypeArgument<V>>>(self, type_argument: T) -> Self {
        let constraint = Not {
            type_argument: Versioned::into_inner(type_argument.into()),
        };
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Not {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!("Implement this once the ValidationContext allows looking up a type")
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Not
where
    TypeArgument: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for Not {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}
