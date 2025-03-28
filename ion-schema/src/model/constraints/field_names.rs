// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::*;
use crate::ion_schema_version::Versioned;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::{TypeDefinitionBuilder, VersionedTypeArgument};
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for FieldNames {
    const CONSTRAINT_NAME: &'static str = "field_names";
}

/// Represents the `field_names` constraint (ref. [ISL 2.0]).
///
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#field_names
#[derive(Debug, PartialEq, Clone)]
pub struct FieldNames {
    type_argument: TypeArgument,
    distinct: bool,
}
impl_type_ref_walker!(FieldNames, type_argument);

impl FieldNames {
    pub(crate) fn new<T: Into<TypeArgument>>(distinct: bool, type_argument: T) -> Self {
        Self {
            type_argument: type_argument.into(),
            distinct,
        }
    }

    pub fn type_argument(&self) -> &TypeArgument {
        &self.type_argument
    }

    pub fn distinct(&self) -> bool {
        self.distinct
    }
}

impl TypeDefinitionBuilder<ISL_2_0> {
    pub fn field_names<T: Into<VersionedTypeArgument<ISL_2_0>>>(self, type_argument: T) -> Self {
        let constraint = FieldNames {
            distinct: false,
            type_argument: Versioned::into_inner(type_argument.into()),
        };
        self.with_constraint(constraint.into())
    }

    pub fn field_names_distinct<T: Into<VersionedTypeArgument<ISL_2_0>>>(
        self,
        distinct: bool,
        type_argument: T,
    ) -> Self {
        let constraint = FieldNames {
            distinct,
            type_argument: Versioned::into_inner(type_argument.into()),
        };
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for FieldNames {
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

impl WriteAsIsl<ISL_1_0> for FieldNames {}

impl WriteAsIsl<ISL_2_0> for FieldNames {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_2_0>,
    ) -> IonSchemaResult<()> {
        if self.distinct {
            let writer = writer.with_annotations("distinct")?;
            TypeArgument::write_as_isl(self.type_argument(), writer, ctx)
        } else {
            TypeArgument::write_as_isl(self.type_argument(), writer, ctx)
        }
    }
}

impl ReadConstraint<ISL_1_0> for FieldNames {}

impl ReadConstraint<ISL_2_0> for FieldNames {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        let distinct = ion.annotations().contains("distinct");
        Ok(Some(FieldNames {
            distinct,
            type_argument: TypeArgument::try_read(ion, ctx)?,
        }))
    }
}
