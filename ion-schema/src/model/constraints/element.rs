// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::*;
use crate::ion_schema_version::Versioned;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::{TypeDefinitionBuilder, VersionedTypeArgument};
use crate::resolver::*;
use crate::result::{invalid_schema, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for ElementType {
    const CONSTRAINT_NAME: &'static str = "element";
}

/// Represents the `element` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// Named `ElementType` to avoid name conflict with `Element` from `ion-rs`.
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#element
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#element
#[derive(Debug, PartialEq, Clone)]
pub struct ElementType {
    type_argument: TypeArgument,
    distinct: bool,
}
impl_type_ref_walker!(ElementType, type_argument);

impl ElementType {
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

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn element<T: Into<VersionedTypeArgument<V>>>(self, type_argument: T) -> Self {
        let constraint = ElementType {
            distinct: false,
            type_argument: Versioned::into_inner(type_argument.into()),
        };
        self.with_constraint(constraint.into())
    }
}

impl TypeDefinitionBuilder<ISL_2_0> {
    pub fn element_distinct<T: Into<VersionedTypeArgument<ISL_2_0>>>(
        self,
        distinct: bool,
        type_argument: T,
    ) -> Self {
        let constraint = ElementType {
            distinct,
            type_argument: Versioned::into_inner(type_argument.into()),
        };
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for ElementType {
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

impl WriteAsIsl<ISL_1_0> for ElementType {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_1_0>,
    ) -> IonSchemaResult<()> {
        if self.distinct {
            invalid_schema!("'element: distinct:: ...' is not available in Ion Schema 1.0")
        } else {
            TypeArgument::write_as_isl(self.type_argument(), writer, ctx)
        }
    }
}

impl WriteAsIsl<ISL_2_0> for ElementType {
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

impl ReadFromIsl<ISL_1_0> for ElementType {
    fn try_read(ion: &Element, ctx: &LoaderContext<ISL_1_0>) -> IonSchemaResult<Self> {
        Ok(ElementType {
            distinct: false,
            type_argument: TypeArgument::try_read(ion, ctx)?,
        })
    }
}

impl ReadConstraint<ISL_1_0> for ElementType {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_1_0>,
    ) -> IonSchemaResult<Option<Self>> {
        Ok(Some(ElementType {
            distinct: false,
            type_argument: TypeArgument::try_read(ion, ctx)?,
        }))
    }
}

impl ReadConstraint<ISL_2_0> for ElementType {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        let distinct = ion.annotations().contains("distinct");
        Ok(Some(ElementType {
            distinct,
            type_argument: TypeArgument::try_read(ion, ctx)?,
        }))
    }
}
