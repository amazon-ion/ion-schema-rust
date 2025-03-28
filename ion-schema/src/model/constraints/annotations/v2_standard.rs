// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::*;
use crate::model::constraints::annotations::AnnotationsVariant;
use crate::model::constraints::{Annotations, ReadConstraint};
use crate::model::{TypeArgument, TypeDefinitionBuilder, VersionedTypeArgument};
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, Versioned, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

/// Represents the [annotations] constraint, standard syntax, in Ion Schema 2.0
///
/// [annotations]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#annotations
#[derive(Debug, PartialEq, Clone)]
pub struct AnnotationsV2Standard {
    annotations_type: TypeArgument,
}
impl_type_ref_walker!(AnnotationsV2Standard, annotations_type);

impl AnnotationsV2Standard {
    pub fn annotations_type(&self) -> &TypeArgument {
        &self.annotations_type
    }

    pub(super) fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: FnMut(IonSchemaElement<'top>, String) -> ControlFlow<()>,
    {
        todo!()
    }
}

impl TypeDefinitionBuilder<ISL_2_0> {
    /// Adds an Ion Schema 2.0 `annotations` constraint using the standard syntax.
    pub fn annotations_type<T: Into<VersionedTypeArgument<ISL_2_0>>>(
        self,
        annotations_type: T,
    ) -> Self {
        let constraint = AnnotationsV2Standard {
            annotations_type: Versioned::into_inner(annotations_type.into()),
        };
        self.with_constraint(
            Annotations {
                variant: AnnotationsVariant::V2Standard(constraint),
            }
            .into(),
        )
    }
}

impl ValidateInternal for AnnotationsV2Standard {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!()
    }
}

impl WriteAsIsl<ISL_1_0> for AnnotationsV2Standard {}

impl WriteAsIsl<ISL_2_0> for AnnotationsV2Standard {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_2_0>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl ReadConstraint<ISL_2_0> for AnnotationsV2Standard {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}
