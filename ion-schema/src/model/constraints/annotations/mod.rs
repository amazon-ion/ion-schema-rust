// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod v1;
mod v2_simple;
mod v2_standard;

pub use v1::*;
pub use v2_simple::*;
pub use v2_standard::*;

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationInfo, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, IonType, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for Annotations {
    const CONSTRAINT_NAME: &'static str = "annotations";
}

/// Represents the `annotations` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#annotations
#[derive(Debug, PartialEq, Clone)]
pub struct Annotations {
    variant: AnnotationsVariant,
}
impl_type_ref_walker!(Annotations, as_annotations_v2_standard());

impl Annotations {
    pub fn as_annotations_v1(&self) -> Option<&AnnotationsV1> {
        if let AnnotationsVariant::V1(variant) = &self.variant {
            Some(variant)
        } else {
            None
        }
    }
    pub fn as_annotations_v2_simple(&self) -> Option<&AnnotationsV2Simple> {
        if let AnnotationsVariant::V2Simple(variant) = &self.variant {
            Some(variant)
        } else {
            None
        }
    }
    pub fn as_annotations_v2_standard(&self) -> Option<&AnnotationsV2Standard> {
        if let AnnotationsVariant::V2Standard(variant) = &self.variant {
            Some(variant)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum AnnotationsVariant {
    V1(AnnotationsV1),
    V2Standard(AnnotationsV2Standard),
    V2Simple(AnnotationsV2Simple),
}

impl ValidateInternal for Annotations {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        let mut wrapped_recorder =
            |el, msg| recorder.accept(ViolationInfo::new(self.into(), el, msg));
        match &self.variant {
            AnnotationsVariant::V1(variant) => {
                variant.validate_internal(value, ctx, &mut wrapped_recorder)
            }
            AnnotationsVariant::V2Standard(variant) => {
                variant.validate_internal(value, ctx, &mut wrapped_recorder)
            }
            AnnotationsVariant::V2Simple(variant) => {
                variant.validate_internal(value, ctx, &mut wrapped_recorder)
            }
        }
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Annotations
where
    TypeArgument: WriteAsIsl<V>,
    AnnotationsV1: WriteAsIsl<V>,
    AnnotationsV2Simple: WriteAsIsl<V>,
    AnnotationsV2Standard: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        match &self.variant {
            AnnotationsVariant::V1(variant) => variant.write_as_isl(writer, ctx),
            AnnotationsVariant::V2Standard(variant) => variant.write_as_isl(writer, ctx),
            AnnotationsVariant::V2Simple(variant) => variant.write_as_isl(writer, ctx),
        }
    }
}

impl ReadConstraint<ISL_1_0> for Annotations {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_1_0>,
    ) -> IonSchemaResult<Option<Self>> {
        match v1::AnnotationsV1::read_constraint(ion, ctx) {
            Ok(Some(v1)) => Ok(Some(Annotations {
                variant: AnnotationsVariant::V1(v1),
            })),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }
}

impl ReadConstraint<ISL_2_0> for Annotations {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        if ion.ion_type() == IonType::List {
            match AnnotationsV2Simple::read_constraint(ion, ctx) {
                Ok(Some(v2_simple)) => Ok(Some(Annotations {
                    variant: AnnotationsVariant::V2Simple(v2_simple),
                })),
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            }
        } else {
            match AnnotationsV2Standard::read_constraint(ion, ctx) {
                Ok(Some(v2_standard)) => Ok(Some(Annotations {
                    variant: AnnotationsVariant::V2Standard(v2_standard),
                })),
                Ok(None) => Ok(None),
                Err(e) => Err(e),
            }
        }
    }
}
