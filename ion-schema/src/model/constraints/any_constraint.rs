// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{ValidateInternal, ValidationContext, WriteAsIsl, WriteContext};
use crate::model::constraints::valid_values::ValidValues;
use crate::model::constraints::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::ValueWriter;
use std::ops::ControlFlow;

macro_rules! any_constraint {
    ($($name:ident,)+) => {
        #[derive(Debug, PartialEq, Clone)]
        #[non_exhaustive]
        pub enum AnyConstraint {
            $($name($name)),*
        }

        impl AnyConstraint {
            pub(in crate::model) fn read_constraint<V: IslVersion>(name: &str, ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<AnyConstraint>>
            where $($name : ReadConstraint<V>,)+
            {
                match name {
                    $($name::CONSTRAINT_NAME => $name::read_constraint(ion, ctx).map(|opt| opt.map(|c| c.into())),
                    )+
                    _ => Ok(None),
                }
            }
        }

        $(
        impl From<$name> for AnyConstraint {
            fn from(value: $name) -> Self { AnyConstraint::$name(value) }
        }
        )+

        // Any traits that should be implemented by delegating to the enum variants can be added here.
        impl ValidateInternal for AnyConstraint {
            fn validate_internal<'top: 'call, 'call, R>(
                &'top self,
                value: &'top IonSchemaElement<'top>,
                ctx: &ValidationContext,
                recorder: &'call mut R,
            ) -> ControlFlow<()>
            where
                R: ViolationRecorder<'top>,
            {
                match self {
                    $(AnyConstraint::$name(constraint) => constraint.validate_internal(value, ctx, recorder),
                    )+
                }
            }
        }

        // Version needs to be explicitly specified to avoid infinite recursion in the type checker.
        impl WriteAsIsl<ISL_1_0> for AnyConstraint {
            fn write_as_isl<W: ValueWriter>(&self, writer: W, ctx: &WriteContext<ISL_1_0>) -> IonSchemaResult<()> {
                match self {
                    $(AnyConstraint::$name(constraint) => $name::write_as_isl(constraint, writer, ctx),
                    )+
                }
            }
        }
        impl WriteAsIsl<ISL_2_0> for AnyConstraint {
            fn write_as_isl<W: ValueWriter>(&self, writer: W, ctx: &WriteContext<ISL_2_0>) -> IonSchemaResult<()> {
                match self {
                    $(AnyConstraint::$name(constraint) => $name::write_as_isl(constraint, writer, ctx),
                    )+
                }
            }
        }

        impl AnyConstraint {
            pub(crate) const fn constraint_keyword(&self) -> &'static str {
                match self {
                    $(AnyConstraint::$name(_) => $name::CONSTRAINT_NAME,
                    )+
                }
            }
        }

        /// An enum over references to all types of constraints. See [`AnyConstraint`].
        #[derive(Debug, PartialEq, Clone)]
        pub enum AnyConstraintRef<'a> {
            $($name(&'a $name)),*
        }

        $(
        impl<'a> From<&'a $name> for AnyConstraintRef<'a> {
            fn from(value: &'a $name) -> Self { AnyConstraintRef::$name(value) }
        }
        )+
    }
}

any_constraint!(
    AllOf,
    // AnnotationsV1,
    AnnotationsV2Simple,
    // AnnotationsV2Standard,
    // AnyOf,
    // ByteLength,
    // CodepointLength,
    // ContainerLength,
    // Contains,
    // ElementConstraint,
    // Exponent,
    Fields,
    // FieldNames,
    // Ieee754Float,
    // Not,
    // OneOf,
    OrderedElements,
    // Precision,
    // Regex,
    // Scale,
    // TimestampOffset,
    TimestampPrecision,
    TypeConstraint,
    Utf8ByteLength,
    ValidValues,
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_harness::*;
    use crate::{ISL_1_0, ISL_2_0};

    // TODO: Include a trivial case for each constraint and ISL version.
    test_harness!(
        use write_as_isl;
        ISL_1_0, ISL_2_0 {
            #[case::utf8_byte_length(Ok("range::[1, 10]"), AnyConstraint::Utf8ByteLength(Utf8ByteLength::new(1..=10)))]
        }
    );
}
