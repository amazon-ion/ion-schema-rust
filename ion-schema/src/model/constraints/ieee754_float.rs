// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::{invalid_schema, IonSchemaError, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, Value, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for Ieee754Float {
    const CONSTRAINT_NAME: &'static str = "ieee754_float";
}

/// Represents the `ieee754_float` constraint (ref.  [ISL 2.0]).
///
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#ieee754_float
#[derive(Debug, Clone, PartialEq)]
pub struct Ieee754Float {
    value: FloatingPointNumberFormat,
}
impl_type_ref_walker!(Ieee754Float);

impl Ieee754Float {
    pub fn value(&self) -> FloatingPointNumberFormat {
        self.value
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn ieee754_float(self, value: FloatingPointNumberFormat) -> Self {
        let constraint = Ieee754Float { value };
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Ieee754Float {
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

impl WriteAsIsl<ISL_1_0> for Ieee754Float {}

impl WriteAsIsl<ISL_2_0> for Ieee754Float {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_2_0>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl ReadConstraint<ISL_1_0> for Ieee754Float {}

impl ReadConstraint<ISL_2_0> for Ieee754Float {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum FloatingPointNumberFormat {
    Binary16,
    Binary32,
    Binary64,
}

impl TryFrom<&Element> for FloatingPointNumberFormat {
    type Error = IonSchemaError;

    fn try_from(value: &Element) -> Result<Self, Self::Error> {
        let text = if let Value::Symbol(s) = value.value() {
            s.text()
        } else {
            None
        };
        use FloatingPointNumberFormat::*;
        match text {
            Some("binary16") => Ok(Binary16),
            Some("binary32") => Ok(Binary32),
            Some("binary64") => Ok(Binary64),
            _ => invalid_schema!("Not a valid IEEE754 format symbol: {value}"),
        }
    }
}

impl From<FloatingPointNumberFormat> for Value {
    fn from(value: FloatingPointNumberFormat) -> Self {
        let text: &str = value.into();
        Value::Symbol(text.into())
    }
}

impl From<FloatingPointNumberFormat> for &'static str {
    fn from(value: FloatingPointNumberFormat) -> Self {
        use FloatingPointNumberFormat::*;
        match value {
            Binary16 => "binary16",
            Binary32 => "binary32",
            Binary64 => "binary64",
        }
    }
}
