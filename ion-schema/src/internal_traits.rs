// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

// TODO: This file is a placeholder. These things will eventually be moved to more sensible locations
//       instead of being clobbered together.

use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::ControlFlow;

/// For internal implementation of validation.
///
/// Should be implemented by TypeDefinition, TypeArgument, etc. and all constraints.
pub(crate) trait ValidateInternal {
    /// Validate a value.
    /// Panics if any unresolved type references are encountered. This crate must
    /// ensure that resolve() is called before calling validate.
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>;
}

// TODO: fields/functions to support
//  - looking up references in the InvisibleSchemaStore
//  - any other state or needed for validation
//  - "validate" configuration options
pub(crate) struct ValidationContext {}

/// For internal implementation of serialization.
///
/// Implementations of `WriteAsIon` may delegate to this when possible.
pub(crate) trait WriteAsIsl<V: IslVersion>: Debug {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        let (major, minor) = V::MAJOR_MINOR;
        invalid_schema_error(format!(
            "{self:?} is not supported in Ion Schema Language {major}.{minor}"
        ))
    }
}

/// Contains context for writing to ISL.
///
/// For now, this includes only the version as a type parameter.
/// In the future, it may include things such as:
///  - options to allow lossy conversion between ISL versions
///  - stylistic choices for writing as Ion
#[derive(Copy, Clone, Debug)]
pub(crate) struct WriteContext<V> {
    version: PhantomData<V>,
    /// Indicates whether ranges may be "minimized" into single values.
    /// E.g.: writing `range::[1, 1]` as `1`.
    pub(crate) minimize_ranges: bool,
}
impl<V: IslVersion> WriteContext<V> {
    pub fn new() -> Self {
        WriteContext {
            version: PhantomData::<V>,
            minimize_ranges: true,
        }
    }
}

/// For internal implementation of reading Ion Schema Language.
pub(crate) trait ReadFromIsl<V: IslVersion>: Sized {
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self>;
}
/// Macro for trivial implementations that can use `expect_*` functions on [`Element`].
macro_rules! read_from_isl {
    ($t:ty, $func:ident) => {
        impl<V: IslVersion> ReadFromIsl<V> for $t {
            fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
                let i = ion.$func()?;
                Ok(i)
            }
        }
    };
}
read_from_isl!(usize, expect_usize);
read_from_isl!(i64, expect_i64);
read_from_isl!(ion_rs::Decimal, expect_decimal);
read_from_isl!(ion_rs::Timestamp, expect_timestamp);

// TODO: fields/functions to support
//    * looking up types/schemas that are already loaded
//    * queueing up types/schemas that need to be loaded
//    * any other context that we discover that we need.
pub(crate) struct LoaderContext<V> {
    version: PhantomData<V>,
}
impl<V: IslVersion> LoaderContext<V> {
    pub fn new() -> Self {
        LoaderContext {
            version: PhantomData::<V>,
        }
    }
}
