// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

// TODO: This file is a placeholder. These things will eventually be moved to more sensible locations
//       instead of being clobbered together.

use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;
use std::sync::Arc;

/// For internal implementation of validation.
///
/// Should be implemented by TypeDefinition, TypeArgument, etc. and all constraints.
pub(crate) trait ValidateInternal {
    /// Validate a value.
    /// Panics if any unresolved type references are encountered. This crate must
    /// ensure that resolve() is called before calling validate.
    fn validate_internal<'a, R>(
        &'a self,
        value: &'a IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'a mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'a>;

    /// Resolve any type references in preparation for validation.
    fn resolve(&self, schema_store: &Arc<InvisibleSchemaStore>) -> IonSchemaResult<()>;

    /// Un-resolve all type references.
    /// This is not strictly required, but if type reference resolution were to fail,
    /// we should undo any changes before returning ownership to the user.
    fn unresolve(&self);
}

// TODO: fields/functions to support
//  - looking up references in the InvisibleSchemaStore
//  - any other state or needed for validation
//  - "validate" configuration options
pub(crate) struct ValidationContext {}

pub(crate) struct InvisibleSchemaStore {
    // TODO: This is a placeholder
}

/// For internal implementation of serialization.
///
/// Implementations of `WriteAsIon` may delegate to this when possible.
pub(crate) trait WriteAsIsl<V: IslVersion> {
    fn write_as_isl<W: ValueWriter>(&self, writer: W) -> IonSchemaResult<()>;
}

/// For internal implementation of reading Ion Schema Language.
pub(crate) trait ReadFromIsl<V: IslVersion>: Sized {
    fn try_read(ion: &Element, ctx: &LoaderContext) -> IonSchemaResult<Self>;
}

// TODO: fields/functions to support
//    * looking up types/schemas that are already loaded
//    * queueing up types/schemas that need to be loaded
//    * any other context that we discover that we need.
pub(crate) struct LoaderContext {}
