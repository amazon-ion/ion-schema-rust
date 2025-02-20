// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::constraints::AnyConstraint;
use crate::IslVersion;
use ion_rs::Element;
use std::marker::PhantomData;

/// A version-safe builder for Ion Schema type definitions.
pub struct TypeDefinitionBuilder<V: IslVersion> {
    pub(crate) constraints: Vec<AnyConstraint>,
    open_content: Vec<(String, Element)>,
    isl_version: PhantomData<V>,
}
