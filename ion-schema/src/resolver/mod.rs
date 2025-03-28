// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::SchemaDocument;

mod type_ref_visitor;

pub(crate) use type_ref_visitor::*;

#[derive(Debug)]
pub(crate) struct SchemaStore {
    schemas: Vec<(String, SchemaDocument)>,
}
