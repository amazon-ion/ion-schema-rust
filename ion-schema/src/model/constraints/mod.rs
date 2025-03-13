// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod all_of;
mod annotations_v2_simple;
mod any_constraint;
mod fields;
mod ordered_elements;
mod timestamp_precision;
mod type_constraint;
mod utf8_byte_length;
mod valid_values;

use crate::internal_traits::*;
use crate::result::IonSchemaResult;
use crate::IslVersion;
use ion_rs::Element;
use std::fmt::Debug;

pub use all_of::*;
pub use annotations_v2_simple::*;
pub use any_constraint::*;
pub use fields::*;
pub use ordered_elements::*;
pub use timestamp_precision::*;
pub use type_constraint::*;
pub use utf8_byte_length::*;
pub use valid_values::*;

/// Provides the constraint's name (ISL keyword) for a given constraint implementation.
pub(crate) trait ConstraintName {
    const CONSTRAINT_NAME: &'static str;
}

pub(super) trait ReadConstraint<V: IslVersion>
where
    Self: Sized,
{
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        // Default implementation indicates this is unsupported for a particular ISL version.
        Ok(None)
    }
}

#[cfg(test)]
/// Runs a test case for [`ReadConstraint`](crate::model::constraints::ReadConstraint).
fn test_read_constraint<V: IslVersion, T: ReadConstraint<V> + PartialEq + Debug>(
    ion: &str,
    expected: Result<Option<T>, ()>,
    version: std::marker::PhantomData<V>,
) {
    use crate::internal_traits::LoaderContext;
    use ion_rs::Element;

    let element = Element::read_one(ion).unwrap();
    let load_ctx = LoaderContext::<V>::new();
    let result = T::read_constraint(&element, &load_ctx);

    if let Ok(expected_constraint) = expected {
        let actual_constraint = result.unwrap();
        assert_eq!(expected_constraint, actual_constraint);
    } else {
        assert!(result.is_err())
    }
}
