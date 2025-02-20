// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0
use crate::model::constraints::*;

macro_rules! any_of_these {
    (#[$derives:meta] enum $super_:ident { $($name:ident,)+ }) => {
        #[$derives]
        pub enum $super_ {
            $($name($name)),*
        }

        $(
        impl From<$name> for $super_ {
            fn from(value: $name) -> Self { $super_::$name(value) }
        }
        )+

        // Any traits that should be implemented by delegating to the enum variants can be added here.
    }
}

any_of_these!(
    #[derive(Debug, PartialEq, Clone)]
    enum AnyConstraint {
        // AllOf,
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
        // Fields,
        // FieldNames,
        // Ieee754Float,
        // Not,
        // OneOf,
        // OrderedElements,
        // Precision,
        // Regex,
        // Scale,
        // TimestampOffset,
        // TimestampPrecision,
        // Type,
        // Utf8ByteLength,
        // ValidValues,
    }
);

macro_rules! any_of_these_refs {
    (#[$derives:meta] enum $super_:ident { $($name:ident,)+ }) => {
        #[$derives]
        pub enum $super_<'a> {
            $($name(&'a $name)),*
        }

        $(
        impl<'a> From<&'a $name> for $super_<'a> {
            fn from(value: &'a $name) -> Self { $super_::$name(value) }
        }
        )+

        // Any traits that should be implemented by delegating to the enum variants can be added here.
    }
}

any_of_these_refs!(
    #[derive(Debug, PartialEq, Clone)]
    enum AnyConstraintRef {
        // AllOf,
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
        // Fields,
        // FieldNames,
        // Ieee754Float,
        // Not,
        // OneOf,
        // OrderedElements,
        // Precision,
        // Regex,
        // Scale,
        // TimestampOffset,
        // TimestampPrecision,
        // Type,
        // Utf8ByteLength,
        // ValidValues,
    }
);
