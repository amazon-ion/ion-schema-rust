// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_schema_version::Versioned;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::{TypeDefinitionBuilder, VersionedTypeArgumentList};
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for AllOf {
    const CONSTRAINT_NAME: &'static str = "all_of";
}

/// Represents the `all_of` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#all_of
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#all_of
#[derive(Debug, PartialEq, Clone)]
pub struct AllOf {
    type_arguments: Vec<TypeArgument>,
}

impl AllOf {
    fn new(mut type_arguments: Vec<TypeArgument>) -> Self {
        // Sorting the vec allows us to get Bag-like equality semantics.
        // TODO: Replace the vec with a HashBag or come up with a less hacky way to sort the type arguments.
        type_arguments.sort_by_cached_key(|item| format!("{item:?}"));

        Self { type_arguments }
    }

    pub fn type_arguments(&self) -> impl Iterator<Item = &TypeArgument> {
        self.type_arguments.iter()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn all_of<T: Into<VersionedTypeArgumentList<V>>>(self, type_arguments: T) -> Self {
        let type_arguments = Versioned::into_inner(type_arguments.into());
        self.with_constraint(AllOf::new(type_arguments).into())
    }
}

impl ValidateInternal for AllOf {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!("Implement this once the ValidationContext allows looking up a type")
    }
}

impl<V: IslVersion> WriteAsIsl<V> for AllOf
where
    TypeArgument: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for AllOf {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::isl::isl_type_reference::NullabilityModifier;
    use crate::model::{IntoTypeArgument, TypeDefinition, TypeReference};
    use crate::{ISL_1_0, ISL_2_0};

    use super::*;

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .all_of((
                // This is a heterogeneous "list" (really a tuple) of things that can become a type arg.
                "foo_type",
                TypeDefinitionBuilder::new().build(),
                TypeReference::imported("bar.isl", "baz"),
                "quux_type".or_null(true),
            ))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AllOf::new(vec![
                TypeArgument::from_type_ref(
                    NullabilityModifier::Nothing,
                    TypeReference::from("foo_type")
                ),
                TypeArgument::from_type_def(
                    NullabilityModifier::Nothing,
                    TypeDefinition::new(vec![], vec![])
                ),
                TypeArgument::from_type_ref(
                    NullabilityModifier::Nothing,
                    TypeReference::imported("bar.isl", "baz")
                ),
                TypeArgument::from_type_ref(
                    NullabilityModifier::Nullable,
                    TypeReference::from("quux_type")
                ),
            ],)
            .into()]
        );

        let type_ = TypeDefinitionBuilder::<ISL_2_0>::new()
            .all_of(("foo_type", "quux_type".or_null(true)))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AllOf::new(vec![
                TypeArgument::from_type_ref(
                    NullabilityModifier::Nothing,
                    TypeReference::from("foo_type")
                ),
                TypeArgument::from_type_ref(
                    NullabilityModifier::NullOr,
                    TypeReference::from("quux_type")
                ),
            ],)
            .into()]
        );
    }

    #[test]
    fn test_equality() {
        // NOTE: this is checking for _bag_ equivalence.
        assert_eq!(
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .all_of(("a", "b", "c"))
                .build(),
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .all_of(("b", "c", "a"))
                .build(),
        );
        assert_eq!(
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .all_of(("a", "b", "c", "a", "a"))
                .build(),
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .all_of(("a", "b", "a", "c", "a"))
                .build(),
        );
        assert_ne!(
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .all_of(("a", "b", "c", "a"))
                .build(),
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .all_of(("a", "b", "c", "b"))
                .build(),
        );
    }
}
