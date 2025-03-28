// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_schema_version::Versioned;
use crate::model::bag::Bag;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::{TypeDefinitionBuilder, VersionedTypeArgumentList};
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for OneOf {
    const CONSTRAINT_NAME: &'static str = "one_of";
}

/// Represents the `one_of` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#one_of
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#one_of
#[derive(Debug, PartialEq, Clone)]
pub struct OneOf {
    type_arguments: Bag<TypeArgument>,
}
impl_type_ref_walker!(OneOf, type_arguments);

impl OneOf {
    fn new<T: Into<Bag<TypeArgument>>>(type_arguments: T) -> Self {
        Self {
            type_arguments: type_arguments.into(),
        }
    }

    pub fn type_arguments(&self) -> impl Iterator<Item = &TypeArgument> {
        self.type_arguments.iter()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn one_of<T: Into<VersionedTypeArgumentList<V>>>(self, type_arguments: T) -> Self {
        let type_arguments = Versioned::into_inner(type_arguments.into());
        self.with_constraint(OneOf::new(type_arguments).into())
    }
}

impl ValidateInternal for OneOf {
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

impl<V: IslVersion> WriteAsIsl<V> for OneOf
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

impl<V: IslVersion> ReadConstraint<V> for OneOf {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isl::isl_type_reference::NullabilityModifier;
    use crate::model::bag::bag;
    use crate::model::{IntoTypeArgument, TypeDefinition, TypeReference};
    use crate::{ISL_1_0, ISL_2_0};

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .one_of((
                // This is a heterogeneous "list" (really a tuple) of things that can become a type arg.
                "foo_type",
                TypeDefinitionBuilder::new().build(),
                TypeReference::imported("bar.isl", "baz"),
                "quux_type".or_null(true),
            ))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Bag<_>>(),
            bag![OneOf::new(bag![
                TypeArgument::from_type_ref(
                    NullabilityModifier::Nothing,
                    TypeReference::from("foo_type")
                ),
                TypeArgument::from_type_def(
                    NullabilityModifier::Nothing,
                    TypeDefinition::new(bag![], bag![])
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
            .one_of(("foo_type", "quux_type".or_null(true)))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Bag<_>>(),
            bag![OneOf::new(bag![
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
                .one_of(("a", "b", "c"))
                .build(),
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .one_of(("b", "c", "a"))
                .build(),
        );
        assert_eq!(
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .one_of(("a", "b", "c", "a", "a"))
                .build(),
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .one_of(("a", "b", "a", "c", "a"))
                .build(),
        );
        assert_ne!(
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .one_of(("a", "b", "c", "a"))
                .build(),
            TypeDefinitionBuilder::<ISL_1_0>::new()
                .one_of(("a", "b", "c", "b"))
                .build(),
        );
    }
}
