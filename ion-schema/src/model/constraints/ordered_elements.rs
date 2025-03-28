// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_schema_version::Versioned;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::variable_type_argument::{
    VariablyOccurringTypeArgument, VersionedVariablyOccurringTypeArgumentList,
};
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use std::ops::ControlFlow;

impl ConstraintName for OrderedElements {
    const CONSTRAINT_NAME: &'static str = "ordered_elements";
}

/// Represents the `ordered_elements` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#ordered_elements
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#ordered_elements
#[derive(Debug, PartialEq, Clone)]
pub struct OrderedElements {
    type_arguments: Vec<VariablyOccurringTypeArgument>,
}
impl_type_ref_walker!(OrderedElements, type_arguments);

impl OrderedElements {
    pub fn elements(&self) -> impl Iterator<Item = &VariablyOccurringTypeArgument> {
        self.type_arguments.iter()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn ordered_elements<T: Into<VersionedVariablyOccurringTypeArgumentList<V>>>(
        self,
        type_arguments: T,
    ) -> Self {
        let type_arguments = Versioned::into_inner(type_arguments.into());
        let constraint = OrderedElements { type_arguments };
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for OrderedElements {
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

impl<V: IslVersion> WriteAsIsl<V> for OrderedElements
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

impl<V: IslVersion> ReadConstraint<V> for OrderedElements {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isl::isl_type_reference::NullabilityModifier;
    use crate::model::variable_type_argument::IntoVariablyOccurringTypeArgument;
    use crate::model::{IntoTypeArgument, TypeReference};
    use crate::ISL_2_0;

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_2_0>::new()
            .ordered_elements((
                (0..=1, "foo"),
                "bar".required(),
                "baz".or_null(true).occurs(3),
            ))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![OrderedElements {
                type_arguments: vec![
                    VariablyOccurringTypeArgument::occurs(
                        TypeArgument::from_type_ref(
                            NullabilityModifier::Nothing,
                            TypeReference::from("foo")
                        ),
                        0..=1
                    ),
                    VariablyOccurringTypeArgument::occurs(
                        TypeArgument::from_type_ref(
                            NullabilityModifier::Nothing,
                            TypeReference::from("bar")
                        ),
                        1
                    ),
                    VariablyOccurringTypeArgument::occurs(
                        TypeArgument::from_type_ref(
                            NullabilityModifier::NullOr,
                            TypeReference::from("baz")
                        ),
                        3
                    ),
                ],
            }
            .into()]
        );
    }
}
