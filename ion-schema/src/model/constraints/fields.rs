// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_schema_version::Versioned;
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::variable_type_argument::{
    VariablyOccurringTypeArgument, VersionedVariablyOccurringTypeArgument,
};
use crate::model::TypeDefinitionBuilder;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, ValueWriter};
use std::collections::HashMap;
use std::ops::ControlFlow;

impl ConstraintName for Fields {
    const CONSTRAINT_NAME: &'static str = "fields";
}

/// Represents the `fields` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#fields
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#fields
#[derive(Debug, PartialEq, Clone)]
pub struct Fields {
    fields: HashMap<String, VariablyOccurringTypeArgument>,
    is_closed: bool,
}

impl Fields {
    pub fn fields(&self) -> impl Iterator<Item = (&str, &VariablyOccurringTypeArgument)> {
        self.fields.iter().map(|(k, v)| (k.as_str(), v))
    }

    pub fn is_closed(&self) -> bool {
        self.is_closed
    }
}

/// An argument for the `fields` constraint that represents a single field.
pub type VersionedFieldTypeArgument<V> = Versioned<(String, VariablyOccurringTypeArgument), V>;

impl<V: IslVersion, S: Into<String>, T: Into<VersionedVariablyOccurringTypeArgument<V>>>
    From<(S, T)> for VersionedFieldTypeArgument<V>
{
    fn from(value: (S, T)) -> Self {
        let name = value.0.into();
        let type_ = value.1.into();
        Versioned::map(type_, |t| (name, t))
    }
}

/// A list of [`VersionedFieldTypeArgument`]s.
///
/// This type can be constructed from `Vec<T>`, `[T; N]`, or `(T₀, T₁, ...)`,
/// where `T` (or `Tₙ`) implements `Into<VersionedFieldTypeArgument<V>>`.
pub type VersionedFieldTypeArgumentList<V> =
    Versioned<Vec<(String, VariablyOccurringTypeArgument)>, V>;

impl<V: IslVersion, T: Into<VersionedFieldTypeArgument<V>>> From<Vec<T>>
    for VersionedFieldTypeArgumentList<V>
{
    fn from(value: Vec<T>) -> Self {
        let list: Vec<(String, VariablyOccurringTypeArgument)> = value
            .into_iter()
            .map(|it| {
                let arg: VersionedFieldTypeArgument<V> = it.into();
                Versioned::into_inner(arg)
            })
            .collect();
        Versioned::new(list)
    }
}

impl<const N: usize, V: IslVersion, T: Into<VersionedFieldTypeArgument<V>>> From<[T; N]>
    for VersionedFieldTypeArgumentList<V>
{
    fn from(value: [T; N]) -> Self {
        let list = value
            .into_iter()
            .map(|it| {
                let arg: VersionedFieldTypeArgument<V> = it.into();
                Versioned::into_inner(arg)
            })
            .collect();
        Versioned::new(list)
    }
}

/// Allows heterogeneous tuples to be converted to VersionedFieldTypeArgumentList
/// Unfortunately, we have to manually include the indices of all the tuple members until
/// https://rust-lang.github.io/rfcs/3086-macro-metavar-expr.html is stable.
macro_rules! into_fields_arg_list {
    ($($t:ident $i:tt),+) => {
        impl<V: IslVersion, $($t),+> From<( $($t,)+ )> for VersionedFieldTypeArgumentList<V>
        where $($t: Into<VersionedFieldTypeArgument<V>>,)+
        {
            fn from(value: ( $($t,)+ )) -> Self {
                Versioned::new(vec![
                    $(Versioned::into_inner(value.$i.into()),)+
                ])
            }
        }
    };
}

into_fields_arg_list!(A 0);
into_fields_arg_list!(A 0, B 1);
into_fields_arg_list!(A 0, B 1, C 2);
into_fields_arg_list!(A 0, B 1, C 2, D 3);
into_fields_arg_list!(A 0, B 1, C 2, D 3, E 4);
into_fields_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5);
into_fields_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);
into_fields_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
into_fields_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
into_fields_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FieldsContent {
    Open,
    Closed,
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn fields<T: Into<VersionedFieldTypeArgumentList<V>>>(
        self,
        content: FieldsContent,
        type_arguments: T,
    ) -> Self {
        let fields: HashMap<String, VariablyOccurringTypeArgument> =
            Versioned::into_inner(type_arguments.into())
                .into_iter()
                .collect();
        let constraint = Fields {
            fields,
            is_closed: content == FieldsContent::Closed,
        };
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Fields {
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

impl<V: IslVersion> WriteAsIsl<V> for Fields
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

impl ReadConstraint<ISL_1_0> for Fields {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_1_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

impl ReadConstraint<ISL_2_0> for Fields {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isl::isl_type_reference::NullabilityModifier;
    use crate::model::constraints::AnyConstraint;
    use crate::model::variable_type_argument::IntoVariablyOccurringTypeArgument;
    use crate::model::{IntoTypeArgument, TypeReference};

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_2_0>::new()
            .fields(
                FieldsContent::Closed,
                (
                    ("a", "foo".optional()),
                    ("b", (1..=1, "bar")),
                    ("c", "baz".or_null(true).occurs(3)),
                ),
            )
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Fields(Fields {
                fields: vec![
                    (
                        "a".to_string(),
                        VariablyOccurringTypeArgument::occurs(
                            TypeArgument::from_type_ref(
                                NullabilityModifier::Nothing,
                                TypeReference::from("foo")
                            ),
                            0..=1
                        )
                    ),
                    (
                        "b".to_string(),
                        VariablyOccurringTypeArgument::occurs(
                            TypeArgument::from_type_ref(
                                NullabilityModifier::Nothing,
                                TypeReference::from("bar")
                            ),
                            1
                        )
                    ),
                    (
                        "c".to_string(),
                        VariablyOccurringTypeArgument::occurs(
                            TypeArgument::from_type_ref(
                                NullabilityModifier::NullOr,
                                TypeReference::from("baz")
                            ),
                            3
                        )
                    ),
                ]
                .into_iter()
                .collect(),
                is_closed: true
            })]
        )
    }
}
