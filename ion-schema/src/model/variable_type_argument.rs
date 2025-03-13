// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{WriteAsIsl, WriteContext};
use crate::ion_schema_version::Versioned;
use crate::model::type_definition::TypeDefinition;
use crate::model::{IonSchemaRange, TypeArgument, VersionedTypeArgument};
use crate::result::IonSchemaResult;
use crate::IslVersion;
use ion_rs::ValueWriter;

/// A [`TypeArgument`] that can occur a specific number of times in a container.
#[derive(Debug, Clone, PartialEq)]
pub struct VariablyOccurringTypeArgument {
    occurs: IonSchemaRange<usize>,
    type_argument: TypeArgument,
}

impl VariablyOccurringTypeArgument {
    pub fn occurs<R: Into<IonSchemaRange<usize>>>(
        type_argument: TypeArgument,
        range: R,
    ) -> VariablyOccurringTypeArgument {
        VariablyOccurringTypeArgument {
            type_argument,
            occurs: range.into(),
        }
    }
}

/// A [`VariablyOccurringTypeArgument`] with attached metadata indicating an ISL version in which it can be represented.
pub type VersionedVariablyOccurringTypeArgument<V> = Versioned<VariablyOccurringTypeArgument, V>;

pub trait IntoVariablyOccurringTypeArgument<V: IslVersion>: Sized {
    fn optional(self) -> VersionedVariablyOccurringTypeArgument<V> {
        self.occurs(0..=1)
    }
    fn required(self) -> VersionedVariablyOccurringTypeArgument<V> {
        self.occurs(1)
    }
    fn occurs<R: Into<IonSchemaRange<usize>>>(
        self,
        range: R,
    ) -> VersionedVariablyOccurringTypeArgument<V>;
}
impl<V: IslVersion, T: Into<VersionedTypeArgument<V>>> IntoVariablyOccurringTypeArgument<V> for T {
    fn occurs<R: Into<IonSchemaRange<usize>>>(
        self,
        range: R,
    ) -> VersionedVariablyOccurringTypeArgument<V> {
        Versioned::map(self.into(), |t| {
            VariablyOccurringTypeArgument::occurs(t, range.into())
        })
    }
}

impl<R: Into<IonSchemaRange<usize>>, T: Into<TypeArgument>> From<(R, T)>
    for VariablyOccurringTypeArgument
{
    fn from(value: (R, T)) -> Self {
        VariablyOccurringTypeArgument {
            occurs: value.0.into(),
            type_argument: value.1.into(),
        }
    }
}

impl<V: IslVersion, R: Into<IonSchemaRange<usize>>, T: Into<VersionedTypeArgument<V>>> From<(R, T)>
    for VersionedVariablyOccurringTypeArgument<V>
{
    fn from(value: (R, T)) -> Self {
        Versioned::new(VariablyOccurringTypeArgument {
            occurs: value.0.into(),
            type_argument: Versioned::into_inner(value.1.into()),
        })
    }
}

impl<V: IslVersion> WriteAsIsl<V> for VariablyOccurringTypeArgument
where
    TypeDefinition: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

/// A list of [`VersionedVariablyOccurringTypeArgument`]s.
///
/// This type can be constructed from `Vec<T>`, `[T; N]`, or `(T₀, T₁, ...)`,
/// where `T` (or `Tₙ`) implements `Into<VariablyOccurringTypeArgument<V>>`.
pub type VersionedVariablyOccurringTypeArgumentList<V> =
    Versioned<Vec<VariablyOccurringTypeArgument>, V>;

// Unfortunately, we cannot have this:
// impl<V, T, I> From<I> for VersionedVariablyOccurringTypeArgumentList<V>
// where V : IslVersion,
//       T: Into<VersionedVariablyOccurringTypeArgument<V>>,
//       I: IntoIterator<Item=T> { ... }
//
// because:
// note: upstream crates may add a new impl of trait `std::iter::IntoIterator` for type `(_, _)` in future versions
//
// So, we'll implement this conversion only for [T; N], Vec<T>, and tuples.

impl<V: IslVersion, T: Into<VersionedVariablyOccurringTypeArgument<V>>> From<Vec<T>>
    for VersionedVariablyOccurringTypeArgumentList<V>
{
    fn from(value: Vec<T>) -> Self {
        let list = value
            .into_iter()
            .map(|it| {
                let arg: VersionedVariablyOccurringTypeArgument<V> = it.into();
                Versioned::into_inner(arg)
            })
            .collect();
        Versioned::new(list)
    }
}

impl<const N: usize, V: IslVersion, T: Into<VersionedVariablyOccurringTypeArgument<V>>> From<[T; N]>
    for VersionedVariablyOccurringTypeArgumentList<V>
{
    fn from(value: [T; N]) -> Self {
        let list = value
            .into_iter()
            .map(|it| {
                let arg: VersionedVariablyOccurringTypeArgument<V> = it.into();
                Versioned::into_inner(arg)
            })
            .collect();
        Versioned::new(list)
    }
}

/// Allows heterogeneous tuples to be converted to VersionedVariablyOccurringTypeArgumentList
/// Unfortunately, we have to manually include the indices of all the tuple members until
/// https://rust-lang.github.io/rfcs/3086-macro-metavar-expr.html is stable.
macro_rules! into_var_type_arg_list {
    ($($t:ident $i:tt),+) => {
        impl<V: IslVersion, $($t),+> From<( $($t),+ )> for VersionedVariablyOccurringTypeArgumentList<V>
        where $($t: Into<VersionedVariablyOccurringTypeArgument<V>>,)+
        {
            fn from(value: ( $($t),+ )) -> Self {
                Versioned::new(vec![
                    $(Versioned::into_inner(value.$i.into()),)+
                ])
            }
        }
    };
}

into_var_type_arg_list!(A 0, B 1);
into_var_type_arg_list!(A 0, B 1, C 2);
into_var_type_arg_list!(A 0, B 1, C 2, D 3);
into_var_type_arg_list!(A 0, B 1, C 2, D 3, E 4);
into_var_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5);
into_var_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);
into_var_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
into_var_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
into_var_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
