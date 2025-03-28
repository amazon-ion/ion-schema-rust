// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{LoaderContext, ReadFromIsl, WriteAsIsl, WriteContext};
use crate::ion_schema_version::Versioned;
use crate::isl::isl_type_reference::NullabilityModifier;
use crate::model::type_definition::TypeDefinition;
use crate::model::type_reference::TypeReference;
use crate::model::VersionedTypeDefinition;
use crate::resolver::*;
use crate::result::{invalid_schema_error, IonSchemaResult};
use crate::{IslVersion, ISL_1_0, ISL_2_0};
use ion_rs::{Element, Value, ValueWriter};

/// An argument for a constraint where that argument is an Ion Schema type.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeArgument {
    nullability_modifier: NullabilityModifier,
    kind: TypeArgumentKind,
}

impl TypeArgument {
    /// Constructs a new TypeArgument from a TypeDefinition.
    ///
    /// Do not expose publicly because that would allow users to mix ISL versions.
    pub(crate) fn from_type_def(
        nullability_modifier: NullabilityModifier,
        type_definition: TypeDefinition,
    ) -> Self {
        TypeArgument {
            nullability_modifier,
            kind: TypeArgumentKind::InlineType(type_definition),
        }
    }

    /// Constructs a new TypeArgument from a TypeReference.
    pub(crate) fn from_type_ref(
        nullability_modifier: NullabilityModifier,
        type_reference: TypeReference,
    ) -> Self {
        TypeArgument {
            nullability_modifier,
            kind: TypeArgumentKind::TypeReference(type_reference),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TypeArgumentKind {
    TypeReference(TypeReference),
    InlineType(TypeDefinition),
}

impl TypeRefWalker for TypeArgument {
    fn walk<V: TypeRefVisitor>(&self, visitor: &mut V) {
        match &self.kind {
            TypeArgumentKind::TypeReference(ref type_ref) => visitor.visit(type_ref),
            TypeArgumentKind::InlineType(type_def) => type_def.walk(visitor),
        }
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TypeArgument
where
    TypeDefinition: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        let writer = match (self.nullability_modifier, V::MAJOR_MINOR) {
            (NullabilityModifier::Nothing, _) => writer.with_annotations([] as [&'static str; 0]),
            (NullabilityModifier::NullOr, (1, _)) => todo!("Implement down-conversion to ISL 1.0 by wrapping in `type: $any, any_of: [$null,  T]`"),
            (NullabilityModifier::NullOr, (2, _)) => writer.with_annotations(["$null_or"]),
            (NullabilityModifier::Nullable, (1, _)) => writer.with_annotations(["nullable"]),
            (NullabilityModifier::Nullable, (2, _)) => return invalid_schema_error("'nullable::' is not valid for Ion Schema 2.0 and higher"),
            _ => unreachable!(),
        }?;

        match &self.kind {
            TypeArgumentKind::TypeReference(t) => TypeReference::write_as_isl(t, writer, ctx),
            TypeArgumentKind::InlineType(t) => TypeDefinition::write_as_isl(t, writer, ctx),
        }?;
        Ok(())
    }
}

impl<V: IslVersion> ReadFromIsl<V> for TypeArgument
where
    TypeDefinition: ReadFromIsl<V>,
{
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        let nullability_modifier = ReadFromIsl::try_read(ion, ctx)?;
        let kind = match ion.value() {
            Value::Symbol(s) => TypeArgumentKind::TypeReference(s.expect_text()?.into()),
            Value::Struct(s) => {
                if let Some(id) = s.get("id") {
                    TypeArgumentKind::TypeReference(TypeReference::try_read(ion, ctx)?)
                } else {
                    let ty: TypeDefinition = TypeDefinition::try_read(ion, ctx)?;
                    TypeArgumentKind::InlineType(ty)
                }
            }
            other => invalid_schema_error(format!("not a type argument: {other}"))?,
        };
        Ok(TypeArgument {
            nullability_modifier,
            kind,
        })
    }
}

/// A [`TypeArgument`] with attached metadata indicating an ISL version in which it can be represented.
pub type VersionedTypeArgument<V> = Versioned<TypeArgument, V>;

impl<V: IslVersion> From<VersionedTypeDefinition<V>> for VersionedTypeArgument<V> {
    fn from(value: VersionedTypeDefinition<V>) -> Self {
        Versioned::new(TypeArgument {
            nullability_modifier: NullabilityModifier::Nothing,
            kind: TypeArgumentKind::InlineType(Versioned::into_inner(value)),
        })
    }
}

impl<V: IslVersion, R: Into<TypeReference>> From<R> for VersionedTypeArgument<V> {
    fn from(value: R) -> Self {
        Versioned::new(TypeArgument {
            nullability_modifier: NullabilityModifier::Nothing,
            kind: TypeArgumentKind::TypeReference(value.into()),
        })
    }
}

pub trait IntoTypeArgument<V: IslVersion> {
    fn or_null(self, n: bool) -> VersionedTypeArgument<V>;
}
impl<T: Into<VersionedTypeArgument<ISL_1_0>>> IntoTypeArgument<ISL_1_0> for T {
    fn or_null(self, n: bool) -> VersionedTypeArgument<ISL_1_0> {
        Versioned::map(self.into(), |mut inner| {
            inner.nullability_modifier = if n {
                NullabilityModifier::Nullable
            } else {
                NullabilityModifier::Nothing
            };
            inner
        })
    }
}
impl<T: Into<VersionedTypeArgument<ISL_2_0>>> IntoTypeArgument<ISL_2_0> for T {
    fn or_null(self, n: bool) -> VersionedTypeArgument<ISL_2_0> {
        Versioned::map(self.into(), |mut inner| {
            inner.nullability_modifier = if n {
                NullabilityModifier::NullOr
            } else {
                NullabilityModifier::Nothing
            };
            inner
        })
    }
}

/// A list of [`VersionedTypeArgument`]s.
///
/// This type can be constructed from `Vec<T>`, `[T; N]`, or `(T₀, T₁, ...)`,
/// where `T` (or `Tₙ`) implements `Into<VersionedTypeArgument<V>>`.
pub type VersionedTypeArgumentList<V> = Versioned<Vec<TypeArgument>, V>;

// Unfortunately, we cannot have this:
// impl<V, T, I> From<I> for VersionedTypeArgumentList<V>
// where V : IslVersion,
//       T: Into<VersionedTypeArgument<V>>,
//       I: IntoIterator<Item=T> { ... }
//
// because:
// note: upstream crates may add a new impl of trait `std::iter::IntoIterator` for type `(_, _)` in future versions
//
// So, we'll implement this conversion only for [T; N], Vec<T>, and tuples.

impl<V: IslVersion, T: Into<VersionedTypeArgument<V>>> From<Vec<T>>
    for VersionedTypeArgumentList<V>
{
    fn from(value: Vec<T>) -> Self {
        let list = value
            .into_iter()
            .map(|it| {
                let arg: VersionedTypeArgument<V> = it.into();
                Versioned::into_inner(arg)
            })
            .collect();
        Versioned::new(list)
    }
}

impl<const N: usize, V: IslVersion, T: Into<VersionedTypeArgument<V>>> From<[T; N]>
    for VersionedTypeArgumentList<V>
{
    fn from(value: [T; N]) -> Self {
        let list = value
            .into_iter()
            .map(|it| {
                let arg: VersionedTypeArgument<V> = it.into();
                Versioned::into_inner(arg)
            })
            .collect();
        Versioned::new(list)
    }
}

/// Allows heterogeneous tuples to be converted to VersionedTypeArgumentList
/// Unfortunately, we have to manually include the indices of all the tuple members until
/// https://rust-lang.github.io/rfcs/3086-macro-metavar-expr.html is stable.
macro_rules! into_type_arg_list {
    ($($t:ident $i:tt),+) => {
        impl<V: IslVersion, $($t),+> From<( $($t,)+ )> for VersionedTypeArgumentList<V>
        where $($t: Into<VersionedTypeArgument<V>>,)+
        {
            fn from(value: ( $($t,)+ )) -> Self {
                Versioned::new(vec![
                    $(Versioned::into_inner(value.$i.into()),)+
                ])
            }
        }
    };
}

into_type_arg_list!(A 0);
into_type_arg_list!(A 0, B 1);
into_type_arg_list!(A 0, B 1, C 2);
into_type_arg_list!(A 0, B 1, C 2, D 3);
into_type_arg_list!(A 0, B 1, C 2, D 3, E 4);
into_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5);
into_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);
into_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
into_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
into_type_arg_list!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8, J 9);
