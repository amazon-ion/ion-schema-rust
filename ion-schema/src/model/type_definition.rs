// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ReadFromIsl, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::ion_schema_version::Versioned;
use crate::model::bag::Bag;
use crate::model::constraints::*;
use crate::resolver::impl_type_ref_walker;
use crate::result::{HasIslSourceLocation, IonSchemaResult, IslSourceLocation};
use crate::{IonSchemaElement, IslVersion, ViolationRecorder, ISL_1_0, ISL_2_0};
use ion_rs::{Element, IonData, StructWriter, Symbol, ValueWriter};
use std::marker::PhantomData;
use std::ops::ControlFlow;

pub type VersionedTypeDefinition<V> = Versioned<TypeDefinition, V>;

/// A TypeDefinition is a set of constraints and (optionally) additional user content.
#[derive(Debug, Clone)]
pub struct TypeDefinition {
    constraints: Bag<AnyConstraint>,
    open_content: Bag<(Symbol, IonData<Element>)>,
    source_location: IslSourceLocation,
}
impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.constraints == other.constraints && self.open_content == other.open_content
    }
}

impl TypeDefinition {
    pub fn builder<V: IslVersion>() -> TypeDefinitionBuilder<V> {
        TypeDefinitionBuilder::new()
    }

    pub(crate) fn new(
        constraints: Bag<AnyConstraint>,
        open_content: Bag<(Symbol, IonData<Element>)>,
    ) -> Self {
        Self {
            constraints,
            open_content,
            source_location: IslSourceLocation::new(None),
        }
    }

    pub fn constraints(&self) -> impl Iterator<Item = &AnyConstraint> {
        self.constraints.iter()
    }

    pub fn open_content(&self) -> impl Iterator<Item = (&Symbol, &Element)> {
        self.open_content
            .iter()
            .map(|(name, value)| (name, value.as_ref()))
    }
}

/// A version-safe builder for Ion Schema type definitions.
#[derive(Debug, Clone)]
pub struct TypeDefinitionBuilder<V: IslVersion> {
    // TypeDefinitionBuilder uses `Vec` because our naive implementation of `Bag` does not support
    // modifying the collection.
    constraints: Vec<AnyConstraint>,
    open_content: Vec<(Symbol, IonData<Element>)>,
    isl_version: PhantomData<V>,
}
impl_type_ref_walker!(TypeDefinition, constraints);

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub(crate) fn new() -> Self {
        TypeDefinitionBuilder {
            constraints: vec![],
            open_content: vec![],
            isl_version: PhantomData::<V>,
        }
    }

    /// Adds a constraint to this type definition.
    ///
    /// This is pub(crate) so that users cannot add a combination of constraints that is not
    /// valid for any ISL version.
    pub(crate) fn with_constraint(mut self, constraint: AnyConstraint) -> Self {
        self.constraints.push(constraint);
        self
    }

    pub fn build(self) -> VersionedTypeDefinition<V> {
        Versioned::new(TypeDefinition::new(
            self.constraints.into(),
            self.open_content.into(),
        ))
    }

    fn build_with_source_location(
        self,
        source_location: IslSourceLocation,
    ) -> VersionedTypeDefinition<V> {
        Versioned::new(TypeDefinition {
            constraints: self.constraints.into(),
            open_content: self.open_content.into(),
            source_location,
        })
    }

    pub fn with_open_content<S: Into<Symbol>, E: Into<Element>>(
        mut self,
        field_name: S,
        value: E,
    ) -> Self {
        self.open_content
            .push((field_name.into(), IonData::from(value.into())));
        self
    }
}

impl ValidateInternal for TypeDefinition {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        for c in &self.constraints {
            c.validate_internal(value, ctx, recorder)?;
        }
        ControlFlow::Continue(())
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TypeDefinition
where
    AnyConstraint: WriteAsIsl<V>,
{
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        let mut struct_writer = writer.struct_writer()?;

        for c in &self.constraints {
            let value_writer = struct_writer.field_writer(c.constraint_keyword());
            c.write_as_isl(value_writer, ctx)?;
        }
        // Can't use `write_all` because `IonData` doesn't implement `WriteAsIon`
        for (k, v) in &self.open_content {
            struct_writer.write(k, v.as_ref())?;
        }
        struct_writer.close()?;
        Ok(())
    }
}

impl ReadFromIsl<ISL_1_0> for TypeDefinition {
    fn try_read(ion: &Element, ctx: &LoaderContext<ISL_1_0>) -> IonSchemaResult<Self> {
        let struct_ = ion.expect_struct()?;
        let mut builder = TypeDefinitionBuilder::<ISL_1_0>::new();
        for (name, value) in struct_.fields() {
            let constraint_name = name.expect_text()?;
            match AnyConstraint::read_constraint(constraint_name, value, ctx)? {
                Some(constraint) => builder.constraints.push(constraint),
                None => builder
                    .open_content
                    .push((name.clone(), IonData::from(value.clone()))),
            }
        }
        Ok(Versioned::into_inner(
            builder.build_with_source_location(ion.isl_source_location()),
        ))
    }
}

impl ReadFromIsl<ISL_2_0> for TypeDefinition {
    fn try_read(ion: &Element, ctx: &LoaderContext<ISL_2_0>) -> IonSchemaResult<Self> {
        let struct_ = ion.expect_struct()?;
        let mut builder = TypeDefinitionBuilder::<ISL_2_0>::new();
        for (name, value) in struct_.fields() {
            let constraint_name = name.expect_text()?;
            match AnyConstraint::read_constraint(constraint_name, value, ctx)? {
                Some(constraint) => builder.constraints.push(constraint),
                None => {
                    // TODO: Check if this particular open content is legal
                    builder
                        .open_content
                        .push((name.clone(), IonData::from(value.clone())))
                }
            }
        }
        Ok(Versioned::into_inner(
            builder.build_with_source_location(ion.isl_source_location()),
        ))
    }
}

impl HasIslSourceLocation for TypeDefinition {
    fn isl_source_location(&self) -> IslSourceLocation {
        self.source_location
    }
}
