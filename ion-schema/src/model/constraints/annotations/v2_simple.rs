// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::*;
use crate::ion_extension::SymbolExtensions;
use crate::model::constraints::annotations::AnnotationsVariant;
use crate::model::constraints::AnnotationsV2Modifier::{Closed, ClosedAndRequired, Required};
use crate::model::constraints::{Annotations, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::result::{invalid_schema, IonSchemaResult};
use crate::{IonSchemaElement, ISL_1_0, ISL_2_0};
use ion_rs::{Element, Symbol, ValueWriter};
use std::ops::ControlFlow;

/// Modifier for the simple syntax of Ion Schema 2.0 `annotations` constraint.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AnnotationsV2Modifier {
    Closed,
    Required,
    ClosedAndRequired,
}
impl AnnotationsV2Modifier {
    fn is_closed(&self) -> bool {
        *self != AnnotationsV2Modifier::Required
    }
    fn is_required(&self) -> bool {
        *self != AnnotationsV2Modifier::Closed
    }
}

/// Represents the [annotations] constraint, simple syntax, in Ion Schema 2.0
///
/// [annotations]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#annotations
#[derive(Debug, PartialEq, Clone)]
pub struct AnnotationsV2Simple {
    modifier: AnnotationsV2Modifier,
    annotations: Vec<Symbol>,
}

impl AnnotationsV2Simple {
    fn new<T: Into<Symbol>>(modifier: AnnotationsV2Modifier, annotations: Vec<T>) -> Self {
        Self {
            modifier,
            annotations: annotations.into_iter().map(|it| it.into()).collect(),
        }
    }

    pub fn modifier(&self) -> AnnotationsV2Modifier {
        self.modifier
    }
    pub fn annotations(&self) -> &[Symbol] {
        self.annotations.as_slice()
    }

    pub(super) fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: FnMut(IonSchemaElement<'top>, String) -> ControlFlow<()>,
    {
        let Some(element) = value.as_element() else {
            return recorder(
                value.clone(),
                "document type is always invalid for annotations constraint".to_string(),
            );
        };

        let actual_annotations: Vec<&Symbol> = element.annotations().iter().collect();

        if self.modifier.is_closed() {
            let mut extra_anns = vec![];
            for &text in actual_annotations.iter() {
                if !self.annotations.iter().any(|it| it == text) {
                    extra_anns.push(text)
                }
            }
            if !extra_anns.is_empty() {
                recorder(
                    value.clone(),
                    format!("Unexpected extra annotations: {:?}", extra_anns),
                )?
            }
        }
        if self.modifier.is_required() {
            let mut missing_annotations = vec![];
            for required in &self.annotations {
                if !actual_annotations.contains(&required) {
                    missing_annotations.push(required)
                }
            }
            if !missing_annotations.is_empty() {
                recorder(
                    value.clone(),
                    format!("Missing required annotations: {:?}", missing_annotations),
                )?
            }
        }
        ControlFlow::Continue(())
    }
}

impl TypeDefinitionBuilder<ISL_2_0> {
    /// Adds an annotations constraint to the type definition using Ion Schema 2.0 simple annotation syntax.
    ///
    /// This method creates an annotations constraint that validates the presence and/or absence of
    /// annotations on Ion values. It supports Ion Schema 2.0's annotation modifiers for more
    /// fine-grained control over annotation validation.
    ///
    /// # Arguments
    /// * `modifier` - An [`AnnotationsV2Modifier`] that specifies how the annotations should be applied
    ///               (e.g., required, closed, or both)
    /// * `annotations` - An iterable collection of items that can be converted into [`Symbol`],
    ///                  representing the annotation names
    ///
    /// # Returns
    /// * Returns `Self` to allow for method chaining in the builder pattern
    ///
    /// # Examples
    /// ```
    /// # use ion_schema::ISL_2_0;
    /// # use ion_schema::model::TypeDefinition;
    /// use ion_schema::model::constraints::AnnotationsV2Modifier::*;
    /// let type_def = TypeDefinition::builder::<ISL_2_0>()
    ///     .annotations(Closed, ["foo", "bar"])
    ///     .build();
    /// ```
    pub fn annotations<S: Into<Symbol>, T: IntoIterator<Item = S>>(
        self,
        modifier: AnnotationsV2Modifier,
        annotations: T,
    ) -> Self {
        let constraint = AnnotationsV2Simple {
            modifier,
            annotations: annotations.into_iter().map(Into::into).collect(),
        };
        self.with_constraint(
            Annotations {
                variant: AnnotationsVariant::V2Simple(constraint),
            }
            .into(),
        )
    }
}

// Default implementation returns Err to indicate that this is unsupported.
impl WriteAsIsl<ISL_1_0> for AnnotationsV2Simple {}

impl WriteAsIsl<ISL_2_0> for AnnotationsV2Simple {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<ISL_2_0>,
    ) -> IonSchemaResult<()> {
        let writer = match self.modifier {
            Closed => writer.with_annotations(["closed"]),
            Required => writer.with_annotations(["required"]),
            ClosedAndRequired => writer.with_annotations(["closed", "required"]),
        }?;
        writer.write_list(&self.annotations)?;
        Ok(())
    }
}

impl ReadConstraint<ISL_1_0> for AnnotationsV2Simple {}

impl ReadConstraint<ISL_2_0> for AnnotationsV2Simple {
    fn read_constraint(
        ion: &Element,
        ctx: &LoaderContext<ISL_2_0>,
    ) -> IonSchemaResult<Option<Self>> {
        let required = ion.annotations().contains("required");
        let closed = ion.annotations().contains("closed");
        let annotations_len = ion.annotations().len();

        if annotations_len != (required as usize + closed as usize) {
            return invalid_schema!(
                "unexpected annotations on annotations constraint argument: {}",
                ion
            );
        }

        let modifier = match (closed, required) {
            (true, true) => ClosedAndRequired,
            (true, false) => Closed,
            (false, true) => Required,
            (false, false) => {
                return invalid_schema!("annotations must be closed, required, or both: {}", ion)
            }
        };
        let annotations = ion
            .as_list()
            .ok_or_else(|| {
                invalid_schema!(
                    "annotations constraint requires a list of annotations, found: {}",
                    ion
                )
            })?
            .iter()
            .map(|el| {
                el.expect_symbol()
                    .cloned()
                    .and_then(|symbol| symbol.expect_known_symbol())
            })
            .collect::<Result<Vec<Symbol>, _>>()?;

        Ok(Some(AnnotationsV2Simple {
            modifier,
            annotations,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::internal_traits::{LoaderContext, WriteAsIsl, WriteContext};
    use crate::ISL_2_0;
    use ion_rs::v1_0::Text;
    use ion_rs::{Element, SequenceWriter, Writer};
    use rstest::rstest;
    // TODO: Tests for TypeDefinitionBuilder function impl, once TypeDefinitionBuilder is further developed

    // validate_internal will be covered by ion-schema-tests

    #[rstest]
    #[case::closed("closed::[foo]", AnnotationsV2Simple::new(AnnotationsV2Modifier::Closed, vec!["foo"]) )]
    #[case::required("required::[foo]", AnnotationsV2Simple::new(AnnotationsV2Modifier::Required, vec!["foo"]) )]
    #[case::closed_and_required("closed::required::[foo]", AnnotationsV2Simple::new(AnnotationsV2Modifier::ClosedAndRequired, vec!["foo"]) )]
    #[case::empty_annotation_list("closed::[]", AnnotationsV2Simple { modifier: AnnotationsV2Modifier::Closed, annotations: vec![] } )]
    fn annotations_v2_simple_write_as_isl(
        #[case] expected_ion: &str,
        #[case] constraint: AnnotationsV2Simple,
    ) {
        let buffer = Vec::new();
        let mut writer = Writer::new(Text, buffer).unwrap();
        let ctx = WriteContext::<ISL_2_0>::new();
        constraint
            .write_as_isl(writer.value_writer(), &ctx)
            .unwrap();
        let output = writer.close().unwrap();

        let actual_element = Element::read_one(output);
        let expected_element = Element::read_one(expected_ion);

        assert_eq!(expected_element, actual_element);
    }

    #[rstest]
    #[case::closed("closed::[foo]", AnnotationsV2Simple::new(AnnotationsV2Modifier::Closed, vec!["foo"]))]
    #[case::required("required::[foo]", AnnotationsV2Simple::new(AnnotationsV2Modifier::Required, vec!["foo"]) )]
    #[case::closed_and_required("closed::required::[foo]", AnnotationsV2Simple::new(AnnotationsV2Modifier::ClosedAndRequired, vec!["foo"]) )]
    #[case::empty_annotation_list("closed::[]", AnnotationsV2Simple::new(AnnotationsV2Modifier::Closed, Vec::<String>::new()))]
    fn annotations_v2_simple_try_read_ok(#[case] ion: &str, #[case] expected: AnnotationsV2Simple) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = AnnotationsV2Simple::read_constraint(&element, &load_ctx);
        assert_eq!(result, Ok(Some(expected)))
    }

    #[rstest]
    #[case::annotations_list_must_have_a_modifier_annotation("[foo]")]
    #[case::required_may_not_be_repeated("required::required::[foo]")]
    #[case::closed_may_not_be_repeated("closed::closed::[foo]")]
    #[case::closed_and_required_are_the_only_allowed_annotations("required::ordered::[foo]")]
    #[case::annotations_cannot_be_non_text("required::[1]")]
    #[case::annotations_cannot_be_strings("required::[\"foo\"]")]
    #[case::annotations_must_be_in_a_list("required::(foo)")]
    #[case::annotations_cannot_have_unknown_text("required::[$0]")]
    fn annotations_v2_simple_try_read_err(#[case] ion: &str) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = AnnotationsV2Simple::read_constraint(&element, &load_ctx);
        assert!(result.is_err())
    }
}
