// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::IonSchemaResult;
use crate::{IonSchemaElement, IslVersion, ViolationRecorder};
use ion_rs::{Element, IonData, ValueWriter};
use std::collections::HashSet;
use std::ops::ControlFlow;

impl ConstraintName for Contains {
    const CONSTRAINT_NAME: &'static str = "contains";
}

/// Represents the `contains` constraint (ref.  [ISL 1.0],[ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#contains
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#contains
#[derive(Debug, PartialEq, Clone)]
pub struct Contains {
    values: HashSet<IonData<Element>>,
}
impl_type_ref_walker!(Contains);

impl Contains {
    pub(crate) fn new<T: IntoIterator<Item = Element>>(values: T) -> Self {
        Self {
            values: values.into_iter().map(IonData::from).collect(),
        }
    }

    pub fn values(&self) -> impl Iterator<Item = &Element> {
        self.values.iter().map(IonData::as_ref)
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    pub fn contains<T: IntoIterator<Item = Element>>(self, values: T) -> Self {
        let constraint = Contains::new(values);
        self.with_constraint(constraint.into())
    }
}

impl ValidateInternal for Contains {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &'top IonSchemaElement,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!()
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Contains {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        todo!()
    }
}

impl<V: IslVersion> ReadConstraint<V> for Contains {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::model::constraints::{AnyConstraint, Contains};
    use crate::model::TypeDefinitionBuilder;
    use crate::ISL_1_0;
    use ion_rs::{Element, IonData};

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .contains([
                Element::boolean(true),
                Element::int(1),
                Element::string("abc"),
            ])
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Contains(Contains {
                values: [
                    IonData::from(Element::boolean(true)),
                    IonData::from(Element::int(1)),
                    IonData::from(Element::string("abc")),
                ]
                .into_iter()
                .collect(),
            })]
        );
    }
}
