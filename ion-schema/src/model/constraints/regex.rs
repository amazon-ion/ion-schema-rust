// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::constraint::RegexConstraint;
use crate::internal_traits::{
    LoaderContext, ValidateInternal, ValidationContext, WriteAsIsl, WriteContext,
};
use crate::model::constraints::{ConstraintName, ReadConstraint};
use crate::model::type_argument::TypeArgument;
use crate::model::TypeDefinitionBuilder;
use crate::resolver::*;
use crate::result::{invalid_schema_error_raw, IonSchemaResult};
use crate::{IonSchemaElement, IslVersion, Versioned, ViolationRecorder};
use ion_rs::{Element, ValueWriter};
use regex::RegexBuilder;
use std::marker::PhantomData;
use std::ops::ControlFlow;

impl ConstraintName for Regex {
    const CONSTRAINT_NAME: &'static str = "regex";
}

/// Builder for regular expressions in Ion Schema.
// TODO: Consider making this `pub`.
#[derive(Clone, PartialEq, Debug)]
pub struct IonSchemaRegexBuilder<V: IslVersion> {
    // The `V` type parameter allows us to safely add new fields in new Ion Schema
    // versions without breaking compatibility.
    _version: PhantomData<V>,
    case_insensitive: bool,
    multiline: bool,
    pattern: String,
}
impl<V: IslVersion> IonSchemaRegexBuilder<V> {
    fn new(pattern: &str) -> Self {
        Self {
            _version: Default::default(),
            case_insensitive: false,
            multiline: false,
            pattern: String::from(pattern),
        }
    }

    fn build(self) -> IonSchemaResult<Versioned<Regex, V>> {
        // TODO: Clean up the different IslVersion representations
        let version = match V::MAJOR_MINOR {
            (1, 0) => crate::isl::IslVersion::V1_0,
            (2, 0) => crate::isl::IslVersion::V2_0,
            _ => unreachable!(),
        };

        // Apply ISL specific regex validation
        let pattern = RegexConstraint::convert_to_pattern(self.pattern, version)?;

        let regex = RegexBuilder::new(&pattern)
            // TODO: See if this would fix https://github.com/amazon-ion/ion-rust/issues/399
            //.crlf(true)
            .case_insensitive(self.case_insensitive)
            .multi_line(self.multiline)
            .build()
            .map_err(|e| invalid_schema_error_raw(format!("Invalid regex: {pattern}")))?;

        let constraint = Regex {
            regex,
            multiline: self.multiline,
            case_insensitive: self.case_insensitive,
        };

        Ok(Versioned::new(constraint))
    }
}

/// Trait for something that can be used to create a new [`IonSchemaRegexBuilder`].
pub trait IonSchemaRegexSource<V: IslVersion>: Into<IonSchemaRegexBuilder<V>>
where
    Self: Sized,
{
    fn multiline(self, yes: bool) -> impl IonSchemaRegexSource<V> {
        self.into().multiline(yes)
    }

    fn case_insensitive(self, yes: bool) -> impl IonSchemaRegexSource<V> {
        self.into().case_insensitive(yes)
    }

    fn build_regex(self) -> IonSchemaResult<Versioned<Regex, V>> {
        self.into().build()
    }
}

impl<V: IslVersion, S: AsRef<str>> IonSchemaRegexSource<V> for S {}

impl<V: IslVersion, S: AsRef<str>> From<S> for IonSchemaRegexBuilder<V> {
    fn from(value: S) -> Self {
        IonSchemaRegexBuilder::new(value.as_ref())
    }
}

impl<V: IslVersion> IonSchemaRegexSource<V> for IonSchemaRegexBuilder<V> {
    fn multiline(mut self, yes: bool) -> impl IonSchemaRegexSource<V> {
        self.multiline = yes;
        self
    }

    fn case_insensitive(mut self, yes: bool) -> impl IonSchemaRegexSource<V> {
        self.case_insensitive = yes;
        self
    }
}

impl<V: IslVersion> From<Versioned<Regex, V>> for IonSchemaRegexBuilder<V> {
    fn from(value: Versioned<Regex, V>) -> Self {
        let value = value.into_inner();
        IonSchemaRegexBuilder {
            _version: Default::default(),
            case_insensitive: value.case_insensitive,
            multiline: value.multiline,
            pattern: value.regex.to_string(),
        }
    }
}

impl<V: IslVersion> IonSchemaRegexSource<V> for Versioned<Regex, V> {
    fn build_regex(self) -> IonSchemaResult<Versioned<Regex, V>> {
        Ok(self)
    }
}

/// Represents the `regex` constraint (ref. [ISL 1.0], [ISL 2.0]).
///
/// [ISL 1.0]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#regex
/// [ISL 2.0]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#regex
#[derive(Debug, Clone)]
pub struct Regex {
    regex: regex::Regex,
    multiline: bool,
    case_insensitive: bool,
}
impl_type_ref_walker!(Regex);

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        // We have to manually implement `PartialEq` because `regex::Regex` does not implement it.
        self.regex.as_str() == other.regex.as_str()
            && self.case_insensitive == other.case_insensitive
            && self.multiline == other.multiline
    }
}

impl Regex {
    fn pattern(&self) -> &str {
        self.regex.as_str()
    }
}

impl<V: IslVersion> TypeDefinitionBuilder<V> {
    /// Adds a `regex` constraint to the type being built.
    ///
    /// This will panic if the input is invalid. For a safe alternative, see [`Self::try_regex`].
    pub fn regex(self, regex: impl IonSchemaRegexSource<V>) -> Self {
        self.try_regex(regex).unwrap()
    }

    /// Adds a `regex` constraint to the type being built.
    pub fn try_regex(self, regex: impl IonSchemaRegexSource<V>) -> IonSchemaResult<Self> {
        Ok(self.with_constraint(Versioned::into_inner(regex.build_regex()?).into()))
    }
}

impl ValidateInternal for Regex {
    fn validate_internal<'top: 'call, 'call, R>(
        &'top self,
        value: &IonSchemaElement<'top>,
        ctx: &ValidationContext,
        recorder: &'call mut R,
    ) -> ControlFlow<()>
    where
        R: ViolationRecorder<'top>,
    {
        todo!()
    }
}

impl<V: IslVersion> WriteAsIsl<V> for Regex
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

impl<V: IslVersion> ReadConstraint<V> for Regex {
    fn read_constraint(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Option<Self>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::constraints::AnyConstraint;
    use crate::ISL_1_0;

    #[test]
    fn test_builder() {
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new().regex("abc").build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: false,
                case_insensitive: false,
            })]
        );

        let my_regex = IonSchemaRegexBuilder::new("abc")
            .case_insensitive(true)
            .multiline(true)
            .build_regex()
            .unwrap();
        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .regex(my_regex)
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(Regex {
                regex: RegexBuilder::new("abc")
                    .case_insensitive(true)
                    .multi_line(true)
                    .build()
                    .unwrap(),
                multiline: true,
                case_insensitive: true,
            })]
        );

        let type_ = TypeDefinitionBuilder::<ISL_1_0>::new()
            .regex("abc".multiline(true).case_insensitive(true))
            .build();

        assert_eq!(
            type_.constraints().cloned().collect::<Vec<_>>(),
            vec![AnyConstraint::Regex(Regex {
                regex: RegexBuilder::new("abc")
                    .case_insensitive(true)
                    .multi_line(true)
                    .build()
                    .unwrap(),
                multiline: true,
                case_insensitive: true,
            })]
        )
    }

    #[test]
    fn test_partial_eq() {
        let regex_constraint = Regex {
            regex: regex::Regex::new("abc").unwrap(),
            multiline: true,
            case_insensitive: true,
        };

        assert_eq!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: true,
                case_insensitive: true,
            }
        );
        assert_ne!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: true,
                case_insensitive: false,
            }
        );
        assert_ne!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("abc").unwrap(),
                multiline: false,
                case_insensitive: true,
            }
        );
        assert_ne!(
            regex_constraint,
            Regex {
                regex: regex::Regex::new("def").unwrap(),
                multiline: true,
                case_insensitive: true,
            }
        );
    }
}
