// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::constraints::AnyConstraintRef;
use crate::IonSchemaElement;
use std::ops::ControlFlow;

/// A trait for recording violations encountered during Ion Schema validation.
///
/// This trait enables customizable handling of constraint violations during schema validation.
/// Implementations can choose how to process, store, or respond to violations - from simply
/// counting them to building complex violation hierarchies.
///
/// The `accept` method returns [`ControlFlow`] to allow implementations to determine whether
/// validation should continue after each violation.
///
/// This crate provides implementations for [`Option<ViolationInfo>`] and [`Vec<ViolationInfo>`].
///
/// # Example
///
/// Custom violation tracking with early termination:
/// ```
/// use std::ops::ControlFlow;
/// use ion_schema::ViolationInfo;
/// use ion_schema::ViolationRecorder;
///
/// struct LimitedRecorder {
///     violations: Vec<String>,
///     max_violations: usize,
/// }
///
/// impl<'a> ViolationRecorder<'a> for LimitedRecorder {
///     fn accept(&mut self, violation: ViolationInfo<'a>) -> ControlFlow<()> {
///         self.violations.push(violation.message().to_string());
///         if self.violations.len() >= self.max_violations {
///             ControlFlow::Break(())
///         } else {
///             ControlFlow::Continue(())
///         }
///     }
/// }
/// // TODO: Add test case once ViolationRecorder is used by the validate method.
/// ```
/// # Implementation Notes
///
/// * Implementations should be prepared to handle multiple violations in any order
/// * The [`ControlFlow`] return value determines whether validation continues:
///   * `Continue` - Continue validation and collect more violations
///   * `Break` - Stop validation immediately
/// * Implementations may store as much or as little information about each violation as needed
pub trait ViolationRecorder<'a> {
    fn accept(&mut self, violation_info: ViolationInfo<'a>) -> ControlFlow<()>;
}

/// Information about a constraint violation that occurred during schema validation.
///
/// A `ViolationInfo` is created whenever a value fails to satisfy a constraint
/// during validation. It contains details about the constraint that failed,
/// the value that caused the violation, and contextual information about where
/// the violation occurred.
#[derive(Debug, Clone, PartialEq)]
pub struct ViolationInfo<'a> {
    /// The constraint that was violated.
    constraint: AnyConstraintRef<'a>,
    /// The value that failed to satisfy the constraint.
    value: IonSchemaElement<'a>,
    /// A human-readable description of why the value violated the constraint.
    message: String,
    // TODO: Wire these in later
    //   path: IonPath
}

impl<'a> ViolationInfo<'a> {
    pub(crate) fn new(
        constraint: AnyConstraintRef<'a>,
        value: IonSchemaElement<'a>,
        message: String,
    ) -> Self {
        Self {
            constraint,
            value,
            message,
        }
    }

    /// Returns a reference to the constraint that was violated.
    ///
    /// This can be useful for programmatically categorizing or filtering violations.
    pub fn constraint(&self) -> &AnyConstraintRef {
        &self.constraint
    }

    /// Returns a reference to the value that failed validation.
    ///
    /// This provides access to the specific Ion value that violated the constraint.
    pub fn value(&self) -> &IonSchemaElement<'a> {
        &self.value
    }

    /// Returns a human-readable message describing the violation.
    ///
    /// The message explains why the value failed to satisfy the constraint.
    /// For example: "expected a value between 1 and 10, but found -5"
    pub fn message(&self) -> &str {
        &self.message
    }

    // TODO: Add location once location metadata is available in Element
}

/// A fast-fail implementation that will stop validation as soon as any violation is found.
impl<'a> ViolationRecorder<'a> for Option<ViolationInfo<'a>> {
    fn accept(&mut self, violation_info: ViolationInfo<'a>) -> ControlFlow<()> {
        *self = Some(violation_info);
        ControlFlow::Break(())
    }
}

/// A simple implementation will collect all violations.
impl<'a> ViolationRecorder<'a> for Vec<ViolationInfo<'a>> {
    fn accept(&mut self, violation_info: ViolationInfo<'a>) -> ControlFlow<()> {
        self.push(violation_info);
        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use crate::model::constraints::{AnyConstraintRef, ContainerLength};
    use crate::violation_recorder::{ViolationInfo, ViolationRecorder};
    use crate::IonSchemaElement;
    use ion_rs::Element;
    use std::ops::ControlFlow;

    #[test]
    fn impl_violation_recorder_for_vec_violation_info() {
        let mut recorder: Vec<ViolationInfo> = vec![];

        let constraint = ContainerLength::new(1..);
        let element = Element::string("hello world");
        let vi = ViolationInfo::new(
            AnyConstraintRef::ContainerLength(&constraint),
            IonSchemaElement::from(&element),
            "fake violation".to_string(),
        );

        // We should be able to accept any number of ViolationInfos and always get "Continue".
        assert_eq!(recorder.accept(vi.clone()), ControlFlow::Continue(()));
        assert_eq!(recorder.accept(vi.clone()), ControlFlow::Continue(()));
        assert_eq!(recorder.accept(vi.clone()), ControlFlow::Continue(()));

        // ...and all the ViolationInfos should have been pushed into the vec
        assert_eq!(recorder, vec![vi.clone(), vi.clone(), vi])
    }

    #[test]
    fn impl_violation_recorder_for_option_violation_info() {
        let mut recorder: Option<ViolationInfo> = None;

        let constraint = ContainerLength::new(1..);
        let element = Element::string("hello world");
        let mut vi = ViolationInfo::new(
            AnyConstraintRef::ContainerLength(&constraint),
            IonSchemaElement::from(&element),
            "fake violation".to_string(),
        );

        // We should be able to accept any number of ViolationInfos and always get "Break".
        assert_eq!(recorder.accept(vi.clone()), ControlFlow::Break(()));
        assert_eq!(recorder, Some(vi.clone()));

        // And it's possible to re-use for another violation
        vi.message = "Another message".to_string();
        assert_eq!(recorder.accept(vi.clone()), ControlFlow::Break(()));
        assert_eq!(recorder, Some(vi));
    }
}
