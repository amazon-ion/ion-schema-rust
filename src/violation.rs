use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

/// Represents [Violation] found during validation with detailed error message, error code and the constraint for which the validation failed
#[derive(Debug, Clone, Error)]
pub struct Violation {
    constraint: String,  // represents the constraint that created this violation
    code: ViolationCode, // represents an error code that indicates the type of the violation
    message: String,     // represents the detailed error message for this violation
    violations: Vec<Violation>,
}

impl Violation {
    pub fn new<A: AsRef<str>>(constraint: A, code: ViolationCode, message: A) -> Self {
        Self {
            constraint: constraint.as_ref().to_owned(),
            code,
            message: message.as_ref().to_owned(),
            violations: vec![],
        }
    }

    pub fn with_violations<A: AsRef<str>>(
        constraint: A,
        code: ViolationCode,
        message: A,
        violations: Vec<Violation>,
    ) -> Self {
        Self {
            constraint: constraint.as_ref().to_owned(),
            code,
            message: message.as_ref().to_owned(),
            violations,
        }
    }

    pub fn violations(&self) -> &[Violation] {
        &self.violations
    }
}

impl fmt::Display for Violation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "A validation error occurred: {}", self.message)
    }
}

/// Represents violation code that indicates the type of the violation
#[derive(Debug, Clone)]
pub enum ViolationCode {
    AllTypesNotMatched,
    AnnotationMismatched,
    ElementMismatched, // this is used for mismatched elements in containers
    FieldsNotMatched,
    InvalidLength, // this is ued for any length related constraints (e.g. container_length, byte_length, codepoint_length)
    InvalidNull,   // if the value is a null for type references that doesn't allow null
    InvalidOpenContent, // if a container contains open content when `content: closed` is specified
    MissingAnnotation, // if the annotation is missing for annotations constraint
    MissingValue,  // if the ion value is missing for a particular constraint
    MoreThanOneTypeMatched,
    NoTypesMatched,
    TypeConstraintsUnsatisfied,
    TypeMatched,
    TypeMismatched,
    UnexpectedAnnotation, // if unexpected annotation is found for annotations constraint
}

impl fmt::Display for ViolationCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ViolationCode::AllTypesNotMatched => "all_types_not_matched",
                ViolationCode::AnnotationMismatched => "annotation_mismatched",
                ViolationCode::ElementMismatched => "element_mismatched",
                ViolationCode::FieldsNotMatched => "fields_not_matched",
                ViolationCode::InvalidLength => "invalid_length",
                ViolationCode::InvalidNull => "invalid_null",
                ViolationCode::InvalidOpenContent => "invalid_open_content",
                ViolationCode::MissingAnnotation => "missing_annotation",
                ViolationCode::MissingValue => "missing_value",
                ViolationCode::MoreThanOneTypeMatched => "more_than_one_type_matched",
                ViolationCode::NoTypesMatched => "no_types_matched",
                ViolationCode::TypeConstraintsUnsatisfied => "type_constraints_unsatisfied",
                ViolationCode::TypeMatched => "type_matched",
                ViolationCode::TypeMismatched => "type_mismatched",
                ViolationCode::UnexpectedAnnotation => "unexpected_annotation",
            }
        )
    }
}
