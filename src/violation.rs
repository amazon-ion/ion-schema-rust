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
    NoTypesMatched,
    InvalidNull, // if the value is a null for type references that doesn't allow null
    MoreThanOneTypeMatched,
    TypeMatched,
    AllTypesNotMatched,
    TypeMismatched,
    FieldsNotMatched,
    InvalidOpenContent, // if a container contains open content when `content: closed` is specified
    TypeConstraintsUnsatisfied,
}

impl fmt::Display for ViolationCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ViolationCode::NoTypesMatched => "no_types_matched",
                ViolationCode::MoreThanOneTypeMatched => "more_than_one_type_matched",
                ViolationCode::TypeMatched => "type_matched",
                ViolationCode::AllTypesNotMatched => "all_types_not_matched",
                ViolationCode::TypeMismatched => "type_mismatched",
                ViolationCode::TypeConstraintsUnsatisfied => "type_constraints_unsatisfied",
                ViolationCode::InvalidNull => "invalid_null",
                ViolationCode::FieldsNotMatched => "fields_not_matched",
                ViolationCode::InvalidOpenContent => "invalid_open_content",
            }
        )
    }
}
