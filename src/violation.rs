use std::fmt;
use std::fmt::Formatter;
use thiserror::Error;

/// Represents [Violation] found during validation with detailed error message, error code and the constraint for which the validation failed
#[derive(Debug, Clone, Error)]
pub struct Violation {
    constraint: String, // represents the constraint that created this violation
    code: String,       // represents an error code that indicates the type of the violation
    message: String,    // represents the detailed error message for this violation
    violations: Vec<Violation>,
}

impl Violation {
    pub fn new<A: AsRef<str>>(constraint: A, code: A, message: A) -> Self {
        Self {
            constraint: constraint.as_ref().to_owned(),
            code: code.as_ref().to_owned(),
            message: message.as_ref().to_owned(),
            violations: vec![],
        }
    }

    pub fn with_violations<A: AsRef<str>>(
        constraint: A,
        code: A,
        message: A,
        violations: Vec<Violation>,
    ) -> Self {
        Self {
            constraint: constraint.as_ref().to_owned(),
            code: code.as_ref().to_owned(),
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
