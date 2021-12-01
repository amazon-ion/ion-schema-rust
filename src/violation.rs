/// Represents [Violation] found during validation with detailed error message, error code and the constraint for which the validation failed
#[derive(Debug, Clone)]
pub struct Violation {
    constraint: String,      // represents the constraint that created this violation
    pub(crate) code: String, // represents an error code that indicates the type of the violation
    pub(crate) message: String, // represents the detailed error message for this violation
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

    pub fn add_violation(&mut self, violation: Violation) {
        self.violations.push(violation);
    }

    pub fn is_valid(&self) -> bool {
        self.violations.is_empty()
    }

    pub fn violations(&self) -> &[Violation] {
        &self.violations
    }
}
