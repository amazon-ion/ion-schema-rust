/// Represents Violations found during validation of values based on schema types
pub trait Violations {
    /// adds a violation into the collection of [Violation]s
    fn add_violation(&mut self, violation: ViolationImpl);

    /// Returns `true` if no violations were found, otherwise `false`
    fn is_valid(&self) -> bool;

    /// Returns the [Violation]s that were found during validation
    fn violations(&self) -> &[ViolationImpl];

    /// Creates a checkpoint to track the number of violations from this point onwards
    fn checkpoint(&self) -> Checkpoint;
}

// Represents collection of [Violation]s
pub struct ViolationsImpl {
    violations: Vec<ViolationImpl>,
}

impl ViolationsImpl {
    pub fn new() -> Self {
        Self { violations: vec![] }
    }
}

impl Violations for ViolationsImpl {
    fn add_violation(&mut self, violation: ViolationImpl) {
        self.violations.push(violation);
    }

    fn is_valid(&self) -> bool {
        self.violations.is_empty()
    }

    fn violations(&self) -> &[ViolationImpl] {
        &self.violations
    }

    fn checkpoint(&self) -> Checkpoint {
        Checkpoint::new(self.violations.len())
    }
}

// TODO: this can be extended to store children violation count as well for container constraints
// Represents a checkpoint to be used while validation
// This determines if children or nested types/constraints have the same number of violations since the checkpoint was created
pub struct Checkpoint {
    violations_count: usize,
}

impl Checkpoint {
    pub fn new(violations_count: usize) -> Self {
        Self { violations_count }
    }

    pub fn is_valid(&self, violation: &ViolationImpl) -> bool {
        self.violations_count == violation.violations.len()
    }
}

// Represents a single Violation with detailed error message, error code and the constraint for which the validation failed
#[derive(Debug, Clone)]
pub struct ViolationImpl {
    constraint: String,      // represents the constraint that created this violation
    pub(crate) code: String, // represents an error code that indicates the type of the violation
    pub(crate) message: String, // represents the detailed error message for this violation
    violations: Vec<ViolationImpl>,
}

impl ViolationImpl {
    pub fn new<A: AsRef<str>>(constraint: A, code: A, message: A) -> Self {
        Self {
            constraint: constraint.as_ref().to_owned(),
            code: code.as_ref().to_owned(),
            message: message.as_ref().to_owned(),
            violations: vec![],
        }
    }
}

impl Violations for ViolationImpl {
    fn add_violation(&mut self, violation: ViolationImpl) {
        self.violations.push(violation);
    }

    fn is_valid(&self) -> bool {
        self.violations.is_empty()
    }

    fn violations(&self) -> &[ViolationImpl] {
        &self.violations
    }

    fn checkpoint(&self) -> Checkpoint {
        Checkpoint::new(self.violations.len())
    }
}
