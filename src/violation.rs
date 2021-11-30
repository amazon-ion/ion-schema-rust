/// Represents Violations found during validation of values based on schema types
pub trait Violation {
    /// adds a violation into the collection of [Violation]s
    fn add_violation(&mut self, violation: ViolationLeaf);

    /// Returns `true` if no violations were found, otherwise `false`
    fn is_valid(&self) -> bool;

    /// Returns the [Violation]s that were found during validation
    fn violations(&self) -> &[ViolationLeaf];

    /// Creates a checkpoint to track the number of violations from this point onwards
    fn checkpoint(&self) -> Checkpoint;
}

// Represents collection of [Violation]s
pub struct ViolationTree {
    violations: Vec<ViolationLeaf>,
}

impl ViolationTree {
    pub fn new() -> Self {
        Self { violations: vec![] }
    }
}

impl Violation for ViolationTree {
    fn add_violation(&mut self, violation: ViolationLeaf) {
        self.violations.push(violation);
    }

    fn is_valid(&self) -> bool {
        self.violations.is_empty()
    }

    fn violations(&self) -> &[ViolationLeaf] {
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

    pub fn is_valid(&self, violation: &ViolationLeaf) -> bool {
        self.violations_count == violation.violations.len()
    }
}

// Represents a single Violation with detailed error message, error code and the constraint for which the validation failed
#[derive(Debug, Clone)]
pub struct ViolationLeaf {
    constraint: String,      // represents the constraint that created this violation
    pub(crate) code: String, // represents an error code that indicates the type of the violation
    pub(crate) message: String, // represents the detailed error message for this violation
    violations: Vec<ViolationLeaf>,
}

impl ViolationLeaf {
    pub fn new<A: AsRef<str>>(constraint: A, code: A, message: A) -> Self {
        Self {
            constraint: constraint.as_ref().to_owned(),
            code: code.as_ref().to_owned(),
            message: message.as_ref().to_owned(),
            violations: vec![],
        }
    }
}

impl Violation for ViolationLeaf {
    fn add_violation(&mut self, violation: ViolationLeaf) {
        self.violations.push(violation);
    }

    fn is_valid(&self) -> bool {
        self.violations.is_empty()
    }

    fn violations(&self) -> &[ViolationLeaf] {
        &self.violations
    }

    fn checkpoint(&self) -> Checkpoint {
        Checkpoint::new(self.violations.len())
    }
}
