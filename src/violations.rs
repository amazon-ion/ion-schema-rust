use ion_rs::value::owned::OwnedElement;

#[derive(Debug, Clone)]
pub struct Violations {
    violations: Vec<Violation>,
    children: Vec<ViolationChild>,
    short_circuit: bool,
    children_allowed: bool
}

impl Violations {
    pub fn new(short_circuit: bool, children_allowed: bool) -> Self {
        let children = vec![];
        let violations =vec![];
        Self {
            violations,
            children,
            short_circuit,
            children_allowed
        }
    }

    pub(crate) fn is_valid(&self) -> bool {
        self.violations.is_empty() && self.children.is_empty()
    }

    pub(crate) fn add_violation(&mut self, violation: Violation) {
        self.violations.push(violation)
    }

    fn add_child(&self, child: ViolationChild) {
        todo!()
    }

    pub(crate) fn checkpoint(&self) -> Checkpoint {
        Checkpoint::new(self.violations.len(), self.children.len())
    }
}

// TODO: Fill the struct
#[derive(Debug, Clone)]
pub struct Violation {
    constraint: Option<OwnedElement>,
    code: Option<String>,
    message: Option<String>
}

impl Violation {
    pub fn new(constraint: Option<OwnedElement>, code: Option<String>, message: Option<String>) -> Self {
        Self {
            constraint,
            code,
            message,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ViolationChild {
    field_name: Option<String>,
    index: Option<i32>,
    value: Option<OwnedElement>
}

impl ViolationChild {
    fn add_value(v: OwnedElement) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Checkpoint {
    violation_count: usize,
    child_count: usize
}

impl Checkpoint {
    pub fn new(violation_count: usize, child_count: usize) -> Self {
        Self {
            violation_count,
            child_count
        }
    }
}