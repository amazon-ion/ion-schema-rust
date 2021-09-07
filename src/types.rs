use crate::constraint::{AllOfConstraint, Constraint, TypeConstraint};
use crate::isl::{IslConstraint, IslType};
use crate::result::IonSchemaResult;
use crate::system::{SharedTypeStore, TypeId, TypeStore};
use crate::violation::Violations;
use ion_rs::value::owned::OwnedElement;
use std::rc::Rc;

/// Provides validation for Type
pub trait TypeValidator {
    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: &OwnedElement) -> bool;

    /// Returns a Violations object indicating whether the specified value
    /// is valid for this type, and if not, provides details as to which
    /// constraints were violated.
    fn validate(&self, value: &OwnedElement, issues: &mut Violations);
}

/// Provides a public facing schema type which has a reference to TypeStore
/// to get the underlying TypeDefinition from TypeStore
struct TypeRef {
    id: TypeId,
    type_store: Rc<TypeStore>,
}

/// A Type consists of an optional name and zero or more constraints.
///
/// Unless otherwise specified, the constraint `type: any` is automatically applied.
#[derive(Debug, Clone)]
pub struct TypeDefinition {
    name: Option<String>,
    constraints: Vec<Constraint>,
}

impl TypeDefinition {
    pub fn new(name: Option<String>, constraints: Vec<Constraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedStruct] to a schema [Type]
    pub fn parse_from_isl_type(
        isl_type: &IslType,
        type_store: &SharedTypeStore,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // add parent information for named type
        if type_name.is_some() {
            type_store
                .borrow_mut()
                .add_parent(type_name.to_owned().unwrap())
        }

        // convert IslConstraint to Constraint
        for isl_constraint in isl_type.constraints() {
            let constraint = match isl_constraint {
                IslConstraint::AllOf(type_references) => {
                    let all_of: AllOfConstraint =
                        AllOfConstraint::parse_from_isl_constraint(type_references, type_store)?;
                    Constraint::AllOf(all_of)
                }
                IslConstraint::Type(type_reference) => {
                    let type_constraint: TypeConstraint =
                        TypeConstraint::parse_from_isl_constraint(type_reference, type_store)?;
                    Constraint::Type(type_constraint)
                }
            };
            constraints.push(constraint);
        }
        Ok(TypeDefinition::new(type_name.to_owned(), constraints))
    }
}

impl TypeValidator for TypeDefinition {
    fn is_valid(&self, value: &OwnedElement) -> bool {
        todo!()
    }

    fn validate(&self, value: &OwnedElement, issues: &mut Violations) {
        todo!()
    }
}
