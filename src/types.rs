use crate::constraint::Constraint;
use crate::isl::isl_type::IslTypeImpl;
use crate::result::IonSchemaResult;
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::violation::{Violation, ViolationLeaf};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::Element;
use ion_rs::IonType;
use std::rc::Rc;

/// Provides validation for Type
pub trait TypeValidator {
    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: &OwnedElement) -> bool;

    /// Returns a Violations object indicating whether the specified value
    /// is valid for this type, and if not, provides details as to which
    /// constraints were violated.
    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore);
}

// Provides a public facing schema type which has a reference to TypeStore
// to get the underlying TypeDefinition from TypeStore
#[derive(Debug, Clone)]
pub struct TypeRef {
    id: TypeId,
    type_store: Rc<TypeStore>,
}

impl TypeRef {
    pub fn new(id: TypeId, type_store: Rc<TypeStore>) -> Self {
        Self { id, type_store }
    }

    pub fn validate(&self, value: &OwnedElement, issues: &mut impl Violation) {
        let type_def = self.type_store.get_type_by_id(self.id).unwrap();
        for constraint in type_def.constraints() {
            constraint.validate(value, issues, &self.type_store);
        }
    }
}

/// Represents a [TypeDefinition] which stores a resolved ISL type using [TypeStore]
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Named(TypeDefinitionImpl),
    Anonymous(TypeDefinitionImpl),
    Core(IonType),
}

impl TypeDefinition {
    /// Creates a [IslType::Named] using the [IslConstraint] defined within it
    pub fn named<A: Into<String>, B: Into<Vec<Constraint>>>(
        name: A,
        constraints: B,
    ) -> TypeDefinition {
        TypeDefinition::Named(TypeDefinitionImpl::new(
            Some(name.into()),
            constraints.into(),
        ))
    }

    /// Creates a [IslType::Anonymous] using the [IslConstraint] defined within it
    pub fn anonymous<A: Into<Vec<Constraint>>>(constraints: A) -> TypeDefinition {
        TypeDefinition::Anonymous(TypeDefinitionImpl::new(None, constraints.into()))
    }
    /// Provides the underlying constraints of [TypeDefinitionImpl]
    pub fn constraints(&self) -> &[Constraint] {
        match &self {
            TypeDefinition::Named(named_type) => named_type.constraints(),
            TypeDefinition::Anonymous(anonymous_type) => anonymous_type.constraints(),
            _ => &[],
        }
    }
}

impl TypeValidator for TypeDefinition {
    fn is_valid(&self, value: &OwnedElement) -> bool {
        todo!()
    }

    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        match self {
            TypeDefinition::Named(named_type) => {
                named_type.validate(value, issues, type_store);
            }
            TypeDefinition::Anonymous(anonymous_type) => {
                anonymous_type.validate(value, issues, type_store);
            }
            TypeDefinition::Core(ion_type) => {
                if value.ion_type() != *ion_type {
                    issues.add_violation(ViolationLeaf::new(
                        "type_constraint",
                        "type_mismatched",
                        &format!("expected type {:?}, found {:?}", ion_type, value.ion_type()),
                    ));
                }
            }
        };
    }
}

/// A [TypeDefinitionImpl] consists of an optional name and zero or more constraints.
#[derive(Debug, Clone)]
pub struct TypeDefinitionImpl {
    name: Option<String>,
    constraints: Vec<Constraint>,
}

impl TypeDefinitionImpl {
    pub fn new(name: Option<String>, constraints: Vec<Constraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn with_name(self, alias: String) -> Self {
        Self {
            name: Some(alias),
            constraints: self.constraints,
        }
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedStruct] to a schema [Type]
    pub fn parse_from_isl_type_and_update_type_store(
        isl_type: &IslTypeImpl,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // add parent information for named type
        if type_name.is_some() {
            pending_types.add_parent(type_name.to_owned().unwrap());
        }

        // add this unresolved type to context for type_id
        let type_id = pending_types.add_type(type_store);

        // convert IslConstraint to Constraint
        for isl_constraint in isl_type.constraints() {
            let constraint =
                Constraint::resolve_from_isl_constraint(isl_constraint, type_store, pending_types)?;
            constraints.push(constraint);
        }

        let type_def = TypeDefinitionImpl::new(type_name.to_owned(), constraints);

        if type_name.is_some() {
            // update with this resolved type_def to context for type_id
            pending_types.update_named_type(
                type_id,
                type_name.as_ref().unwrap(),
                type_def.to_owned(),
                type_store,
            );

            // clear parent information from type_store as the type is already added in the type_store now
            pending_types.clear_parent();
        } else {
            // update with this resolved type_def to context for type_id
            pending_types.update_anonymous_type(type_id, type_def.to_owned(), type_store);
        }

        Ok(type_def)
    }
}

impl PartialEq for TypeDefinitionImpl {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.constraints == other.constraints()
    }
}

impl TypeValidator for TypeDefinitionImpl {
    fn is_valid(&self, value: &OwnedElement) -> bool {
        todo!()
    }

    fn validate(&self, value: &OwnedElement, issues: &mut impl Violation, type_store: &TypeStore) {
        for constraint in self.constraints() {
            constraint.validate(value, issues, type_store);
        }
    }
}

#[cfg(test)]
mod type_definition_tests {
    use super::*;
    use crate::constraint::Constraint;
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type::IslType;
    use crate::isl::isl_type_reference::IslTypeRef;
    use crate::system::PendingTypes;
    use ion_rs::IonType;
    use rstest::*;

    #[rstest(
    isl_type, type_def,
    case::type_constraint_with_anonymous_type(
        /* For a schema with single anonymous type as below:
            { type: int }
         */
        IslType::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))]),
        TypeDefinition::anonymous([Constraint::type_constraint(1)])
    ),
    case::type_constraint_with_named_type(
        /* For a schema with named type as below:
            { name: my_int, type: int }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(1)])
    ),
    case::type_constraint_with_self_reference_type(
        /* For a schema with self reference type as below:
            { name: my_int, type: my_int }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::named("my_int"))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(0)])
    ),
    case::type_constraint_with_nested_self_reference_type(
        /* For a schema with nested self reference type as below:
            { name: my_int, type: { type: my_int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(1)])
    ),
    case::type_constraint_with_nested_type(
        /* For a schema with nested types as below:
            { name: my_int, type: { type: int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(1)])
    ),
    case::type_constraint_with_nested_multiple_types(
        /* For a schema with nested multiple types as below:
            { name: my_int, type: { type: int }, type: { type: my_int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))])), IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(1), Constraint::type_constraint(3)])
    ),
    case::all_of_constraint(
        /* For a schema with all_of type as below:
            { all_of: [{ type: int }] }
        */
        IslType::anonymous([IslConstraint::all_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))])])]),
        TypeDefinition::anonymous([Constraint::all_of([1])])
    ),
    case::any_of_constraint(
        /* For a schema with any_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        IslType::anonymous([IslConstraint::any_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Decimal))])])]),
        TypeDefinition::anonymous([Constraint::any_of([1, 3])])
    ),
    case::one_of_constraint(
        /* For a schema with one_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        IslType::anonymous([IslConstraint::one_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Decimal))])])]),
        TypeDefinition::anonymous([Constraint::one_of([1, 3])])
    ),
    case::not_constraint(
        /* For a schema with not constraint as below:
            { not: { type: int } }
        */
        IslType::anonymous([IslConstraint::not(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::core(IonType::Integer))]))]),
        TypeDefinition::anonymous([Constraint::not(1)])
    ),
    )]
    fn isl_type_to_type_definition(isl_type: IslType, type_def: TypeDefinition) {
        // assert if both the TypeDefinition are same in terms of constraints and name
        let type_store = &mut TypeStore::new();
        let pending_types = &mut PendingTypes::new();
        let this_type_def = match isl_type {
            IslType::Named(named_isl_type) => TypeDefinition::Named(
                TypeDefinitionImpl::parse_from_isl_type_and_update_type_store(
                    &named_isl_type,
                    type_store,
                    pending_types,
                )
                .unwrap(),
            ),
            IslType::Anonymous(anonymous_isl_type) => TypeDefinition::Anonymous(
                TypeDefinitionImpl::parse_from_isl_type_and_update_type_store(
                    &anonymous_isl_type,
                    type_store,
                    pending_types,
                )
                .unwrap(),
            ),
        };
        assert_eq!(this_type_def, type_def);
    }
}
