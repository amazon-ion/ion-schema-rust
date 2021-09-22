use crate::constraint::{AllOfConstraint, Constraint, TypeConstraint};
use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_type::{AnonymousIslType, NamedIslType};
use crate::result::IonSchemaResult;
use crate::system::{PendingTypes, TypeId, TypeStore};
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
}

/// Represents a [TypeDefinition] which stores a resolved ISL type using [TypeStore]
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    NamedTypeDefinition(NamedTypeDefinition),
    AnonymousTypeDefinition(AnonymousTypeDefinition),
}

/// A [AnonymousTypeDefinition] consists of a name and zero or more constraints.
#[derive(Debug, Clone)]
pub struct AnonymousTypeDefinition {
    constraints: Vec<Constraint>,
}

impl AnonymousTypeDefinition {
    pub fn new(constraints: Vec<Constraint>) -> Self {
        Self { constraints }
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedStruct] to a schema [Type]
    pub fn parse_from_isl_type_and_update_type_store(
        isl_type: &AnonymousIslType,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // add this unresolved type to context for type_id
        let type_id = pending_types.add_type(type_store);

        // convert IslConstraint to Constraint
        for isl_constraint in isl_type.constraints() {
            let constraint = match isl_constraint {
                IslConstraint::AllOf(type_references) => {
                    let all_of: AllOfConstraint = AllOfConstraint::resolve_from_isl_constraint(
                        type_references,
                        type_store,
                        pending_types,
                    )?;
                    Constraint::AllOf(all_of)
                }
                IslConstraint::Type(type_reference) => {
                    let type_constraint: TypeConstraint =
                        TypeConstraint::resolve_from_isl_constraint(
                            type_reference,
                            type_store,
                            pending_types,
                        )?;
                    Constraint::Type(type_constraint)
                }
            };
            constraints.push(constraint);
        }

        let type_def = AnonymousTypeDefinition::new(constraints);

        // update with this resolved type_def to context for type_id
        pending_types.update_anonymous_type(type_id, type_def.to_owned(), type_store);

        Ok(type_def)
    }
}

impl PartialEq for AnonymousTypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.constraints == other.constraints()
    }
}

impl TypeValidator for AnonymousTypeDefinition {
    fn is_valid(&self, value: &OwnedElement) -> bool {
        todo!()
    }

    fn validate(&self, value: &OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

/// A [NamedTypeDefinition] consists of a name and zero or more constraints.
#[derive(Debug, Clone)]
pub struct NamedTypeDefinition {
    name: String,
    constraints: Vec<Constraint>,
}

impl NamedTypeDefinition {
    pub fn new(name: String, constraints: Vec<Constraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Parse constraints inside an [OwnedStruct] to a schema [Type]
    pub fn parse_from_isl_type_and_update_type_store(
        isl_type: &NamedIslType,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // add parent information for named type
        pending_types.add_parent(type_name.to_owned());

        // add this unresolved type to context for type_id
        let type_id = pending_types.add_type(type_store);

        // convert IslConstraint to Constraint
        for isl_constraint in isl_type.constraints() {
            let constraint = match isl_constraint {
                IslConstraint::AllOf(type_references) => {
                    let all_of: AllOfConstraint = AllOfConstraint::resolve_from_isl_constraint(
                        type_references,
                        type_store,
                        pending_types,
                    )?;
                    Constraint::AllOf(all_of)
                }
                IslConstraint::Type(type_reference) => {
                    let type_constraint: TypeConstraint =
                        TypeConstraint::resolve_from_isl_constraint(
                            type_reference,
                            type_store,
                            pending_types,
                        )?;
                    Constraint::Type(type_constraint)
                }
            };
            constraints.push(constraint);
        }

        let type_def = NamedTypeDefinition::new(type_name.to_owned(), constraints);

        // update with this resolved type_def to context for type_id
        pending_types.update_named_type(type_id, type_name, type_def.to_owned(), type_store);

        // clear parent information from type_store as the type is already added in the type_store now
        pending_types.clear_parent();

        Ok(type_def)
    }
}

impl PartialEq for NamedTypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.constraints == other.constraints()
    }
}

impl TypeValidator for NamedTypeDefinition {
    fn is_valid(&self, value: &OwnedElement) -> bool {
        todo!()
    }

    fn validate(&self, value: &OwnedElement, issues: &mut Violations) {
        todo!()
    }
}

#[cfg(test)]
mod type_definition_tests {
    use super::*;
    use crate::constraint::{AllOfConstraint, Constraint, TypeConstraint};
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type::{IslType, NamedIslType};
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
        IslType::AnonymousIslType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])),
        TypeDefinition::AnonymousTypeDefinition(AnonymousTypeDefinition::new(vec![Constraint::Type(TypeConstraint::new(1))]))
    ),
    case::type_constraint_with_named_type(
        /* For a schema with named type as below:
            { name: my_int, type: int }
         */
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])),
        TypeDefinition::NamedTypeDefinition(NamedTypeDefinition::new("my_int".to_owned(), vec![Constraint::Type(TypeConstraint::new(1))]))
    ),
    case::type_constraint_with_self_reference_type(
        /* For a schema with self reference type as below:
            { name: my_int, type: my_int }
         */
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])),
        TypeDefinition::NamedTypeDefinition(NamedTypeDefinition::new("my_int".to_owned(), vec![Constraint::Type(TypeConstraint::new(0))]))
    ),
    case::type_constraint_with_nested_self_reference_type(
        /* For a schema with nested self reference type as below:
            { name: my_int, type: { type: my_int } }
         */
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))])),
        TypeDefinition::NamedTypeDefinition(NamedTypeDefinition::new("my_int".to_owned(), vec![Constraint::Type(TypeConstraint::new(1))]))
    ),
    case::type_constraint_with_nested_type(
        /* For a schema with nested types as below:
            { name: my_int, type: { type: int } }
         */
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])))])),
        TypeDefinition::NamedTypeDefinition(NamedTypeDefinition::new("my_int".to_owned(), vec![Constraint::Type(TypeConstraint::new(1))]))
    ),
    case::type_constraint_with_nested_multiple_types(
        /* For a schema with nested multiple types as below:
            { name: my_int, type: { type: int }, type: { type: my_int } }
         */
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))), IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))])),
        TypeDefinition::NamedTypeDefinition(NamedTypeDefinition::new("my_int".to_owned(), vec![Constraint::Type(TypeConstraint::new(1)), Constraint::Type(TypeConstraint::new(3))]))
    ),
    case::all_of_constraint(
        /* For a schema with all_of type as below:
            { all_of: [{ type: int }] }
        */
        IslType::AnonymousIslType(AnonymousIslType::new(vec![IslConstraint::AllOf(vec![IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))])])),
        TypeDefinition::AnonymousTypeDefinition(AnonymousTypeDefinition::new(vec![Constraint::AllOf(AllOfConstraint::new(vec![1]))]))
    ),
    )]
    fn isl_type_to_type_definition(isl_type: IslType, type_def: TypeDefinition) {
        // assert if both the TypeDefinition are same in terms of constraints and name
        let type_store = &mut TypeStore::new();
        let pending_types = &mut PendingTypes::new();
        let this_type_def = match isl_type {
            IslType::NamedIslType(named_isl_type) => TypeDefinition::NamedTypeDefinition(
                NamedTypeDefinition::parse_from_isl_type_and_update_type_store(
                    &named_isl_type,
                    type_store,
                    pending_types,
                )
                .unwrap(),
            ),
            IslType::AnonymousIslType(anonymous_isl_type) => {
                TypeDefinition::AnonymousTypeDefinition(
                    AnonymousTypeDefinition::parse_from_isl_type_and_update_type_store(
                        &anonymous_isl_type,
                        type_store,
                        pending_types,
                    )
                    .unwrap(),
                )
            }
        };
        assert_eq!(this_type_def, type_def);
    }
}
