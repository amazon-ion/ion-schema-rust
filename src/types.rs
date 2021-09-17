use crate::constraint::{AllOfConstraint, Constraint, TypeConstraint};
use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_type::IslType;
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
    pub fn parse_from_isl_type_and_update_type_store(
        isl_type: &IslType,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // add parent information for named type
        if type_name.is_some() {
            pending_types.add_parent(type_name.to_owned().unwrap())
        }

        // add this unresolved type to context for type_id
        let type_id = pending_types.add_type();

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

        let type_def = TypeDefinition::new(type_name.to_owned(), constraints);

        // update with this resolved type_def to context for type_id
        let type_name = type_def.name();
        match type_name {
            Some(name) => {
                pending_types.update_named_type(type_id, name, type_def.to_owned(), type_store)
            }
            None => pending_types.update_anonymous_type(type_id, type_def.to_owned()),
        };

        // clear parent information from type_store as the type is already added in the type_store now
        if type_name.is_some() {
            pending_types.clear_parent();
        }

        Ok(type_def)
    }
}

impl PartialEq for TypeDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.constraints == other.constraints()
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

#[cfg(test)]
mod type_definition_tests {
    use super::*;
    use crate::constraint::{AllOfConstraint, Constraint, TypeConstraint};
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
        IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]),
        TypeDefinition::new(None, vec![Constraint::Type(TypeConstraint::new(1))])
    ),
    case::type_constraint_with_named_type(
        /* For a schema with named type as below: 
            { name: my_int, type: int }
         */
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]),
        TypeDefinition::new(Some("my_int".to_owned()), vec![Constraint::Type(TypeConstraint::new(1))])
    ),
    case::type_constraint_with_self_reference_type(
        /* For a schema with self reference type as below: 
            { name: my_int, type: my_int }
         */
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))]),
        TypeDefinition::new(Some("my_int".to_owned()), vec![Constraint::Type(TypeConstraint::new(0))])
    ),
    case::type_constraint_with_nested_self_reference_type(
        /* For a schema with nested self reference type as below: 
            { name: my_int, type: { type: my_int } }
         */
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))]),
        TypeDefinition::new(Some("my_int".to_owned()), vec![Constraint::Type(TypeConstraint::new(1))])
    ),
    case::type_constraint_with_nested_type(
        /* For a schema with nested types as below: 
            { name: my_int, type: { type: int } }
         */
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])))]),
        TypeDefinition::new(Some("my_int".to_owned()), vec![Constraint::Type(TypeConstraint::new(1))])
    ),
    case::type_constraint_with_nested_multiple_types(
        /* For a schema with nested multiple types as below: 
            { name: my_int, type: { type: int }, type: { type: my_int } }
         */
        IslType::new(Some("my_int".to_owned()), vec![IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))), IslConstraint::Type(IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))]),
        TypeDefinition::new(Some("my_int".to_owned()), vec![Constraint::Type(TypeConstraint::new(1)), Constraint::Type(TypeConstraint::new(3))])
    ),
    case::all_of_constraint(
        /* For a schema with all_of type as below: 
            { all_of: [{ type: int }] }
        */
        IslType::new(None, vec![IslConstraint::AllOf(vec![IslTypeRef::AnonymousType(IslType::new(None, vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))])]),
        TypeDefinition::new(None, vec![Constraint::AllOf(AllOfConstraint::new(vec![1]))])
    ),
    )]
    fn isl_type_to_type_definition(isl_type: IslType, type_def: TypeDefinition) {
        // assert if both the IslType are same in terms of constraints and name
        let type_store = &mut TypeStore::new();
        let pending_types = &mut PendingTypes::new();
        let this_type_def = TypeDefinition::parse_from_isl_type_and_update_type_store(
            &isl_type,
            type_store,
            pending_types,
        )
        .unwrap();
        assert_eq!(this_type_def, type_def);
    }
}
