use crate::constraint::Constraint;
use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_range::Range;
use crate::isl::isl_type::IslTypeImpl;
use crate::result::{IonSchemaResult, ValidationResult};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::violation::{Violation, ViolationCode};
use ion_rs::value::owned::{text_token, OwnedElement};
use ion_rs::value::{Builder, Element};
use ion_rs::IonType;
use std::rc::Rc;

/// Provides validation for Type
pub trait TypeValidator {
    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: &OwnedElement, type_store: &TypeStore) -> bool;

    /// Returns `Err(violation)` with details as to which constraints were violated,
    /// otherwise returns `Ok(())` indicating no violations were found during the validation
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult;
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

    pub fn validate(&self, value: &OwnedElement) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let type_def = self.type_store.get_type_by_id(self.id).unwrap();
        let type_name = match type_def {
            TypeDefinition::Named(named_type) => named_type.name().as_ref().unwrap().to_owned(),
            TypeDefinition::Anonymous(anonymous_type) => "".to_owned(),
            TypeDefinition::BuiltIn(builtin_isl_type) => match builtin_isl_type {
                BuiltInTypeDefinition::Atomic(ion_type, is_nullable) => match is_nullable {
                    Nullability::Nullable => format!("${}", ion_type),
                    Nullability::NotNullable => format!("{}", ion_type),
                },
                BuiltInTypeDefinition::Derived(other_type) => other_type.name().to_owned().unwrap(),
            },
        };
        for constraint in type_def.constraints() {
            if let Err(violation) = constraint.validate(value, &self.type_store) {
                violations.push(violation);
            }
        }
        if violations.is_empty() {
            return Ok(());
        }
        Err(Violation::with_violations(
            type_name.as_str(),
            ViolationCode::TypeConstraintsUnsatisfied,
            "value didn't satisfy type constraint(s)",
            violations,
        ))
    }
}

/// Represents a [BuiltInTypeDefinition] which stores a resolved builtin ISl type using [TypeStore]
#[derive(Debug, Clone, PartialEq)]
pub enum BuiltInTypeDefinition {
    Atomic(IonType, Nullability),
    Derived(TypeDefinitionImpl),
}

/// Represents whether an atomic built-in type is nullable or not
#[derive(Debug, Clone, PartialEq)]
pub enum Nullability {
    Nullable,
    NotNullable,
}

impl BuiltInTypeDefinition {
    pub fn parse_from_isl_type(
        isl_type: &IslTypeImpl,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // convert IslConstraint to Constraint
        for isl_constraint in isl_type.constraints() {
            // For built in types, open_content is set as true as Ion Schema by default allows open content
            let constraint = Constraint::resolve_from_isl_constraint(
                isl_constraint,
                type_store,
                pending_types,
                true,
            )?;
            constraints.push(constraint);
        }

        let builtin_type_def = BuiltInTypeDefinition::Derived(TypeDefinitionImpl::new(
            type_name.to_owned(),
            constraints,
        ));
        Ok(builtin_type_def)
    }
}

/// Represents a [TypeDefinition] which stores a resolved ISL type using [TypeStore]
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Named(TypeDefinitionImpl),
    Anonymous(TypeDefinitionImpl),
    BuiltIn(BuiltInTypeDefinition),
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

    /// Returns an occurs constraint as range if it exists in the [TypeDefinition] otherwise returns `occurs: required`
    pub fn get_occurs_constraint(&self, validation_constraint_name: &str) -> Range {
        // verify if the type_def contains `occurs` constraint and fill occurs_range
        // Otherwise if there is no `occurs` constraint specified then use `occurs: required`
        if let Some(Constraint::Occurs(occurs)) = self
            .constraints()
            .iter()
            .find(|c| matches!(c, Constraint::Occurs(_)))
        {
            return occurs.occurs_range().to_owned();
        }
        // by default, if there is no `occurs` constraint for given type_def
        // then use `occurs:optional` if its `fields` constraint validation
        // otherwise, use `occurs: required` if its `ordered_elements` constraint validation
        if validation_constraint_name == "fields" {
            return Range::optional();
        }
        Range::required()
    }
}

impl TypeValidator for TypeDefinition {
    fn is_valid(&self, value: &OwnedElement, type_store: &TypeStore) -> bool {
        let violation = self.validate(value, type_store);
        violation.is_ok()
    }

    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        match self {
            TypeDefinition::Named(named_type) => named_type.validate(value, type_store),
            TypeDefinition::Anonymous(anonymous_type) => anonymous_type.validate(value, type_store),
            TypeDefinition::BuiltIn(built_in_type) => match built_in_type {
                BuiltInTypeDefinition::Atomic(ion_type, is_nullable) => {
                    if *is_nullable == Nullability::NotNullable && value.is_null() {
                        return Err(Violation::new(
                            "type_constraint",
                            ViolationCode::InvalidNull,
                            &format!("expected type {:?} doesn't allow null", ion_type),
                        ));
                    }
                    if value.ion_type() != *ion_type {
                        return Err(Violation::new(
                            "type_constraint",
                            ViolationCode::TypeMismatched,
                            &format!("expected type {:?}, found {:?}", ion_type, value.ion_type()),
                        ));
                    }
                    Ok(())
                }
                BuiltInTypeDefinition::Derived(other_type) => {
                    other_type.validate(value, type_store)
                }
            },
        }
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
    pub fn parse_from_isl_type_and_update_pending_types(
        isl_type: &IslTypeImpl,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // add parent information for named type
        if type_name.is_some() {
            pending_types.add_parent(type_name.to_owned().unwrap(), type_store);
        }

        // add this unresolved type to context for type_id
        let type_id = pending_types.add_type(type_store);

        // convert IslConstraint to Constraint
        let mut found_type_constraint = false;
        for isl_constraint in isl_type.constraints() {
            if let IslConstraint::Type(_) = isl_constraint {
                found_type_constraint = true;
            }

            let constraint = Constraint::resolve_from_isl_constraint(
                isl_constraint,
                type_store,
                pending_types,
                isl_type.open_content(),
            )?;
            constraints.push(constraint);
        }

        // add `type: any` as a default type constraint if there is no type constraint found
        if !found_type_constraint {
            // set the isl type name for any error that is returned while parsing its constraints
            let isl_type_name = match type_name.to_owned() {
                Some(name) => name,
                None => "".to_owned(),
            };

            let isl_constraint = IslConstraint::from_ion_element(
                "type",
                &OwnedElement::new_symbol(text_token("any")),
                &isl_type_name,
                &mut vec![],
            )?;
            let constraint = Constraint::resolve_from_isl_constraint(
                &isl_constraint,
                type_store,
                pending_types,
                true, // by default Ion Schema allows open content
            )?;
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
    fn is_valid(&self, value: &OwnedElement, type_store: &TypeStore) -> bool {
        let violation = self.validate(value, type_store);
        violation.is_ok()
    }

    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let type_name = match self.name() {
            None => "",
            Some(name) => name,
        };
        for constraint in self.constraints() {
            if let Err(violation) = constraint.validate(value, type_store) {
                violations.push(violation);
            }
        }
        if violations.is_empty() {
            return Ok(());
        }
        Err(Violation::with_violations(
            type_name,
            ViolationCode::TypeConstraintsUnsatisfied,
            "value didn't satisfy type constraint(s)",
            violations,
        ))
    }
}

#[cfg(test)]
mod type_definition_tests {
    use super::*;
    use crate::constraint::Constraint;
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_range::Number;
    use crate::isl::isl_range::NumberRange;
    use crate::isl::isl_type::IslType;
    use crate::isl::isl_type_reference::IslTypeRef;
    use crate::system::PendingTypes;
    use ion_rs::Decimal;
    use ion_rs::Integer;
    use rstest::*;

    // TODO: Remove type ids for assertion to make tests more readable
    #[rstest(
    isl_type, type_def,
    case::type_constraint_with_anonymous_type(
        /* For a schema with single anonymous type as below:
            { type: int }
         */
        IslType::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]),
        TypeDefinition::anonymous([Constraint::type_constraint(0)])
    ),
    case::type_constraint_with_named_type(
        /* For a schema with named type as below:
            { name: my_int, type: int }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::named("int"))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(0)])
    ),
    case::type_constraint_with_self_reference_type(
        /* For a schema with self reference type as below:
            { name: my_int, type: my_int }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::named("my_int"))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(34)])
    ),
    case::type_constraint_with_nested_self_reference_type(
        /* For a schema with nested self reference type as below:
            { name: my_int, type: { type: my_int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(35)]) // 0-32 are built-in types which are preloaded to the type_store
    ),
    case::type_constraint_with_nested_type(
        /* For a schema with nested types as below:
            { name: my_int, type: { type: int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(35)])
    ),
    case::type_constraint_with_nested_multiple_types(
        /* For a schema with nested multiple types as below:
            { name: my_int, type: { type: int }, type: { type: my_int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))])), IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(35), Constraint::type_constraint(36)])
    ),
    case::all_of_constraint(
        /* For a schema with all_of type as below:
            { all_of: [{ type: int }] }
        */
        IslType::anonymous([IslConstraint::all_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))])])]),
        TypeDefinition::anonymous([Constraint::all_of([35]), Constraint::type_constraint(25)])
    ),
    case::any_of_constraint(
        /* For a schema with any_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        IslType::anonymous([IslConstraint::any_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("decimal"))])])]),
        TypeDefinition::anonymous([Constraint::any_of([35, 36]), Constraint::type_constraint(25)])
    ),
    case::one_of_constraint(
        /* For a schema with one_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        IslType::anonymous([IslConstraint::one_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("decimal"))])])]),
        TypeDefinition::anonymous([Constraint::one_of([35, 36]), Constraint::type_constraint(25)])
    ),
    case::not_constraint(
        /* For a schema with not constraint as below:
            { not: { type: int } }
        */
        IslType::anonymous([IslConstraint::not(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]))]),
        TypeDefinition::anonymous([Constraint::not(35), Constraint::type_constraint(25)])
    ),
    case::ordered_elements_constraint(
        /* For a schema with ordered_elements constraint as below:
            { ordered_elements: [ symbol, { type: int, occurs: optional }, ] }
        */
        IslType::anonymous([IslConstraint::ordered_elements([IslTypeRef::named("symbol"), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int")), IslConstraint::Occurs(Range::optional())])])]),
        TypeDefinition::anonymous([Constraint::ordered_elements([5, 35]), Constraint::type_constraint(25)])
    ),
    case::fields_constraint(
        /* For a schema with fields constraint as below:
            { fields: { name: string, id: int} }
        */
        IslType::anonymous([IslConstraint::fields(vec![("name".to_owned(), IslTypeRef::named("string")), ("id".to_owned(), IslTypeRef::named("int"))].into_iter())]),
        TypeDefinition::anonymous([Constraint::fields(vec![("name".to_owned(), 4), ("id".to_owned(), 0)].into_iter()), Constraint::type_constraint(25)])
    ),
    case::contains_constraint(
        /* For a schema with contains constraint as below:
            { contains: [true, 1, "hello"] }
        */
        IslType::anonymous([IslConstraint::contains([true.into(), 1.into(), "hello".to_owned().into()])]),
        TypeDefinition::anonymous([Constraint::contains([true.into(), 1.into(), "hello".to_owned().into()]), Constraint::type_constraint(25)])
        ),
    case::container_length_constraint(
        /* For a schema with container_length constraint as below:
            { container_length: 3 }
        */
        IslType::anonymous([IslConstraint::container_length(3.into())]),
        TypeDefinition::anonymous([Constraint::container_length(3.into()), Constraint::type_constraint(25)])
    ),
    case::byte_length_constraint(
        /* For a schema with byte_length constraint as below:
            { byte_length: 3 }
        */
        IslType::anonymous([IslConstraint::byte_length(3.into())]),
        TypeDefinition::anonymous([Constraint::byte_length(3.into()), Constraint::type_constraint(25)])
    ),
    case::codepoint_length_constraint(
        /* For a schema with codepoint_length constraint as below:
            { codepoint_length: 3 }
        */
        IslType::anonymous([IslConstraint::codepoint_length(3.into())]),
        TypeDefinition::anonymous([Constraint::codepoint_length(3.into()), Constraint::type_constraint(25)])
    ),
    case::element_constraint(
        /* For a schema with element constraint as below:
            { element: int }
        */
        IslType::anonymous([IslConstraint::element(IslTypeRef::named("int"))]),
        TypeDefinition::anonymous([Constraint::element(0), Constraint::type_constraint(25)])
    ),
    case::annotations_constraint(
        /* For a schema with annotations constraint as below:
            { annotations: closed::[red, blue, green] }
        */
        IslType::anonymous([IslConstraint::annotations(vec!["closed"], vec![text_token("red").into(), text_token("blue").into(), text_token("green").into()])]),
        TypeDefinition::anonymous([Constraint::annotations(vec!["closed"], vec![text_token("red").into(), text_token("blue").into(), text_token("green").into()]), Constraint::type_constraint(25)])
    ),
    case::precision_constraint(
        /* For a schema with precision constraint as below:
            { precision: 3 }
        */
        IslType::anonymous([IslConstraint::precision(3.into())]),
        TypeDefinition::anonymous([Constraint::precision(3.into()), Constraint::type_constraint(25)])
    ),
    case::scale_constraint(
        /* For a schema with scale constraint as below:
            { scale: 2 }
        */
        IslType::anonymous([IslConstraint::scale(Integer::I64(2).into())]),
        TypeDefinition::anonymous([Constraint::scale(Integer::I64(2).into()), Constraint::type_constraint(25)])
    ),
    case::timestamp_precision_constraint(
        /* For a schema with timestamp_precision constraint as below:
            { timestamp_precision: month }
        */
        IslType::anonymous([IslConstraint::timestamp_precision("month".try_into().unwrap())]),
        TypeDefinition::anonymous([Constraint::timestamp_precision("month".try_into().unwrap()), Constraint::type_constraint(25)])
    ),
    case::valid_values_constraint(
        /* For a schema with valid_values constraint as below:
            { valid_values: [2, 3.5, 5e7, "hello", hi] }        
        */
        IslType::anonymous([IslConstraint::valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), text_token("hi").into()]).unwrap()]),
        TypeDefinition::anonymous([Constraint::valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), text_token("hi").into()]).unwrap(), Constraint::type_constraint(25)])
    ),
    case::valid_values_with_range_constraint(
        /* For a schema with valid_values constraint as below:
            { valid_values: range::[1, 5.5] }        
        */
        IslType::anonymous(
            [IslConstraint::valid_values_with_range(
                NumberRange::new(
                    Number::from(&Integer::I64(1)),
                    Number::from(&Decimal::new(55, -1))
                ).unwrap().into())
            ]
        ),
        TypeDefinition::anonymous([
            Constraint::valid_values_with_range(
            NumberRange::new(
                Number::from(&Integer::I64(1)),
                Number::from(&Decimal::new(55, -1))
            ).unwrap().into()),
            Constraint::type_constraint(25)
        ])
    ),
    case::regex_constraint(
        /* For a schema with regex constraint as below:
            { regex: "[abc]" }        
        */
        IslType::anonymous(
            [IslConstraint::regex(
                false, // case insensitive
                false, // multiline
                "[abc]".to_string()
            )]
        ),
        TypeDefinition::anonymous([
            Constraint::regex(
                false, // case insensitive
                false, // multiline
                "[abc]".to_string()
            ).unwrap(),
            Constraint::type_constraint(25)
        ])
    )
    )]
    fn isl_type_to_type_definition(isl_type: IslType, type_def: TypeDefinition) {
        // assert if both the TypeDefinition are same in terms of constraints and name
        let type_store = &mut TypeStore::default();
        let pending_types = &mut PendingTypes::default();
        let this_type_def = match isl_type {
            IslType::Named(named_isl_type) => TypeDefinition::Named(
                TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                    &named_isl_type,
                    type_store,
                    pending_types,
                )
                .unwrap(),
            ),
            IslType::Anonymous(anonymous_isl_type) => TypeDefinition::Anonymous(
                TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
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
