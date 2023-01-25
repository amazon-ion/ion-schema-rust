use crate::constraint::Constraint;
use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_range::Range;
use crate::isl::isl_type::IslTypeImpl;
use crate::result::{IonSchemaResult, ValidationResult};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::violation::{Violation, ViolationCode};
use crate::IonSchemaElement;
use ion_rs::value::owned::{text_token, Element};
use ion_rs::value::{Builder, IonElement};
use ion_rs::IonType;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

/// Provides validation for [`TypeDefinition`]
pub trait TypeValidator {
    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(&self, value: &IonSchemaElement, type_store: &TypeStore) -> bool;

    /// Returns `Err(violation)` with details as to which constraints were violated,
    /// otherwise returns `Ok(())` indicating no violations were found during the validation
    fn validate(&self, value: &IonSchemaElement, type_store: &TypeStore) -> ValidationResult;
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

    pub fn id(&self) -> TypeId {
        self.id
    }

    /// Provides the validation for the given value based on this schema type
    /// ```
    /// use ion_rs::value::owned::Element;
    /// use ion_schema::IonSchemaElement;
    /// use ion_schema::authority::{FileSystemDocumentAuthority, DocumentAuthority};
    /// use ion_schema::system::SchemaSystem;
    /// use ion_schema::result::IonSchemaResult;
    /// use std::path::Path;
    ///
    /// fn main() -> IonSchemaResult<()> {
    ///     // create an IonSchemaElement from an Element
    ///     use ion_schema::authority::MapDocumentAuthority;
    ///     let owned_element: Element = 4.into();
    ///     let document: Vec<Element> = vec![4.into(), "hello".to_string().into(), true.into()];
    ///
    ///     let map_authority = [
    ///         (
    ///            "sample.isl",
    ///             r#"
    ///                 type::{
    ///                     name: my_int,
    ///                     type: int,
    ///                 }
    ///             "#
    ///         )   
    ///     ];
    ///
    ///     // create a vector of authorities and construct schema system
    ///     // this example uses above mentioned map as the authority
    ///     let authorities: Vec<Box<dyn DocumentAuthority>> = vec![Box::new(
    ///             MapDocumentAuthority::new(map_authority),
    ///         )];
    ///     let mut schema_system = SchemaSystem::new(authorities);
    ///
    ///     // use this schema_system to load a schema as following
    ///     let schema = schema_system.load_schema("sample.isl")?;
    ///
    ///     // unwrap() here because we know that the `my_int` type exists in sample.isl
    ///     let type_ref = schema.get_type("my_int").unwrap();
    ///
    ///     assert!(type_ref.validate(&owned_element).is_ok()); // 4 is valid for `my_int`
    ///     assert!(type_ref.validate(&document).is_err()); // document type is invalid for `my_int` type
    ///     Ok(())
    /// }
    /// ```
    pub fn validate<I: Into<IonSchemaElement>>(&self, value: I) -> ValidationResult {
        let type_def = self.type_store.get_type_by_id(self.id).unwrap();

        // convert given IonSchemaElement to an Element
        let schema_element: IonSchemaElement = value.into();

        type_def.validate(&schema_element, &self.type_store)
    }
}

/// Represents a [`BuiltInTypeDefinition`] which stores a resolved builtin ISl type using [`TypeStore`]
#[derive(Debug, Clone, PartialEq)]
pub enum BuiltInTypeDefinition {
    Atomic(IonType, Nullability),
    Derived(TypeDefinitionImpl),
}

/// Represents whether an atomic built-in type is nullable or not
#[derive(Debug, Clone, PartialEq, Eq)]
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
            None,
        ));
        Ok(builtin_type_def)
    }
}

impl TypeValidator for BuiltInTypeDefinition {
    fn is_valid(&self, value: &IonSchemaElement, type_store: &TypeStore) -> bool {
        let violation = self.validate(value, type_store);
        violation.is_ok()
    }

    fn validate(&self, value: &IonSchemaElement, type_store: &TypeStore) -> ValidationResult {
        match &self {
            BuiltInTypeDefinition::Atomic(ion_type, is_nullable) => {
                // atomic types doesn't include document type
                match value {
                    IonSchemaElement::SingleElement(element) => {
                        if *is_nullable == Nullability::NotNullable && element.is_null() {
                            return Err(Violation::new(
                                "type_constraint",
                                ViolationCode::InvalidNull,
                                &format!("expected type {ion_type:?} doesn't allow null"),
                            ));
                        }
                        if element.ion_type() != *ion_type {
                            return Err(Violation::new(
                                "type_constraint",
                                ViolationCode::TypeMismatched,
                                &format!(
                                    "expected type {:?}, found {:?}",
                                    ion_type,
                                    element.ion_type()
                                ),
                            ));
                        }

                        Ok(())
                    }
                    IonSchemaElement::Document(document) => Err(Violation::new(
                        "type_constraint",
                        ViolationCode::TypeMismatched,
                        &format!("expected type {ion_type:?}, found document"),
                    )),
                }
            }
            BuiltInTypeDefinition::Derived(other_type) => {
                if other_type.name() == &Some("document".to_owned()) {
                    // Verify whether the given derived type is document
                    // And check if it is using enum variant IonSchemaIonElement::Document
                    if Option::is_none(&value.as_document()) {
                        return Err(Violation::new(
                            "type_constraint",
                            ViolationCode::TypeMismatched,
                            &format!(
                                "expected type document found {:?}",
                                value.as_element().unwrap().ion_type()
                            ),
                        ));
                    }
                    return Ok(());
                }
                // if it is not a document type do validation using the type definition
                other_type.validate(value, type_store)
            }
        }
    }
}

impl Display for BuiltInTypeDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            BuiltInTypeDefinition::Atomic(ion_type, _) => write!(f, "{ion_type}"),
            BuiltInTypeDefinition::Derived(type_def) => write!(f, "{type_def}"),
        }
    }
}

/// Represents a [`TypeDefinition`] which stores a resolved ISL type using [`TypeStore`]
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Named(TypeDefinitionImpl),
    Anonymous(TypeDefinitionImpl),
    BuiltIn(BuiltInTypeDefinition),
}

impl TypeDefinition {
    /// Creates a named [`TypeDefinition`] using the [`IslConstraint`] defined within it
    pub fn named<A: Into<String>, B: Into<Vec<Constraint>>>(
        name: A,
        constraints: B,
    ) -> TypeDefinition {
        TypeDefinition::Named(TypeDefinitionImpl::new(
            Some(name.into()),
            constraints.into(),
            None,
        ))
    }

    /// Creates an anonymous [`TypeDefinition`] using the [`IslConstraint`] defined within it
    pub fn anonymous<A: Into<Vec<Constraint>>>(constraints: A) -> TypeDefinition {
        TypeDefinition::Anonymous(TypeDefinitionImpl::new(None, constraints.into(), None))
    }

    /// Provides the underlying constraints of [`TypeDefinitionImpl`]
    pub fn constraints(&self) -> &[Constraint] {
        match &self {
            TypeDefinition::Named(named_type) => named_type.constraints(),
            TypeDefinition::Anonymous(anonymous_type) => anonymous_type.constraints(),
            _ => &[],
        }
    }

    /// Returns an occurs constraint as range if it exists in the [`TypeDefinition`] otherwise returns `occurs: required`
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

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            TypeDefinition::Named(named_type_def) => {
                write!(f, "{named_type_def}")
            }
            TypeDefinition::Anonymous(anonymous_type_def) => {
                write!(f, "{anonymous_type_def}")
            }
            TypeDefinition::BuiltIn(builtin_type_def) => {
                write!(f, "{builtin_type_def}")
            }
        }
    }
}

impl TypeValidator for TypeDefinition {
    fn is_valid(&self, value: &IonSchemaElement, type_store: &TypeStore) -> bool {
        let violation = self.validate(value, type_store);
        violation.is_ok()
    }

    fn validate(&self, value: &IonSchemaElement, type_store: &TypeStore) -> ValidationResult {
        match self {
            TypeDefinition::Named(named_type) => named_type.validate(value, type_store),
            TypeDefinition::Anonymous(anonymous_type) => anonymous_type.validate(value, type_store),
            TypeDefinition::BuiltIn(built_in_type) => built_in_type.validate(value, type_store),
        }
    }
}

/// A [`TypeDefinitionImpl`] consists of an optional name and zero or more constraints.
#[derive(Debug, Clone)]
pub struct TypeDefinitionImpl {
    name: Option<String>,
    constraints: Vec<Constraint>,
    // `is_deferred_type_def` indicates if this is a deferred type def which will be resolved later
    // e.g.
    // ```
    // type:: {
    //  name: foo,
    //  type: bar,
    // }
    // type:: {
    //  name: bar,
    //  type: int
    // }
    // ```
    // For above example, `bar` will be saved as deferred type definition until we resolve the definition of `bar`
    is_deferred_type_def: bool,
    // Represents the ISL type struct in string format, this will be used for violation messages
    isl_type_struct: Option<Element>,
}

impl TypeDefinitionImpl {
    pub fn new(
        name: Option<String>,
        constraints: Vec<Constraint>,
        isl_type_struct: Option<Element>,
    ) -> Self {
        Self {
            name,
            constraints,
            is_deferred_type_def: false,
            isl_type_struct,
        }
    }

    pub fn new_deferred_type_def(name: String) -> Self {
        Self {
            name: Some(name),
            constraints: vec![],
            is_deferred_type_def: true,
            isl_type_struct: None,
        }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn with_name(self, alias: String) -> Self {
        Self {
            name: Some(alias),
            constraints: self.constraints,
            is_deferred_type_def: self.is_deferred_type_def,
            isl_type_struct: None,
        }
    }

    pub fn is_deferred_type_def(&self) -> bool {
        self.is_deferred_type_def
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    /// Parse constraints inside an [`IslTypeImpl`] to a schema type definition, update the [`PendingTypes`]
    /// and return its [`TypeId`] if the conversion was successful, otherwise return an [`IonSchemaError`]
    ///
    /// [`IonSchemaError`]: crate::result::IonSchemaError
    pub fn parse_from_isl_type_and_update_pending_types(
        isl_type: &IslTypeImpl,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<TypeId> {
        let mut constraints = vec![];

        // parses an isl_type to a TypeDefinition
        let type_name = isl_type.name();

        // add parent information for named type
        if type_name.is_some() {
            pending_types.add_parent(type_name.to_owned().unwrap(), type_store);
        }

        // add this unresolved type to context for type_id
        let type_id = pending_types.add_type(type_store, type_name.to_owned());

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

        let isl_struct = isl_type.isl_type_struct.as_ref();
        // add `type: any` as a default type constraint if there is no type constraint found
        if !found_type_constraint {
            // set the isl type name for any error that is returned while parsing its constraints
            let isl_type_name = match type_name.to_owned() {
                Some(name) => name,
                None => match isl_struct {
                    None => "".to_owned(),
                    Some(isl_type_struct) => format!("{isl_type_struct}"),
                },
            };

            let isl_constraint = IslConstraint::from_ion_element(
                "type",
                &Element::new_symbol(text_token("any")),
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

        let type_def = TypeDefinitionImpl::new(
            type_name.to_owned(),
            constraints,
            isl_type.isl_type_struct.to_owned(),
        );

        // actual type id is the type id with respect to type store length
        let actual_type_id;

        if type_name.is_some() {
            // update with this resolved type_def to context for type_id
            actual_type_id = pending_types.update_named_type(
                type_id,
                type_name.as_ref().unwrap(),
                type_def,
                type_store,
            );

            // clear parent information from type_store as the type is already added in the type_store now
            pending_types.clear_parent();
        } else {
            // update with this resolved type_def to context for type_id
            actual_type_id = pending_types.update_anonymous_type(type_id, type_def, type_store);
        }

        Ok(actual_type_id)
    }
}

impl PartialEq for TypeDefinitionImpl {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.constraints == other.constraints()
    }
}

impl TypeValidator for TypeDefinitionImpl {
    fn is_valid(&self, value: &IonSchemaElement, type_store: &TypeStore) -> bool {
        let violation = self.validate(value, type_store);
        violation.is_ok()
    }

    fn validate(&self, value: &IonSchemaElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let type_name = match self.name() {
            None => format!("{}", self.isl_type_struct.as_ref().unwrap()),
            Some(name) => name.to_owned(),
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

impl Display for TypeDefinitionImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let type_def_name = match &self.name {
            None => match &self.isl_type_struct {
                None => "".to_owned(),
                Some(type_name) => format!("{type_name}"),
            },
            Some(type_name) => type_name.to_owned(),
        };

        write!(f, "{type_def_name}")
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
    use std::collections::HashSet;

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
        TypeDefinition::named("my_int", [Constraint::type_constraint(35)])
    ),
    case::type_constraint_with_nested_self_reference_type(
        /* For a schema with nested self reference type as below:
            { name: my_int, type: { type: my_int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(36)]) // 0-35 are built-in types which are preloaded to the type_store
    ),
    case::type_constraint_with_nested_type(
        /* For a schema with nested types as below:
            { name: my_int, type: { type: int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(36)])
    ),
    case::type_constraint_with_nested_multiple_types(
        /* For a schema with nested multiple types as below:
            { name: my_int, type: { type: int }, type: { type: my_int } }
         */
        IslType::named("my_int", [IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))])), IslConstraint::type_constraint(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("my_int"))]))]),
        TypeDefinition::named("my_int", [Constraint::type_constraint(36), Constraint::type_constraint(37)])
    ),
    case::all_of_constraint(
        /* For a schema with all_of type as below:
            { all_of: [{ type: int }] }
        */
        IslType::anonymous([IslConstraint::all_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))])])]),
        TypeDefinition::anonymous([Constraint::all_of([36]), Constraint::type_constraint(34)])
    ),
    case::any_of_constraint(
        /* For a schema with any_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        IslType::anonymous([IslConstraint::any_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("decimal"))])])]),
        TypeDefinition::anonymous([Constraint::any_of([36, 37]), Constraint::type_constraint(34)])
    ),
    case::one_of_constraint(
        /* For a schema with one_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        IslType::anonymous([IslConstraint::one_of([IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("decimal"))])])]),
        TypeDefinition::anonymous([Constraint::one_of([36, 37]), Constraint::type_constraint(34)])
    ),
    case::not_constraint(
        /* For a schema with not constraint as below:
            { not: { type: int } }
        */
        IslType::anonymous([IslConstraint::not(IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]))]),
        TypeDefinition::anonymous([Constraint::not(36), Constraint::type_constraint(34)])
    ),
    case::ordered_elements_constraint(
        /* For a schema with ordered_elements constraint as below:
            { ordered_elements: [ symbol, { type: int, occurs: optional }, ] }
        */
        IslType::anonymous([IslConstraint::ordered_elements([IslTypeRef::named("symbol"), IslTypeRef::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int")), IslConstraint::Occurs(Range::optional())])])]),
        TypeDefinition::anonymous([Constraint::ordered_elements([5, 36]), Constraint::type_constraint(34)])
    ),
    case::fields_constraint(
        /* For a schema with fields constraint as below:
            { fields: { name: string, id: int} }
        */
        IslType::anonymous([IslConstraint::fields(vec![("name".to_owned(), IslTypeRef::named("string")), ("id".to_owned(), IslTypeRef::named("int"))].into_iter())]),
        TypeDefinition::anonymous([Constraint::fields(vec![("name".to_owned(), 4), ("id".to_owned(), 0)].into_iter()), Constraint::type_constraint(34)])
    ),
    case::contains_constraint(
        /* For a schema with contains constraint as below:
            { contains: [true, 1, "hello"] }
        */
        IslType::anonymous([IslConstraint::contains([true.into(), 1.into(), "hello".to_owned().into()])]),
        TypeDefinition::anonymous([Constraint::contains([true.into(), 1.into(), "hello".to_owned().into()]), Constraint::type_constraint(34)])
        ),
    case::container_length_constraint(
        /* For a schema with container_length constraint as below:
            { container_length: 3 }
        */
        IslType::anonymous([IslConstraint::container_length(3.into())]),
        TypeDefinition::anonymous([Constraint::container_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::byte_length_constraint(
        /* For a schema with byte_length constraint as below:
            { byte_length: 3 }
        */
        IslType::anonymous([IslConstraint::byte_length(3.into())]),
        TypeDefinition::anonymous([Constraint::byte_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::codepoint_length_constraint(
        /* For a schema with codepoint_length constraint as below:
            { codepoint_length: 3 }
        */
        IslType::anonymous([IslConstraint::codepoint_length(3.into())]),
        TypeDefinition::anonymous([Constraint::codepoint_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::element_constraint(
        /* For a schema with element constraint as below:
            { element: int }
        */
        IslType::anonymous([IslConstraint::element(IslTypeRef::named("int"))]),
        TypeDefinition::anonymous([Constraint::element(0), Constraint::type_constraint(34)])
    ),
    case::annotations_constraint(
        /* For a schema with annotations constraint as below:
            { annotations: closed::[red, blue, green] }
        */
        IslType::anonymous([IslConstraint::annotations(vec!["closed"], vec![text_token("red").into(), text_token("blue").into(), text_token("green").into()])]),
        TypeDefinition::anonymous([Constraint::annotations(vec!["closed"], vec![text_token("red").into(), text_token("blue").into(), text_token("green").into()]), Constraint::type_constraint(34)])
    ),
    case::precision_constraint(
        /* For a schema with precision constraint as below:
            { precision: 3 }
        */
        IslType::anonymous([IslConstraint::precision(3.into())]),
        TypeDefinition::anonymous([Constraint::precision(3.into()), Constraint::type_constraint(34)])
    ),
    case::scale_constraint(
        /* For a schema with scale constraint as below:
            { scale: 2 }
        */
        IslType::anonymous([IslConstraint::scale(Integer::I64(2).into())]),
        TypeDefinition::anonymous([Constraint::scale(Integer::I64(2).into()), Constraint::type_constraint(34)])
    ),
    case::timestamp_precision_constraint(
        /* For a schema with timestamp_precision constraint as below:
            { timestamp_precision: month }
        */
        IslType::anonymous([IslConstraint::timestamp_precision("month".try_into().unwrap())]),
        TypeDefinition::anonymous([Constraint::timestamp_precision("month".try_into().unwrap()), Constraint::type_constraint(34)])
    ),
    case::valid_values_constraint(
        /* For a schema with valid_values constraint as below:
            { valid_values: [2, 3.5, 5e7, "hello", hi] }
        */
        IslType::anonymous([IslConstraint::valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), text_token("hi").into()]).unwrap()]),
        TypeDefinition::anonymous([Constraint::valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), text_token("hi").into()]).unwrap(), Constraint::type_constraint(34)])
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
            Constraint::type_constraint(34)
        ])
    ),
    case::utf8_byte_length_constraint(
        /* For a schema with utf8_byte_length constraint as below:
            { utf8_byte_length: 3 }
        */
        IslType::anonymous([IslConstraint::utf8_byte_length(3.into())]),
        TypeDefinition::anonymous([Constraint::utf8_byte_length(3.into()), Constraint::type_constraint(34)])
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
            Constraint::type_constraint(34)
        ])
    ),
    case::timestamp_offset_constraint(
        /* For a schema with timestamp_offset constraint as below:
            { timestamp_offset: ["-00:00"] }
        */
        IslType::anonymous(
            [IslConstraint::timestamp_offset(vec!["-00:00".try_into().unwrap()])]
        ),
        TypeDefinition::anonymous([Constraint::timestamp_offset(vec!["-00:00".try_into().unwrap()]),
            Constraint::type_constraint(34)
        ])
    )
    )]
    fn isl_type_to_type_definition(isl_type: IslType, type_def: TypeDefinition) {
        // assert if both the TypeDefinition are same in terms of constraints and name
        let type_store = &mut TypeStore::default();
        let pending_types = &mut PendingTypes::default();
        let this_type_def = match isl_type {
            IslType::Named(named_isl_type) => {
                let type_id = TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                    &named_isl_type,
                    type_store,
                    pending_types,
                )
                .unwrap();
                pending_types
                    .update_type_store(type_store, None, &HashSet::new())
                    .unwrap();
                type_store.get_type_by_id(type_id).unwrap()
            }
            IslType::Anonymous(anonymous_isl_type) => {
                let type_id = TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                    &anonymous_isl_type,
                    type_store,
                    pending_types,
                )
                .unwrap();
                pending_types
                    .update_type_store(type_store, None, &HashSet::new())
                    .unwrap();
                type_store.get_type_by_id(type_id).unwrap()
            }
        };
        assert_eq!(this_type_def, &type_def);
    }
}
