use crate::constraint::Constraint;
use crate::ion_path::IonPath;
use crate::isl::isl_constraint::IslConstraintImpl;
use crate::isl::isl_type::IslTypeImpl;
use crate::isl::IslVersion;
use crate::result::{IonSchemaResult, ValidationResult};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::violation::{Violation, ViolationCode};
use crate::IonSchemaElement;
use ion_rs::element::Element;
use ion_rs::IonType;
use ion_rs::Symbol;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

/// Provides validation for type definition
pub(crate) trait TypeValidator {
    /// If the specified value violates one or more of this type's constraints,
    /// returns `false`, otherwise `true`
    fn is_valid(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> bool;

    /// Returns `Err(violation)` with details as to which constraints were violated,
    /// otherwise returns `Ok(())` indicating no violations were found during the validation
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult;
}

// Provides a public facing schema type which has a reference to TypeStore
// to get the underlying TypeDefinitionKind from TypeStore
/// Represents a top level ISL type definition
#[derive(Debug, Clone)]
pub struct TypeDefinition {
    id: TypeId,
    type_store: Arc<TypeStore>,
}

impl TypeDefinition {
    pub(crate) fn new(id: TypeId, type_store: Arc<TypeStore>) -> Self {
        Self { id, type_store }
    }

    pub fn id(&self) -> TypeId {
        self.id
    }

    /// Provides the validation for the given value based on this schema type
    /// ```
    /// use ion_rs::element::Element;
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

        type_def.validate(&schema_element, &self.type_store, &mut IonPath::default())
    }
}

/// Represents a [`BuiltInTypeDefinition`] which stores a resolved builtin ISl type using [`TypeStore`]
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BuiltInTypeDefinition {
    Atomic(IonType, Nullability),
    Derived(TypeDefinitionImpl),
}

/// Represents whether an atomic built-in type is nullable or not
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Nullability {
    Nullable,
    NotNullable,
}

impl BuiltInTypeDefinition {
    pub(crate) fn parse_from_isl_type(
        isl_version: IslVersion,
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
                isl_version,
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
    fn is_valid(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> bool {
        let violation = self.validate(value, type_store, ion_path);
        violation.is_ok()
    }

    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        match &self {
            BuiltInTypeDefinition::Atomic(ion_type, is_nullable) => {
                // atomic types doesn't include document type
                match value {
                    IonSchemaElement::SingleElement(element) => {
                        if *is_nullable == Nullability::NotNullable && element.is_null() {
                            return Err(Violation::new(
                                "type_constraint",
                                ViolationCode::InvalidNull,
                                format!("expected type {ion_type:?} doesn't allow null"),
                                ion_path,
                            ));
                        }
                        if element.ion_type() != *ion_type {
                            return Err(Violation::new(
                                "type_constraint",
                                ViolationCode::TypeMismatched,
                                format!(
                                    "expected type {:?}, found {:?}",
                                    ion_type,
                                    element.ion_type()
                                ),
                                ion_path,
                            ));
                        }

                        Ok(())
                    }
                    IonSchemaElement::Document(document) => Err(Violation::new(
                        "type_constraint",
                        ViolationCode::TypeMismatched,
                        format!("expected type {ion_type:?}, found document"),
                        ion_path,
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
                            format!(
                                "expected type document found {:?}",
                                value.as_element().unwrap().ion_type()
                            ),
                            ion_path,
                        ));
                    }
                    return Ok(());
                }
                // if it is not a document type do validation using the type definition
                other_type.validate(value, type_store, ion_path)
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

/// Represents a [`TypeDefinitionKind`] which stores a resolved ISL type using [`TypeStore`]
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeDefinitionKind {
    Named(TypeDefinitionImpl),
    Anonymous(TypeDefinitionImpl),
    BuiltIn(BuiltInTypeDefinition),
}

impl TypeDefinitionKind {
    /// Creates a named [`TypeDefinitionKind`] using the [`Constraint`]s defined within it
    pub fn named<A: Into<String>, B: Into<Vec<Constraint>>>(
        name: A,
        constraints: B,
    ) -> TypeDefinitionKind {
        TypeDefinitionKind::Named(TypeDefinitionImpl::new(
            Some(name.into()),
            constraints.into(),
            None,
        ))
    }

    /// Creates an anonymous [`TypeDefinitionKind`] using the [`Constraint`]s defined within it
    pub fn anonymous<A: Into<Vec<Constraint>>>(constraints: A) -> TypeDefinitionKind {
        TypeDefinitionKind::Anonymous(TypeDefinitionImpl::new(None, constraints.into(), None))
    }

    /// Provides the underlying constraints of [`TypeDefinitionKind`]
    pub fn constraints(&self) -> &[Constraint] {
        match &self {
            TypeDefinitionKind::Named(named_type) => named_type.constraints(),
            TypeDefinitionKind::Anonymous(anonymous_type) => anonymous_type.constraints(),
            _ => &[],
        }
    }

    /// Provides the validation result for base built in type of the type definition with the given value
    // This method is only used by nullable type reference for validation, it searches for a base type name which is built in type and nullable.
    // It returns the result of validation for that nullable base type.
    pub fn is_valid_for_base_nullable_type(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> bool {
        // get a nullable built in base type name which can be used to perform validation to check for correct `null.*` type
        let built_in_type_name = match self {
            TypeDefinitionKind::Named(_) | TypeDefinitionKind::Anonymous(_) => {
                // only built in type names are needed for base type
                None
            }
            TypeDefinitionKind::BuiltIn(built_int_type) => match built_int_type {
                BuiltInTypeDefinition::Atomic(ion_type, nullability) => {
                    Some(format!("${ion_type}"))
                }
                BuiltInTypeDefinition::Derived(type_def) => {
                    let type_name = type_def.name().as_ref().unwrap().to_string();
                    if !type_name.starts_with('$') {
                        Some(format!("${type_name}"))
                    } else {
                        Some(type_name)
                    }
                }
            },
        }
        .unwrap(); // safe unwrap() because nullable type references are only used for built in types

        // unwrap() here are safe because it has been verified while constructing the schema that built type definition exist in the type store
        let type_def = type_store
            .get_type_by_id(
                type_store
                    .get_builtin_type_id(built_in_type_name.as_str())
                    .unwrap(),
            )
            .unwrap();

        type_def.is_valid(value, type_store, ion_path)
    }
}

impl Display for TypeDefinitionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            TypeDefinitionKind::Named(named_type_def) => {
                write!(f, "{named_type_def}")
            }
            TypeDefinitionKind::Anonymous(anonymous_type_def) => {
                write!(f, "{anonymous_type_def}")
            }
            TypeDefinitionKind::BuiltIn(builtin_type_def) => {
                write!(f, "{builtin_type_def}")
            }
        }
    }
}

impl TypeValidator for TypeDefinitionKind {
    fn is_valid(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> bool {
        let violation = self.validate(value, type_store, ion_path);
        violation.is_ok()
    }

    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        match self {
            TypeDefinitionKind::Named(named_type) => {
                named_type.validate(value, type_store, ion_path)
            }
            TypeDefinitionKind::Anonymous(anonymous_type) => {
                anonymous_type.validate(value, type_store, ion_path)
            }
            TypeDefinitionKind::BuiltIn(built_in_type) => {
                built_in_type.validate(value, type_store, ion_path)
            }
        }
    }
}

/// A [`TypeDefinitionImpl`] consists of an optional name and zero or more constraints.
#[derive(Debug, Clone)]
pub(crate) struct TypeDefinitionImpl {
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
    pub(crate) fn parse_from_isl_type_and_update_pending_types(
        isl_version: IslVersion,
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
            if let IslConstraintImpl::Type(_) = isl_constraint {
                found_type_constraint = true;
            }

            let constraint = Constraint::resolve_from_isl_constraint(
                isl_version,
                isl_constraint,
                type_store,
                pending_types,
                isl_type.is_open_content_allowed(),
            )?;
            constraints.push(constraint);
        }

        let isl_struct = isl_type.isl_type_struct.as_ref();
        // add `type: any` as a default type constraint if there is no type constraint found
        if !found_type_constraint && isl_version == IslVersion::V1_0 {
            // set the isl type name for any error that is returned while parsing its constraints
            let isl_type_name = match type_name.to_owned() {
                Some(name) => name,
                None => match isl_struct {
                    None => "".to_owned(),
                    Some(isl_type_struct) => format!("{isl_type_struct}"),
                },
            };

            let isl_constraint: IslConstraintImpl =
                    // default type for ISL 1.0 is `any`
                    IslConstraintImpl::from_ion_element(
                        isl_version,
                        "type",
                        &Element::symbol(Symbol::from("any")),
                        &isl_type_name,
                        &mut vec![],
                    )?;

            let constraint = Constraint::resolve_from_isl_constraint(
                isl_version,
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
    fn is_valid(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> bool {
        let violation = self.validate(value, type_store, ion_path);
        violation.is_ok()
    }

    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let type_name = match self.name() {
            None => match self.isl_type_struct.as_ref() {
                None => "".to_owned(),
                Some(anonymous_struct) => {
                    format!("{anonymous_struct}")
                }
            },
            Some(name) => name.to_owned(),
        };
        for constraint in self.constraints() {
            if let Err(violation) = constraint.validate(value, type_store, ion_path) {
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
            ion_path,
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
    use crate::isl::isl_constraint::v_1_0::*;
    use crate::isl::isl_range::Number;
    use crate::isl::isl_range::NumberRange;
    use crate::isl::isl_type::v_1_0::*;
    use crate::isl::isl_type::IslType;
    use crate::isl::isl_type_reference::v_1_0::*;
    use crate::isl::util::Ieee754InterchangeFormat;
    use crate::isl::*;
    use crate::system::PendingTypes;
    use ion_rs::Decimal;
    use ion_rs::Int;
    use rstest::*;
    use std::collections::HashSet;

    // TODO: Remove type ids for assertion to make tests more readable
    #[rstest(
    isl_type, type_def,
    case::type_constraint_with_anonymous_type(
        /* For a schema with single anonymous type as below:
            { type: int }
         */
        anonymous_type([type_constraint(named_type_ref("int"))]),
    TypeDefinitionKind::anonymous([Constraint::type_constraint(0)])
    ),
    case::type_constraint_with_named_type(
        /* For a schema with named type as below:
            { name: my_int, type: int }
         */
        named_type("my_int", [type_constraint(named_type_ref("int"))]),
    TypeDefinitionKind::named("my_int", [Constraint::type_constraint(0)])
    ),
    case::type_constraint_with_self_reference_type(
        /* For a schema with self reference type as below:
            { name: my_int, type: my_int }
         */
        named_type("my_int", [type_constraint(named_type_ref("my_int"))]),
    TypeDefinitionKind::named("my_int", [Constraint::type_constraint(35)])
    ),
    case::type_constraint_with_nested_self_reference_type(
        /* For a schema with nested self reference type as below:
            { name: my_int, type: { type: my_int } }
         */
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("my_int"))]))]),
    TypeDefinitionKind::named("my_int", [Constraint::type_constraint(36)]) // 0-35 are built-in types which are preloaded to the type_store
    ),
    case::type_constraint_with_nested_type(
        /* For a schema with nested types as below:
            { name: my_int, type: { type: int } }
         */
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("int"))]))]),
    TypeDefinitionKind::named("my_int", [Constraint::type_constraint(36)])
    ),
    case::type_constraint_with_nested_multiple_types(
        /* For a schema with nested multiple types as below:
            { name: my_int, type: { type: int }, type: { type: my_int } }
         */
        named_type("my_int", [type_constraint(anonymous_type_ref([type_constraint(named_type_ref("int"))])), type_constraint(anonymous_type_ref([type_constraint(named_type_ref("my_int"))]))]),
    TypeDefinitionKind::named("my_int", [Constraint::type_constraint(36), Constraint::type_constraint(37)])
    ),
    case::all_of_constraint(
        /* For a schema with all_of type as below:
            { all_of: [{ type: int }] }
        */
        anonymous_type([all_of([anonymous_type_ref([type_constraint(named_type_ref("int"))])])]),
    TypeDefinitionKind::anonymous([Constraint::all_of([36]), Constraint::type_constraint(34)])
    ),
    case::any_of_constraint(
        /* For a schema with any_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        anonymous_type([any_of([anonymous_type_ref([type_constraint(named_type_ref("int"))]), anonymous_type_ref([type_constraint(named_type_ref("decimal"))])])]),
    TypeDefinitionKind::anonymous([Constraint::any_of([36, 37]), Constraint::type_constraint(34)])
    ),
    case::one_of_constraint(
        /* For a schema with one_of constraint as below:
            { any_of: [{ type: int }, { type: decimal }] }
        */
        anonymous_type([one_of([anonymous_type_ref([type_constraint(named_type_ref("int"))]), anonymous_type_ref([type_constraint(named_type_ref("decimal"))])])]),
    TypeDefinitionKind::anonymous([Constraint::one_of([36, 37]), Constraint::type_constraint(34)])
    ),
    case::not_constraint(
        /* For a schema with not constraint as below:
            { not: { type: int } }
        */
        anonymous_type([not(anonymous_type_ref([type_constraint(named_type_ref("int"))]))]),
    TypeDefinitionKind::anonymous([Constraint::not(36), Constraint::type_constraint(34)])
    ),
    case::ordered_elements_constraint(
        /* For a schema with ordered_elements constraint as below:
            { ordered_elements: [ symbol, { type: int }, ] }
        */
        anonymous_type([ordered_elements([named_type_ref("symbol"), anonymous_type_ref([type_constraint(named_type_ref("int"))])])]),
    TypeDefinitionKind::anonymous([Constraint::ordered_elements([5, 36]), Constraint::type_constraint(34)])
    ),
    case::fields_constraint(
        /* For a schema with fields constraint as below:
            { fields: { name: string, id: int} }
        */
        anonymous_type([fields(vec![("name".to_owned(), named_type_ref("string")), ("id".to_owned(), named_type_ref("int"))].into_iter())]),
    TypeDefinitionKind::anonymous([Constraint::fields(vec![("name".to_owned(), 4), ("id".to_owned(), 0)].into_iter()), Constraint::type_constraint(34)])
    ),
    case::field_names_constraint(
        /* For a schema with field_names constraint as below:
            { field_names: distinct::symbol }
        */
    isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::field_names(named_type_ref("symbol"), true)]),
    TypeDefinitionKind::anonymous([Constraint::field_names(5, true), Constraint::type_constraint(34)])
    ),
    case::contains_constraint(
        /* For a schema with contains constraint as below:
            { contains: [true, 1, "hello"] }
        */
        anonymous_type([contains([true.into(), 1.into(), "hello".to_owned().into()])]),
    TypeDefinitionKind::anonymous([Constraint::contains([true.into(), 1.into(), "hello".to_owned().into()]), Constraint::type_constraint(34)])
        ),
    case::container_length_constraint(
        /* For a schema with container_length constraint as below:
            { container_length: 3 }
        */
        anonymous_type([container_length(3.into())]),
    TypeDefinitionKind::anonymous([Constraint::container_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::byte_length_constraint(
        /* For a schema with byte_length constraint as below:
            { byte_length: 3 }
        */
        anonymous_type([byte_length(3.into())]),
    TypeDefinitionKind::anonymous([Constraint::byte_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::codepoint_length_constraint(
        /* For a schema with codepoint_length constraint as below:
            { codepoint_length: 3 }
        */
        anonymous_type([codepoint_length(3.into())]),
    TypeDefinitionKind::anonymous([Constraint::codepoint_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::element_constraint(
        /* For a schema with element constraint as below:
            { element: int }
        */
        anonymous_type([element(named_type_ref("int"))]),
    TypeDefinitionKind::anonymous([Constraint::element(0, false), Constraint::type_constraint(34)])
    ),
    case::distinct_element_constraint(
        /* For a schema with distinct element constraint as below:
            { element: distinct::int }
        */
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::element(named_type_ref("int"), true)]),
    TypeDefinitionKind::anonymous([Constraint::element(0, true), Constraint::type_constraint(34)])
    ),
    case::annotations_constraint(
        /* For a schema with annotations constraint as below:
            { annotations: closed::[red, blue, green] }
        */
        anonymous_type([annotations(vec!["closed"], vec![Symbol::from("red").into(), Symbol::from("blue").into(), Symbol::from("green").into()])]),
    TypeDefinitionKind::anonymous([Constraint::annotations(vec!["closed"], vec![Symbol::from("red").into(), Symbol::from("blue").into(), Symbol::from("green").into()]), Constraint::type_constraint(34)])
    ),
    case::annotations_v2_0_constraint(
        /* For a schema with annotations constraint as below:
            { annotations: { container_length: 1 } }
        */
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::standard_annotations(isl_type_reference::v_2_0::anonymous_type_ref([isl_constraint::v_2_0::container_length(1.into())]))]),
        TypeDefinitionKind::anonymous([Constraint::annotations_v2_0(36), Constraint::type_constraint(34)])
    ),
    case::precision_constraint(
        /* For a schema with precision constraint as below:
            { precision: 3 }
        */
        anonymous_type([precision(3.into())]),
    TypeDefinitionKind::anonymous([Constraint::precision(3.into()), Constraint::type_constraint(34)])
    ),
    case::scale_constraint(
        /* For a schema with scale constraint as below:
            { scale: 2 }
        */
        anonymous_type([scale(Int::I64(2).into())]),
    TypeDefinitionKind::anonymous([Constraint::scale(Int::I64(2).into()), Constraint::type_constraint(34)])
    ),
    case::exponent_constraint(
        /* For a schema with exponent constraint as below:
            { exponent: 2 }
        */
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::exponent(Int::I64(2).into())]),
    TypeDefinitionKind::anonymous([Constraint::exponent(Int::I64(2).into()), Constraint::type_constraint(34)])
    ),
    case::timestamp_precision_constraint(
        /* For a schema with timestamp_precision constraint as below:
            { timestamp_precision: month }
        */
        anonymous_type([timestamp_precision("month".try_into().unwrap())]),
    TypeDefinitionKind::anonymous([Constraint::timestamp_precision("month".try_into().unwrap()), Constraint::type_constraint(34)])
    ),
    case::valid_values_constraint(
        /* For a schema with valid_values constraint as below:
            { valid_values: [2, 3.5, 5e7, "hello", hi] }
        */
        anonymous_type([valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), Symbol::from("hi").into()]).unwrap()]),
    TypeDefinitionKind::anonymous([Constraint::valid_values_with_values(vec![2.into(), Decimal::new(35, -1).into(), 5e7.into(), "hello".to_owned().into(), Symbol::from("hi").into()]).unwrap(), Constraint::type_constraint(34)])
    ),
    case::valid_values_with_range_constraint(
        /* For a schema with valid_values constraint as below:
            { valid_values: range::[1, 5.5] }
        */
        anonymous_type(
            [valid_values_with_range(
                NumberRange::new(
                    Number::from(&Int::I64(1)),
                    Number::from(&Decimal::new(55, -1))
                ).unwrap().into())
            ]
        ),
    TypeDefinitionKind::anonymous([
            Constraint::valid_values_with_range(
            NumberRange::new(
                Number::from(&Int::I64(1)),
                Number::from(&Decimal::new(55, -1))
            ).unwrap().into()),
            Constraint::type_constraint(34)
        ])
    ),
    case::utf8_byte_length_constraint(
        /* For a schema with utf8_byte_length constraint as below:
            { utf8_byte_length: 3 }
        */
        anonymous_type([utf8_byte_length(3.into())]),
    TypeDefinitionKind::anonymous([Constraint::utf8_byte_length(3.into()), Constraint::type_constraint(34)])
    ),
    case::regex_constraint(
        /* For a schema with regex constraint as below:
            { regex: "[abc]" }
        */
        anonymous_type(
            [regex(
                false, // case insensitive
                false, // multiline
                "[abc]".to_string()
            )]
        ),
    TypeDefinitionKind::anonymous([
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
        anonymous_type(
            [timestamp_offset(vec!["-00:00".try_into().unwrap()])]
        ),
    TypeDefinitionKind::anonymous([Constraint::timestamp_offset(vec!["-00:00".try_into().unwrap()]),
            Constraint::type_constraint(34)
        ])
    ),
    case::ieee754_float_constraint(
        /* For a schema with ieee754_float constraint as below:
            { ieee754_float: binary16 }
        */
        isl_type::v_2_0::anonymous_type([isl_constraint::v_2_0::ieee754_float(Ieee754InterchangeFormat::Binary16)]),
        TypeDefinitionKind::anonymous([Constraint::ieee754_float(Ieee754InterchangeFormat::Binary16), Constraint::type_constraint(34)])
    ),
    )]
    fn isl_type_to_type_definition(isl_type: IslType, type_def: TypeDefinitionKind) {
        // assert if both the TypeDefinitionKind are same in terms of constraints and name
        let type_store = &mut TypeStore::default();
        let pending_types = &mut PendingTypes::default();
        let this_type_def = {
            let type_id = TypeDefinitionImpl::parse_from_isl_type_and_update_pending_types(
                IslVersion::V1_0,
                &isl_type.type_definition,
                type_store,
                pending_types,
            )
            .unwrap();
            pending_types
                .update_type_store(type_store, None, &HashSet::new())
                .unwrap();
            type_store.get_type_by_id(type_id).unwrap()
        };
        assert_eq!(this_type_def, &type_def);
    }
}
