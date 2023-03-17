use crate::isl::isl_constraint::{IslConstraint, IslConstraintImpl};
use crate::isl::isl_import::IslImportType;
use crate::isl::IonSchemaLanguageVersion;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::{text_token, Element};
use ion_rs::value::{IonElement, IonStruct};

/// Provides public facing APIs for constructing ISL types programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::{IslConstraint, IslConstraintImpl};
    use crate::isl::isl_type::{IslType, IslTypeImpl, IslTypeKind};

    /// Creates a [IslType::Named] using the [IslConstraint] defined within it
    pub fn named_type<A: Into<String>, B: Into<Vec<IslConstraint>>>(
        name: A,
        constraints: B,
    ) -> IslType {
        let constraints = constraints.into();
        let isl_constraints: Vec<IslConstraintImpl> = constraints
            .iter()
            .map(|c| c.constraint.to_owned())
            .collect();
        IslType::new(
            IslTypeKind::Named(IslTypeImpl::new(Some(name.into()), isl_constraints, None)),
            constraints,
        )
    }

    /// Creates a [IslType::Anonymous] using the [IslConstraint] defined within it
    pub fn anonymous_type<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslType {
        let constraints = constraints.into();
        let isl_constraints: Vec<IslConstraintImpl> = constraints
            .iter()
            .map(|c| c.constraint.to_owned())
            .collect();
        IslType::new(
            IslTypeKind::Anonymous(IslTypeImpl::new(None, isl_constraints, None)),
            constraints,
        )
    }
}

/// Provides public facing APIs for constructing ISL types programmatically for ISL 2.0
pub mod v_2_0 {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type::{v_1_0, IslType};

    /// Creates a [IslType::Named] using the [IslConstraint] defined within it
    pub fn named_type<A: Into<String>, B: Into<Vec<IslConstraint>>>(
        name: A,
        constraints: B,
    ) -> IslType {
        v_1_0::named_type(name, constraints)
    }

    /// Creates a [IslType::Anonymous] using the [IslConstraint] defined within it
    pub fn anonymous_type<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslType {
        v_1_0::anonymous_type(constraints)
    }
}

/// Represents a type in an ISL schema.
#[derive(Debug, Clone, PartialEq)]
pub struct IslType {
    pub(crate) kind: IslTypeKind,
    constraints: Vec<IslConstraint>,
}

impl IslType {
    pub(crate) fn new(kind: IslTypeKind, constraints: Vec<IslConstraint>) -> Self {
        Self { kind, constraints }
    }

    /// Provides a name if the ISL type is named type definition
    /// Otherwise returns None
    pub fn name(&self) -> &Option<String> {
        self.kind.name()
    }

    /// Provides open content that is there in the type definition
    pub fn open_content(&self) -> Vec<(String, Element)> {
        self.kind.open_content()
    }

    /// Provides the underlying constraints of [IslType]
    pub fn constraints(&self) -> &[IslConstraint] {
        &self.constraints
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IslTypeKind {
    Named(IslTypeImpl),
    Anonymous(IslTypeImpl),
}

impl IslTypeKind {
    /// Provides a name if the ISL type is named type definition
    /// Otherwise returns None
    pub fn name(&self) -> &Option<String> {
        match &self {
            IslTypeKind::Named(named_isl_type) => named_isl_type.name(),
            IslTypeKind::Anonymous(_) => &None,
        }
    }

    /// Provides the underlying constraints of [IslTypeImpl]
    pub fn constraints(&self) -> &[IslConstraintImpl] {
        match &self {
            IslTypeKind::Named(named_type) => named_type.constraints(),
            IslTypeKind::Anonymous(anonymous_type) => anonymous_type.constraints(),
        }
    }

    /// Verifies if the [IslType] allows open content or not
    pub fn is_open_content_allowed(&self) -> bool {
        match &self {
            IslTypeKind::Named(named_type) => named_type.is_open_content_allowed(),
            IslTypeKind::Anonymous(anonymous_type) => anonymous_type.is_open_content_allowed(),
        }
    }

    /// Provides open content that is there in the type definition
    pub fn open_content(&self) -> Vec<(String, Element)> {
        match &self {
            IslTypeKind::Named(named_type) => named_type.open_content(),
            IslTypeKind::Anonymous(anonymous_type) => anonymous_type.open_content(),
        }
    }
}

/// Represents both named and anonymous [IslType]s and can be converted to a resolved type definition
/// Named ISL type grammar: `type:: { name: <NAME>, <CONSTRAINT>...}`
/// Anonymous ISL type grammar: `{ <CONSTRAINT>... }`
#[derive(Debug, Clone)]
pub(crate) struct IslTypeImpl {
    name: Option<String>,
    constraints: Vec<IslConstraintImpl>,
    // Represents the ISL type struct in string format for anonymous type definition
    // For named type definition & programmatically created type definition, this will be `None`
    pub(crate) isl_type_struct: Option<Element>,
}

impl IslTypeImpl {
    pub fn new(
        name: Option<String>,
        constraints: Vec<IslConstraintImpl>,
        isl_type_struct: Option<Element>,
    ) -> Self {
        Self {
            name,
            constraints,
            isl_type_struct,
        }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn constraints(&self) -> &[IslConstraintImpl] {
        &self.constraints
    }

    pub fn open_content(&self) -> Vec<(String, Element)> {
        let mut open_content = vec![];
        for constraint in &self.constraints {
            if let IslConstraintImpl::Unknown(constraint_name, element) = constraint {
                open_content.push((constraint_name.to_owned(), element.to_owned()))
            }
        }
        open_content
    }

    pub(crate) fn is_open_content_allowed(&self) -> bool {
        let mut open_content = true;
        if self.constraints.contains(&IslConstraintImpl::ContentClosed) {
            open_content = false;
        }
        open_content
    }

    /// Parse constraints inside an [Element] to an [IslTypeImpl]
    pub fn from_owned_element(
        isl_version: IonSchemaLanguageVersion,
        ion: &Element,
        inline_imported_types: &mut Vec<IslImportType>, // stores the inline_imports that are discovered while loading this ISL type
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];
        let contains_annotations = ion.annotations().any(|x| x == &text_token("type"));

        let ion_struct = try_to!(ion.as_struct());

        // parses the name of the type specified by schema
        if ion_struct.get_all("name").count() > 1 {
            return Err(invalid_schema_error_raw(
                "type definition must only contain a single field that represents name of the type",
            ));
        }
        let type_name: Option<String> = match ion_struct.get("name") {
            Some(name_element) => match name_element.as_sym() {
                Some(name_symbol) => match name_symbol.text() {
                    None => {
                        return Err(invalid_schema_error_raw(
                            "type names must be a symbol with defined text",
                        ))
                    }
                    Some(name) => Some(name.to_owned()),
                },
                None => {
                    return Err(invalid_schema_error_raw(
                        "type names must be a symbol with defined text",
                    ))
                }
            },
            None => None, // If there is no name field then it is an anonymous type
        };

        if !contains_annotations && type_name.is_some() {
            // For named types if it does not have the `type::` annotation return an error
            return Err(invalid_schema_error_raw(
                "Top level types must have `type::` annotation in their definition",
            ));
        }

        // set the isl type name for any error that is returned while parsing its constraints
        let isl_type_name = match type_name.to_owned() {
            Some(name) => name,
            None => format!("{ion_struct:?}"),
        };

        // parses all the constraints inside a Type
        for (field_name, value) in ion_struct.iter() {
            let constraint_name = match field_name.text() {
                Some("name") => continue, // if the field_name is "name" then it's the type name not a constraint
                Some(name) => name,
                None => {
                    return Err(invalid_schema_error_raw(
                        "A type name symbol token does not have any text",
                    ))
                }
            };

            let constraint = IslConstraintImpl::from_ion_element(
                isl_version.to_owned(),
                constraint_name,
                value,
                &isl_type_name,
                inline_imported_types,
            )?;
            constraints.push(constraint);
        }
        Ok(IslTypeImpl::new(
            type_name,
            constraints,
            Some(ion.to_owned()),
        ))
    }
}

// OwnedStruct doesn't preserve field order hence the PartialEq won't work properly for unordered constraints
// Related issue: https://github.com/amazon-ion/ion-rust/issues/200
impl PartialEq for IslTypeImpl {
    fn eq(&self, other: &Self) -> bool {
        self.constraints.len() == other.constraints.len()
            && self.name == other.name
            && self.constraints.iter().all(|constraint| {
                other
                    .constraints
                    .iter()
                    .any(|other_constraint| constraint == other_constraint)
            })
    }
}
