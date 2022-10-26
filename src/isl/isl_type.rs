use crate::isl::isl_constraint::IslConstraint;
use crate::isl::isl_import::IslImportType;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::{text_token, Element};
use ion_rs::value::{IonElement, IonStruct};

/// Represents a type in an ISL schema.
#[derive(Debug, Clone, PartialEq)]
pub enum IslType {
    Named(IslTypeImpl),
    Anonymous(IslTypeImpl),
}

impl IslType {
    /// Creates a [IslType::Named] using the [IslConstraint] defined within it
    pub fn named<A: Into<String>, B: Into<Vec<IslConstraint>>>(name: A, constraints: B) -> IslType {
        IslType::Named(IslTypeImpl::new(Some(name.into()), constraints.into()))
    }

    /// Creates a [IslType::Anonymous] using the [IslConstraint] defined within it
    pub fn anonymous<A: Into<Vec<IslConstraint>>>(constraints: A) -> IslType {
        IslType::Anonymous(IslTypeImpl::new(None, constraints.into()))
    }

    /// Provides the underlying constraints of [IslTypeImpl]
    pub fn constraints(&self) -> &[IslConstraint] {
        match &self {
            IslType::Named(named_type) => named_type.constraints(),
            IslType::Anonymous(anonymous_type) => anonymous_type.constraints(),
        }
    }

    /// Verifies if the [IslType] allows open content or not
    pub fn open_content(&self) -> bool {
        match &self {
            IslType::Named(named_type) => named_type.open_content(),
            IslType::Anonymous(anonymous_type) => anonymous_type.open_content(),
        }
    }
}

/// Represents both named and anonymous [IslType]s and can be converted to a resolved type definition
/// Named ISL type grammar: `type:: { name: <NAME>, <CONSTRAINT>...}`
/// Anonymous ISL type grammar: `{ <CONSTRAINT>... }`
#[derive(Debug, Clone)]
pub struct IslTypeImpl {
    name: Option<String>,
    constraints: Vec<IslConstraint>,
}

impl IslTypeImpl {
    pub fn new(name: Option<String>, constraints: Vec<IslConstraint>) -> Self {
        Self { name, constraints }
    }

    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    pub fn constraints(&self) -> &[IslConstraint] {
        &self.constraints
    }

    pub fn open_content(&self) -> bool {
        let mut open_content = true;
        if self.constraints.contains(&IslConstraint::ContentClosed) {
            open_content = false;
        }
        open_content
    }

    /// Parse constraints inside an [Element] to an [IslTypeImpl]
    pub fn from_owned_element(
        ion: &Element,
        inline_imported_types: &mut Vec<IslImportType>, // stores the inline_imports that are discovered while loading this ISL type
    ) -> IonSchemaResult<Self> {
        let mut constraints = vec![];
        let contains_annotations = ion.annotations().any(|x| x == &text_token("type"));

        let ion_struct = try_to!(ion.as_struct());

        // parses the name of the type specified by schema
        let type_name: Option<String> = match ion_struct.get("name") {
            Some(name_element) => match name_element.as_str() {
                Some(name) => Some(name.to_owned()),
                None => {
                    return Err(invalid_schema_error_raw(
                        "type names must be a string or a symbol with defined text",
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
            None => format!("{:?}", ion_struct),
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

            let constraint = IslConstraint::from_ion_element(
                constraint_name,
                value,
                &isl_type_name,
                inline_imported_types,
            )?;
            constraints.push(constraint);
        }
        Ok(IslTypeImpl::new(type_name, constraints))
    }
}

// OwnedStruct doesn't preserve field order hence the PartialEq won't work properly for unordered constraints
// Related issue: https://github.com/amzn/ion-rust/issues/200
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
