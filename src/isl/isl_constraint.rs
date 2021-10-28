use crate::isl::isl_import::IslImportType;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::result::{invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence};
use ion_rs::IonType;

/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint {
    AllOf(Vec<IslTypeRef>),
    Type(IslTypeRef),
    AnyOf(Vec<IslTypeRef>),
    OneOf(Vec<IslTypeRef>),
    Not(IslTypeRef),
}

impl IslConstraint {
    /// Creates a [IslConstraint::Type] using the [IslTypeRef] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::Type(isl_type)
    }

    /// Creates a [IslConstraint::AllOf] using the [IslTypeRef] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::AllOf(isl_types.into())
    }

    /// Creates a [IslConstraint::AnyOf] using the [IslTypeRef] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::AnyOf(isl_types.into())
    }

    /// Creates a [IslConstraint::OneOf] using the [IslTypeRef] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::OneOf(isl_types.into())
    }

    /// Creates a [IslConstraint::Not] using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::Not(isl_type)
    }

    /// Parse constraints inside an [OwnedElement] to an [IslConstraint]
    pub fn from_ion_element(
        constraint_name: &str,
        value: &OwnedElement,
        type_name: &str,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<IslConstraint> {
        // TODO: add more constraints to match below
        match constraint_name {
            "all_of" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                )?;
                Ok(IslConstraint::AllOf(types))
            }
            "any_of" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                )?;
                Ok(IslConstraint::AnyOf(types))
            }
            "one_of" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                )?;
                Ok(IslConstraint::OneOf(types))
            }
            "not" => {
                if value.ion_type() != IonType::Symbol && value.ion_type() != IonType::Struct {
                    return Err(invalid_schema_error_raw(format!(
                        "type constraint was a {:?} instead of a symbol/struct",
                        value.ion_type()
                    )));
                }
                let type_reference: IslTypeRef =
                    IslTypeRef::parse_from_ion_element(value, inline_imported_types)?;
                Ok(IslConstraint::Not(type_reference))
            }
            "type" => {
                if value.ion_type() != IonType::Symbol && value.ion_type() != IonType::Struct {
                    return Err(invalid_schema_error_raw(format!(
                        "type constraint was a {:?} instead of a symbol/struct",
                        value.ion_type()
                    )));
                }
                let type_reference: IslTypeRef =
                    IslTypeRef::parse_from_ion_element(value, inline_imported_types)?;
                Ok(IslConstraint::Type(type_reference))
            }
            _ => {
                return Err(invalid_schema_error_raw(
                    "Type: ".to_owned()
                        + type_name
                        + " can not be built as constraint: "
                        + constraint_name
                        + " does not exist",
                ))
            }
        }
    }

    // helper method for from_ion_element to get isl type references from given ion element
    fn isl_type_references_from_ion_element(
        value: &OwnedElement,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<Vec<IslTypeRef>> {
        //TODO: create a method/macro for this ion type check which can be reused
        if value.ion_type() != IonType::List {
            return Err(invalid_schema_error_raw(format!(
                "all_of constraint was a {:?} instead of a list",
                value.ion_type()
            )));
        }
        Ok(value
            .as_sequence()
            .unwrap()
            .iter()
            .map(|e| IslTypeRef::parse_from_ion_element(e, inline_imported_types))
            .collect::<IonSchemaResult<Vec<IslTypeRef>>>()?)
    }
}
