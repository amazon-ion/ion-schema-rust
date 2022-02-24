use crate::isl::isl_import::IslImportType;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::isl::util::Range;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::SymbolToken;
use ion_rs::value::{AnyInt, Element, Sequence};
use ion_rs::IonType;

/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint {
    AllOf(Vec<IslTypeRef>),
    AnyOf(Vec<IslTypeRef>),
    Not(IslTypeRef),
    Occurs(IslOccurs),
    OneOf(Vec<IslTypeRef>),
    OrderedElements(Vec<IslTypeRef>),
    Type(IslTypeRef),
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

    /// Creates a [IslConstraint::OrderedElements] using the [IslTypeRef] referenced inside it
    pub fn ordered_elements<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::OrderedElements(isl_types.into())
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
                    "all_of",
                )?;
                Ok(IslConstraint::AllOf(types))
            }
            "any_of" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                    "any_of",
                )?;
                Ok(IslConstraint::AnyOf(types))
            }
            "one_of" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                    "one_of",
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
                    IslTypeRef::from_ion_element(value, inline_imported_types)?;
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
                    IslTypeRef::from_ion_element(value, inline_imported_types)?;
                Ok(IslConstraint::Type(type_reference))
            }
            "occurs" => Ok(IslConstraint::Occurs(IslOccurs::from_ion_element(value)?)),
            "ordered_elements" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                    "ordered_elements",
                )?;
                Ok(IslConstraint::OrderedElements(types))
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
        constraint_name: &str,
    ) -> IonSchemaResult<Vec<IslTypeRef>> {
        //TODO: create a method/macro for this ion type check which can be reused
        if value.is_null() {
            return Err(invalid_schema_error_raw(format!(
                "{} constraint was a null instead of a list",
                constraint_name
            )));
        }
        if value.ion_type() != IonType::List {
            return Err(invalid_schema_error_raw(format!(
                "{} constraint was a {:?} instead of a list",
                constraint_name,
                value.ion_type()
            )));
        }
        Ok(value
            .as_sequence()
            .unwrap()
            .iter()
            .map(|e| IslTypeRef::from_ion_element(e, inline_imported_types))
            .collect::<IonSchemaResult<Vec<IslTypeRef>>>()?)
    }
}

/// Represents ISL [Occurs] where some constraints can be defined by occurs
/// Grammar: <OCCURS> ::= occurs: <INT>
///            | occurs: <RANGE<INT>>
///            | occurs: optional
///            | occurs: required
#[derive(Debug, Clone, PartialEq)]
pub enum IslOccurs {
    Int(AnyInt),
    Range(Range),
    Optional,
    Required,
}

impl IslOccurs {
    pub fn from_ion_element(value: &OwnedElement) -> IonSchemaResult<IslOccurs> {
        use IonType::*;
        match value.ion_type() {
            Integer => Ok(IslOccurs::Int(value.as_any_int().unwrap().to_owned())),
            List => {
                let value_annotations: Vec<&OwnedSymbolToken> = value.annotations().collect();
                if value_annotations.contains(&&text_token("range")) {
                    let is_non_negative = false; // for integer range is_non_negative is false
                    let range = Range::from_ion_element(value, is_non_negative)?;
                    match range {
                        Range::Integer(_, _) => {}
                        _ => {
                            return invalid_schema_error(
                                "only integer ranges are supported for occurs constraint",
                            )
                        }
                    };
                    return Ok(IslOccurs::Range(range));
                } else {
                    return invalid_schema_error("range annotation missing for occurs constraint");
                }
            }
            Symbol => {
                let sym = try_to!(try_to!(value.as_sym()).text());
                match sym {
                    "optional" => Ok(IslOccurs::Optional),
                    "required" => Ok(IslOccurs::Required),
                    _ => {
                        return invalid_schema_error(format!(
                            "only optional and required symbols are supported with occurs constraint, found {}",
                            sym
                        ))
                    }
                }
            }
            _ => {
                return invalid_schema_error(format!(
                    "ion type: {:?} is not supported with occurs constraint",
                    value.ion_type()
                ))
            }
        }
    }
}
