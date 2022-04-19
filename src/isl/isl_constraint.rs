use crate::isl::isl_import::IslImportType;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::isl::util::Range;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence};
use ion_rs::value::{Struct, SymbolToken};
use ion_rs::IonType;
use std::collections::HashMap;

/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint {
    AllOf(Vec<IslTypeRef>),
    AnyOf(Vec<IslTypeRef>),
    ByteLength(Range),
    CodePointLength(Range),
    Contains(Vec<OwnedElement>),
    ContentClosed,
    ContainerLength(Range),
    Fields(HashMap<String, IslTypeRef>),
    Not(IslTypeRef),
    Occurs(Range),
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

    /// Creates a [IslConstraint::Fields] using the field names and [IslTypeRef]s referenced inside it
    pub fn fields<I>(fields: I) -> IslConstraint
    where
        I: Iterator<Item = (String, IslTypeRef)>,
    {
        IslConstraint::Fields(fields.collect())
    }

    /// Creates a [IslConstraint::Not] using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::Not(isl_type)
    }

    /// Creates a [IslConstraint::Contains] using the [OwnedElement] specified inside it
    pub fn contains<A: Into<Vec<OwnedElement>>>(values: A) -> IslConstraint {
        IslConstraint::Contains(values.into())
    }

    /// Creates a [IslConstraint::ContainerLength] using the range specified in it
    pub fn container_length(length: Range) -> IslConstraint {
        IslConstraint::ContainerLength(length)
    }

    /// Creates a [IslConstraint::ByteLength] using the range specified in it
    pub fn byte_length(length: Range) -> IslConstraint {
        IslConstraint::ByteLength(length)
    }

    /// Creates a [IslConstraint::CodePointLength] using the range specified in it
    pub fn codepoint_length(length: Range) -> IslConstraint {
        IslConstraint::CodePointLength(length)
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
            "byte_length" => Ok(IslConstraint::ByteLength(Range::from_ion_element(
                value, true, // Pass true as byte_length will have non negative range
            )?)),
            "codepoint_length" => Ok(IslConstraint::CodePointLength(Range::from_ion_element(
                value, true, // Pass true as codepoint_length will have non negative range
            )?)),
            "contains" => {
                if value.is_null() {
                    return Err(invalid_schema_error_raw(
                        "contains constraint was a null instead of a list".to_string(),
                    ));
                }

                if value.ion_type() != IonType::List {
                    return Err(invalid_schema_error_raw(format!(
                        "contains constraint was a {:?} instead of a list",
                        value.ion_type()
                    )));
                }

                let values: Vec<OwnedElement> = value
                    .as_sequence()
                    .unwrap()
                    .iter()
                    .map(|e| e.to_owned())
                    .collect();
                Ok(IslConstraint::Contains(values))
            }
            "content" => {
                if value.is_null() {
                    return Err(invalid_schema_error_raw(
                        "content constraint was a null instead of a symbol `closed`".to_string(),
                    ));
                }

                if value.ion_type() != IonType::Symbol {
                    return Err(invalid_schema_error_raw(format!(
                        "content constraint was a {:?} instead of a symbol `closed`",
                        value.ion_type()
                    )));
                }

                if let Some(closed) = value.as_sym().unwrap().text() {
                    if closed != "closed" {
                        return Err(invalid_schema_error_raw(format!(
                            "content constraint was a {} instead of a symbol `closed`",
                            closed
                        )));
                    }
                }

                Ok(IslConstraint::ContentClosed)
            }

            "container_length" => Ok(IslConstraint::ContainerLength(Range::from_ion_element(
                value, true, // Pass true as container_length will have non negative range
            )?)),
            "fields" => {
                let fields: HashMap<String, IslTypeRef> =
                    IslConstraint::isl_fields_from_ion_element(value, inline_imported_types)?;

                if fields.is_empty() {
                    return Err(invalid_schema_error_raw(
                        "fields constraint can not be empty",
                    ));
                }
                Ok(IslConstraint::Fields(fields))
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
            "occurs" => {
                use IonType::*;
                if value.is_null() {
                    return invalid_schema_error(
                        "expected an integer or integer range for an `occurs` constraint, found null",
                    );
                }
                let range = match value.ion_type() {
                    Symbol => {
                        let sym = try_to!(try_to!(value.as_sym()).text());
                        match sym {
                            "optional" => Range::optional(),
                            "required" => Range::required(),
                            _ => {
                                return invalid_schema_error(format!(
                                    "only optional and required symbols are supported with occurs constraint, found {}",
                                    sym
                                ))
                            }
                        }
                    }
                    Integer | List => {
                        Range::from_ion_element(value, true)? // Pass true as occurs will have non negative range
                    }
                    _ => {
                        return invalid_schema_error(format!(
                            "ion type: {:?} is not supported with occurs constraint",
                            value.ion_type()
                        ))
                    }
                };
                Ok(IslConstraint::Occurs(range))
            }
            "ordered_elements" => {
                let types: Vec<IslTypeRef> = IslConstraint::isl_type_references_from_ion_element(
                    value,
                    inline_imported_types,
                    "ordered_elements",
                )?;
                Ok(IslConstraint::OrderedElements(types))
            }
            _ => Err(invalid_schema_error_raw(
                "Type: ".to_owned()
                    + type_name
                    + " can not be built as constraint: "
                    + constraint_name
                    + " does not exist",
            )),
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
        value
            .as_sequence()
            .unwrap()
            .iter()
            .map(|e| IslTypeRef::from_ion_element(e, inline_imported_types))
            .collect::<IonSchemaResult<Vec<IslTypeRef>>>()
    }

    // helper method for from_ion_element to get isl fields from given ion element
    fn isl_fields_from_ion_element(
        value: &OwnedElement,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<HashMap<String, IslTypeRef>> {
        if value.is_null() {
            return Err(invalid_schema_error_raw(
                "fields constraint was a null instead of a struct",
            ));
        }

        if value.ion_type() != IonType::Struct {
            return Err(invalid_schema_error_raw(format!(
                "fields constraint was a {:?} instead of a struct",
                value.ion_type()
            )));
        }

        value
            .as_struct()
            .unwrap()
            .iter()
            .map(|(f, v)| {
                IslTypeRef::from_ion_element(v, inline_imported_types)
                    .map(|t| (f.text().unwrap().to_owned(), t))
            })
            .collect::<IonSchemaResult<HashMap<String, IslTypeRef>>>()
    }
}
