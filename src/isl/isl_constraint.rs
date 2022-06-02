use crate::isl::isl_import::IslImportType;
use crate::isl::isl_type_reference::IslTypeRef;
use crate::isl::util::{Annotation, Range, RangeType};
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence};
use ion_rs::value::{Struct, SymbolToken};
use ion_rs::IonType;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint {
    AllOf(Vec<IslTypeRef>),
    Annotations(IslAnnotationsConstraint),
    AnyOf(Vec<IslTypeRef>),
    ByteLength(Range),
    CodepointLength(Range),
    Contains(Vec<OwnedElement>),
    ContentClosed,
    ContainerLength(Range),
    Element(IslTypeRef),
    Fields(HashMap<String, IslTypeRef>),
    Not(IslTypeRef),
    Occurs(Range),
    OneOf(Vec<IslTypeRef>),
    OrderedElements(Vec<IslTypeRef>),
    Precision(Range),
    Scale(Range),
    TimestampPrecision(Range),
    Type(IslTypeRef),
}

impl IslConstraint {
    /// Creates an [IslConstraint::Type] using the [IslTypeRef] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::Type(isl_type)
    }

    /// Creates an [IslConstraint::AllOf] using the [IslTypeRef] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::AllOf(isl_types.into())
    }

    /// Creates an [IslConstraint::AnyOf] using the [IslTypeRef] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::AnyOf(isl_types.into())
    }

    /// Creates an [IslConstraint::OneOf] using the [IslTypeRef] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::OneOf(isl_types.into())
    }

    /// Creates an [IslConstraint::OrderedElements] using the [IslTypeRef] referenced inside it
    pub fn ordered_elements<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::OrderedElements(isl_types.into())
    }

    /// Creates an [IslConstraint::Precision] using the range specified in it
    pub fn precision(precision: Range) -> IslConstraint {
        if !matches!(precision, Range::IntegerNonNegative(_, _)) {
            panic!("precision constraint  must have a range of type IntegerNonNegative")
        }
        IslConstraint::Precision(precision)
    }

    /// Creates an [IslConstraint::Scale] using the range specified in it
    pub fn scale(scale: Range) -> IslConstraint {
        if !matches!(scale, Range::Integer(_, _)) {
            panic!("scale constraint  must have a range of type Integer")
        }
        IslConstraint::Scale(scale)
    }

    /// Creates an [IslConstraint::Fields] using the field names and [IslTypeRef]s referenced inside it
    pub fn fields<I>(fields: I) -> IslConstraint
    where
        I: Iterator<Item = (String, IslTypeRef)>,
    {
        IslConstraint::Fields(fields.collect())
    }

    /// Creates an [IslConstraint::Not] using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::Not(isl_type)
    }

    /// Creates a [IslConstraint::Contains] using the [OwnedElement] specified inside it
    pub fn contains<A: Into<Vec<OwnedElement>>>(values: A) -> IslConstraint {
        IslConstraint::Contains(values.into())
    }

    /// Creates an [IslConstraint::ContainerLength] using the range specified in it
    pub fn container_length(length: Range) -> IslConstraint {
        if !matches!(length, Range::IntegerNonNegative(_, _)) {
            panic!("container_length constraint  must have a range of type IntegerNonNegative")
        }
        IslConstraint::ContainerLength(length)
    }

    /// Creates an [IslConstraint::ByteLength] using the range specified in it
    pub fn byte_length(length: Range) -> IslConstraint {
        if !matches!(length, Range::IntegerNonNegative(_, _)) {
            panic!("byte_length constraint  must have a range of type IntegerNonNegative")
        }
        IslConstraint::ByteLength(length)
    }

    /// Creates an [IslConstraint::CodePointLength] using the range specified in it
    pub fn codepoint_length(length: Range) -> IslConstraint {
        if !matches!(length, Range::IntegerNonNegative(_, _)) {
            panic!("codepoint_length constraint  must have a range of type IntegerNonNegative")
        }
        IslConstraint::CodepointLength(length)
    }

    /// Creates an [IslConstraint::TimestampPrecision] using the range specified in it
    pub fn timestamp_precision(precision: Range) -> IslConstraint {
        IslConstraint::TimestampPrecision(precision)
    }

    /// Creates an [IslConstraint::Element] using the [IslTypeRef] referenced inside it
    pub fn element(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::Element(isl_type)
    }

    /// Creates an [IslConstraint::Annotations] using [str]s and [OwnedElement]s specified inside it
    pub fn annotations<
        'a,
        A: IntoIterator<Item = &'a str>,
        B: IntoIterator<Item = OwnedElement>,
    >(
        annotations_modifiers: A,
        annotations: B,
    ) -> IslConstraint {
        let annotations_modifiers: Vec<&str> = annotations_modifiers.into_iter().collect();
        let annotations: Vec<Annotation> = annotations
            .into_iter()
            .map(|a| {
                Annotation::new(
                    a.as_str().unwrap().to_owned(),
                    Annotation::is_annotation_required(
                        &a,
                        annotations_modifiers.contains(&"required"),
                    ),
                )
            })
            .collect();
        IslConstraint::Annotations(IslAnnotationsConstraint::new(
            annotations_modifiers.contains(&"closed"),
            annotations_modifiers.contains(&"ordered"),
            annotations,
        ))
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
            "annotations" => {
                if value.is_null() {
                    return Err(invalid_schema_error_raw(
                        "annotations constraint was a null instead of a list",
                    ));
                }

                if value.ion_type() != IonType::List {
                    return Err(invalid_schema_error_raw(format!(
                        "annotations constraint was a {:?} instead of a list",
                        value.ion_type()
                    )));
                }

                Ok(IslConstraint::Annotations(value.try_into()?))
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
                value,
                RangeType::NonNegativeInteger,
            )?)),
            "codepoint_length" => Ok(IslConstraint::CodepointLength(Range::from_ion_element(
                value,
                RangeType::NonNegativeInteger,
            )?)),
            "contains" => {
                if value.is_null() {
                    return Err(invalid_schema_error_raw(
                        "contains constraint was a null instead of a list",
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
                        "content constraint was a null instead of a symbol `closed`",
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
                value,
                RangeType::NonNegativeInteger,
            )?)),
            "element" => {
                let type_reference: IslTypeRef =
                    IslTypeRef::from_ion_element(value, inline_imported_types)?;
                Ok(IslConstraint::Element(type_reference))
            }
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
                let type_reference: IslTypeRef =
                    IslTypeRef::from_ion_element(value, inline_imported_types)?;
                Ok(IslConstraint::Not(type_reference))
            }
            "type" => {
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
                        Range::from_ion_element(value, RangeType::NonNegativeInteger)?
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
            "precision" => Ok(IslConstraint::Precision(Range::from_ion_element(
                value,
                RangeType::Precision,
            )?)),
            "scale" => Ok(IslConstraint::Scale(Range::from_ion_element(
                value,
                RangeType::Any,
            )?)),
            "timestamp_precision" => Ok(IslConstraint::TimestampPrecision(
                Range::from_ion_element(value, RangeType::TimestampPrecision)?,
            )),
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

/// Represents the `annotations` constraint
/// [annotations]: https://amzn.github.io/ion-schema/docs/spec.html#annotations
// The `required` annotation provided on the list of annotations is not represented here,
// requirement of an annotation is represented in the annotation itself by the field `is_required` of `Annotation` struct.
#[derive(Debug, Clone, PartialEq)]
pub struct IslAnnotationsConstraint {
    pub is_closed: bool,
    pub is_ordered: bool,
    pub annotations: Vec<Annotation>,
}

impl IslAnnotationsConstraint {
    pub fn new(is_closed: bool, is_ordered: bool, annotations: Vec<Annotation>) -> Self {
        Self {
            is_closed,
            is_ordered,
            annotations,
        }
    }
}

impl TryFrom<&OwnedElement> for IslAnnotationsConstraint {
    type Error = IonSchemaError;

    fn try_from(value: &OwnedElement) -> IonSchemaResult<Self> {
        let annotation_modifiers: Vec<&str> =
            value.annotations().map(|sym| sym.text().unwrap()).collect();

        let annotations: Vec<Annotation> = value
            .as_sequence()
            .unwrap()
            .iter()
            .map(|e| {
                Annotation::new(
                    e.as_str().unwrap().to_owned(),
                    Annotation::is_annotation_required(
                        e,
                        annotation_modifiers.contains(&"required"),
                    ),
                )
            })
            .collect();

        Ok(IslAnnotationsConstraint::new(
            annotation_modifiers.contains(&"closed"),
            annotation_modifiers.contains(&"ordered"),
            annotations,
        ))
    }
}
