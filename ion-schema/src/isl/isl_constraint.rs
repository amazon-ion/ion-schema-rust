use crate::isl::isl_import::IslImportType;
use crate::isl::isl_range::{Range, RangeType};
use crate::isl::isl_type_reference::IslTypeRef;
use crate::isl::util::{Annotation, TimestampOffset, ValidValue};
use crate::isl::IonSchemaLanguageVersion;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
};
use ion_rs::value::owned::{text_token, Element};
use ion_rs::value::IonStruct;
use ion_rs::value::{IonElement, IonSequence};
use ion_rs::IonType;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

/// Provides public facing APIs for constructing ISL constraints programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::{
        IslAnnotationsConstraint, IslConstraint, IslConstraintImpl, IslRegexConstraint,
        IslTimestampOffsetConstraint, IslValidValuesConstraint,
    };
    use crate::isl::isl_range::{IntegerRange, NonNegativeIntegerRange, Range, RangeImpl};
    use crate::isl::isl_type_reference::IslTypeRef;
    use crate::isl::util::{Annotation, TimestampOffset, TimestampPrecision, ValidValue};
    use crate::result::IonSchemaResult;
    use ion_rs::value::owned::Element;
    use ion_rs::value::IonElement;

    /// Creates an [IslConstraint::Type] using the [IslTypeRef] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Type(isl_type))
    }

    /// Creates an [IslConstraint::AllOf] using the [IslTypeRef] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::AllOf(isl_types.into()))
    }

    /// Creates an [IslConstraint::AnyOf] using the [IslTypeRef] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::AnyOf(isl_types.into()))
    }

    /// Creates an [IslConstraint::OneOf] using the [IslTypeRef] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::OneOf(isl_types.into()))
    }

    /// Creates an [IslConstraint::OrderedElements] using the [IslTypeRef] referenced inside it
    pub fn ordered_elements<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::OrderedElements(isl_types.into()))
    }

    /// Creates an [IslConstraint::Precision] using the range specified in it
    pub fn precision(precision: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Precision(Range::NonNegativeInteger(
            precision,
        )))
    }

    /// Creates an [IslConstraint::Scale] using the range specified in it
    pub fn scale(scale: IntegerRange) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Scale(Range::Integer(scale)))
    }

    /// Creates an [IslConstraint::Fields] using the field names and [IslTypeRef]s referenced inside it
    pub fn fields<I>(fields: I) -> IslConstraint
    where
        I: Iterator<Item = (String, IslTypeRef)>,
    {
        IslConstraint::new(IslConstraintImpl::Fields(fields.collect()))
    }

    /// Creates an [IslConstraint::Not] using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Not(isl_type))
    }

    /// Creates a [IslConstraint::Contains] using the [Element] specified inside it
    pub fn contains<A: Into<Vec<Element>>>(values: A) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Contains(values.into()))
    }

    /// Creates an [IslConstraint::ContainerLength] using the range specified in it
    pub fn container_length(length: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::ContainerLength(
            Range::NonNegativeInteger(length),
        ))
    }

    /// Creates an [IslConstraint::ByteLength] using the range specified in it
    pub fn byte_length(length: RangeImpl<usize>) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::ByteLength(Range::NonNegativeInteger(
            length,
        )))
    }

    /// Creates an [IslConstraint::CodepointLength] using the range specified in it
    pub fn codepoint_length(length: RangeImpl<usize>) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::CodepointLength(
            Range::NonNegativeInteger(length),
        ))
    }

    /// Creates an [IslConstraint::TimestampPrecision] using the range specified in it
    pub fn timestamp_precision(precision: RangeImpl<TimestampPrecision>) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::TimestampPrecision(
            Range::TimestampPrecision(precision),
        ))
    }

    /// Creates an [IslConstraint::TimestampOffset] using the offset list specified in it
    pub fn timestamp_offset(offsets: Vec<TimestampOffset>) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::TimestampOffset(
            IslTimestampOffsetConstraint::new(offsets),
        ))
    }

    /// Creates an [IslConstraint::Utf8ByteLength] using the range specified in it
    pub fn utf8_byte_length(length: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Utf8ByteLength(
            Range::NonNegativeInteger(length),
        ))
    }

    /// Creates an [IslConstraint::Element] using the [IslTypeRef] referenced inside it
    pub fn element(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Element(isl_type))
    }

    /// Creates an [IslConstraint::Annotations] using [str]s and [Element]s specified inside it
    pub fn annotations<'a, A: IntoIterator<Item = &'a str>, B: IntoIterator<Item = Element>>(
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
        IslConstraint::new(IslConstraintImpl::Annotations(
            IslAnnotationsConstraint::new(
                annotations_modifiers.contains(&"closed"),
                annotations_modifiers.contains(&"ordered"),
                annotations,
            ),
        ))
    }

    /// Creates a [IslConstraint::ValidValues] using the [Element]s specified inside it
    pub fn valid_values_with_values(values: Vec<Element>) -> IonSchemaResult<IslConstraint> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> =
            values.iter().map(|e| e.try_into()).collect();
        Ok(IslConstraint::new(IslConstraintImpl::ValidValues(
            IslValidValuesConstraint {
                valid_values: valid_values?,
            },
        )))
    }

    /// Creates a [IslConstraint::ValidValues] using the [Range] specified inside it
    pub fn valid_values_with_range(range: Range) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::ValidValues(IslValidValuesConstraint {
            valid_values: vec![ValidValue::Range(range)],
        }))
    }

    /// Creates an [IslConstraint::Regex] using the expression and flags (case_insensitive, multi_line)
    pub fn regex(case_insensitive: bool, multi_line: bool, expression: String) -> IslConstraint {
        IslConstraint::new(IslConstraintImpl::Regex(IslRegexConstraint::new(
            case_insensitive,
            multi_line,
            expression,
        )))
    }
}

/// Provides public facing APIs for constructing ISL constraints programmatically for ISL 2.0
mod v_2_0 {
    use crate::isl::isl_constraint::v_1_0;
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_range::{NonNegativeIntegerRange, Range, RangeImpl};
    use crate::isl::isl_type_reference::IslTypeRef;
    use crate::isl::util::{TimestampOffset, TimestampPrecision, ValidValue};
    use crate::result::IonSchemaResult;
    use ion_rs::value::owned::Element;

    /// Creates an [IslConstraint::Type] using the [IslTypeRef] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type: IslTypeRef) -> IslConstraint {
        v_1_0::type_constraint(isl_type)
    }

    /// Creates an [IslConstraint::AllOf] using the [IslTypeRef] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        v_1_0::all_of(isl_types)
    }

    /// Creates an [IslConstraint::AnyOf] using the [IslTypeRef] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        v_1_0::any_of(isl_types)
    }

    /// Creates an [IslConstraint::OneOf] using the [IslTypeRef] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        v_1_0::one_of(isl_types)
    }

    /// Creates an [IslConstraint::OrderedElements] using the [IslTypeRef] referenced inside it
    pub fn ordered_elements<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        v_1_0::ordered_elements(isl_types)
    }

    /// Creates an [IslConstraint::Precision] using the range specified in it
    pub fn precision(precision: NonNegativeIntegerRange) -> IslConstraint {
        v_1_0::precision(precision)
    }

    /// Creates an [IslConstraint::Fields] using the field names and [IslTypeRef]s referenced inside it
    pub fn fields<I>(fields: I) -> IslConstraint
    where
        I: Iterator<Item = (String, IslTypeRef)>,
    {
        v_1_0::fields(fields)
    }

    /// Creates an [IslConstraint::Not] using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        v_1_0::not(isl_type)
    }

    /// Creates a [IslConstraint::Contains] using the [Element] specified inside it
    pub fn contains<A: Into<Vec<Element>>>(values: A) -> IslConstraint {
        v_1_0::contains(values)
    }

    /// Creates an [IslConstraint::ContainerLength] using the range specified in it
    pub fn container_length(length: NonNegativeIntegerRange) -> IslConstraint {
        v_1_0::container_length(length)
    }

    /// Creates an [IslConstraint::ByteLength] using the range specified in it
    pub fn byte_length(length: RangeImpl<usize>) -> IslConstraint {
        v_1_0::byte_length(length)
    }

    /// Creates an [IslConstraint::CodepointLength] using the range specified in it
    pub fn codepoint_length(length: RangeImpl<usize>) -> IslConstraint {
        v_1_0::codepoint_length(length)
    }

    /// Creates an [IslConstraint::TimestampPrecision] using the range specified in it
    pub fn timestamp_precision(precision: RangeImpl<TimestampPrecision>) -> IslConstraint {
        v_1_0::timestamp_precision(precision)
    }

    /// Creates an [IslConstraint::TimestampOffset] using the offset list specified in it
    pub fn timestamp_offset(offsets: Vec<TimestampOffset>) -> IslConstraint {
        v_1_0::timestamp_offset(offsets)
    }

    /// Creates an [IslConstraint::Utf8ByteLength] using the range specified in it
    pub fn utf8_byte_length(length: NonNegativeIntegerRange) -> IslConstraint {
        v_1_0::utf8_byte_length(length)
    }

    /// Creates an [IslConstraint::Element] using the [IslTypeRef] referenced inside it
    pub fn element(isl_type: IslTypeRef) -> IslConstraint {
        // add support for `distinct::` elements
        todo!()
    }

    /// Creates an [IslConstraint::Annotations] using [str]s and [Element]s specified inside it
    pub fn annotations<'a, A: IntoIterator<Item = &'a str>, B: IntoIterator<Item = Element>>(
        annotations_modifiers: A,
        annotations: B,
    ) -> IslConstraint {
        todo!()
    }

    /// Creates a [IslConstraint::ValidValues] using the [Element]s specified inside it
    pub fn valid_values_with_values(values: Vec<Element>) -> IonSchemaResult<IslConstraint> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> =
            values.iter().map(|e| e.try_into()).collect();
        v_1_0::valid_values_with_values(values)
    }

    /// Creates a [IslConstraint::ValidValues] using the [Range] specified inside it
    pub fn valid_values_with_range(range: Range) -> IslConstraint {
        v_1_0::valid_values_with_range(range)
    }

    /// Creates an [IslConstraint::Regex] using the expression and flags (case_insensitive, multi_line)
    pub fn regex(case_insensitive: bool, multi_line: bool, expression: String) -> IslConstraint {
        todo!()
    }
}

/// Represents schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub struct IslConstraint {
    pub(crate) constraint: IslConstraintImpl,
}

impl IslConstraint {
    pub(crate) fn new(constraint: IslConstraintImpl) -> Self {
        Self { constraint }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraintImpl {
    AllOf(Vec<IslTypeRef>),
    Annotations(IslAnnotationsConstraint),
    AnyOf(Vec<IslTypeRef>),
    ByteLength(Range),
    CodepointLength(Range),
    Contains(Vec<Element>),
    ContentClosed,
    ContainerLength(Range),
    Element(IslTypeRef),
    Fields(HashMap<String, IslTypeRef>),
    Not(IslTypeRef),
    Occurs(Range),
    OneOf(Vec<IslTypeRef>),
    OrderedElements(Vec<IslTypeRef>),
    Precision(Range),
    Regex(IslRegexConstraint),
    Scale(Range),
    TimestampOffset(IslTimestampOffsetConstraint),
    TimestampPrecision(Range),
    Type(IslTypeRef),
    Unknown(String, Element), // Unknown constraint is used to store open contents
    Utf8ByteLength(Range),
    ValidValues(IslValidValuesConstraint),
}

impl IslConstraintImpl {
    /// Parse constraints inside an [Element] to an [IslConstraint]
    pub fn from_ion_element(
        isl_version: IonSchemaLanguageVersion,
        constraint_name: &str,
        value: &Element,
        type_name: &str,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<IslConstraintImpl> {
        // TODO: add more constraints to match below
        match constraint_name {
            "all_of" => {
                let types: Vec<IslTypeRef> =
                    IslConstraintImpl::isl_type_references_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                        "all_of",
                    )?;
                Ok(IslConstraintImpl::AllOf(types))
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

                Ok(IslConstraintImpl::Annotations(value.try_into()?))
            }
            "any_of" => {
                let types: Vec<IslTypeRef> =
                    IslConstraintImpl::isl_type_references_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                        "any_of",
                    )?;
                Ok(IslConstraintImpl::AnyOf(types))
            }
            "byte_length" => Ok(IslConstraintImpl::ByteLength(Range::from_ion_element(
                value,
                RangeType::NonNegativeInteger,
            )?)),
            "codepoint_length" => Ok(IslConstraintImpl::CodepointLength(Range::from_ion_element(
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

                let values: Vec<Element> = value
                    .as_sequence()
                    .unwrap()
                    .iter()
                    .map(|e| e.to_owned())
                    .collect();
                Ok(IslConstraintImpl::Contains(values))
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
                            "content constraint was a {closed} instead of a symbol `closed`"
                        )));
                    }
                }

                Ok(IslConstraintImpl::ContentClosed)
            }

            "container_length" => Ok(IslConstraintImpl::ContainerLength(Range::from_ion_element(
                value,
                RangeType::NonNegativeInteger,
            )?)),
            "element" => {
                let type_reference: IslTypeRef =
                    IslTypeRef::from_ion_element(isl_version, value, inline_imported_types)?;
                Ok(IslConstraintImpl::Element(type_reference))
            }
            "fields" => {
                let fields: HashMap<String, IslTypeRef> =
                    IslConstraintImpl::isl_fields_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                    )?;

                if fields.is_empty() {
                    return Err(invalid_schema_error_raw(
                        "fields constraint can not be empty",
                    ));
                }
                Ok(IslConstraintImpl::Fields(fields))
            }
            "one_of" => {
                let types: Vec<IslTypeRef> =
                    IslConstraintImpl::isl_type_references_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                        "one_of",
                    )?;
                Ok(IslConstraintImpl::OneOf(types))
            }
            "not" => {
                let type_reference: IslTypeRef =
                    IslTypeRef::from_ion_element(isl_version, value, inline_imported_types)?;
                Ok(IslConstraintImpl::Not(type_reference))
            }
            "type" => {
                let type_reference: IslTypeRef =
                    IslTypeRef::from_ion_element(isl_version, value, inline_imported_types)?;
                Ok(IslConstraintImpl::Type(type_reference))
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
                                    "only optional and required symbols are supported with occurs constraint, found {sym}"
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
                Ok(IslConstraintImpl::Occurs(range))
            }
            "ordered_elements" => {
                let types: Vec<IslTypeRef> =
                    IslConstraintImpl::isl_type_references_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                        "ordered_elements",
                    )?;
                Ok(IslConstraintImpl::OrderedElements(types))
            }
            "precision" => Ok(IslConstraintImpl::Precision(Range::from_ion_element(
                value,
                RangeType::Precision,
            )?)),
            "regex" => {
                let case_insensitive = value.annotations().any(|a| a == &text_token("i"));
                let multi_line = value.annotations().any(|a| a == &text_token("m"));

                let expression = value.as_str().ok_or_else(|| {
                    invalid_schema_error_raw(format!(
                        "expected regex to contain a string expression but found: {}",
                        value.ion_type()
                    ))
                })?;

                Ok(IslConstraintImpl::Regex(IslRegexConstraint::new(
                    case_insensitive,
                    multi_line,
                    expression.to_string(),
                )))
            }
            "scale" => Ok(IslConstraintImpl::Scale(Range::from_ion_element(
                value,
                RangeType::Any,
            )?)),
            "timestamp_precision" => Ok(IslConstraintImpl::TimestampPrecision(
                Range::from_ion_element(value, RangeType::TimestampPrecision)?,
            )),
            "timestamp_offset" => {
                use IonType::*;
                if value.is_null() {
                    return invalid_schema_error(
                        "expected a list of valid offsets for an `timestamp_offset` constraint, found null",
                    );
                }

                if value.annotations().next().is_some() {
                    return invalid_schema_error("`timestamp_offset` list may not be annotated");
                }

                let valid_offsets: Vec<TimestampOffset> = match value.ion_type() {
                    List => {
                        let list_values = value.as_sequence().unwrap();
                        if list_values.is_empty() {
                            return invalid_schema_error(
                                "`timestamp_offset` constraint must contain at least one offset",
                            );
                        }
                        let list_vec: IonSchemaResult<Vec<TimestampOffset>> = list_values
                            .iter()
                            .map(|e| {
                                if e.is_null() {
                                    return invalid_schema_error(
                                    "`timestamp_offset` values must be non-null strings, found null"
                                );
                                }

                                if e.ion_type() != IonType::String {
                                    return invalid_schema_error(format!(
                                    "`timestamp_offset` values must be non-null strings, found {e}"
                                ));
                                }

                                if e.annotations().next().is_some() {
                                    return invalid_schema_error(format!(
                                        "`timestamp_offset` values may not be annotated, found {e}"
                                    ));
                                }

                                // unwrap here will not panic as we have already verified the ion type to be a string
                                let string_value = e.as_str().unwrap();

                                // convert the string to TimestampOffset which stores offset in minutes
                                string_value.try_into()
                            })
                            .collect();
                        list_vec?
                    }
                    _ => {
                        return invalid_schema_error(format!(
                        "`timestamp_offset` requires a list of offset strings, but found: {value}"
                    ))
                    }
                };
                Ok(IslConstraintImpl::TimestampOffset(
                    IslTimestampOffsetConstraint::new(valid_offsets),
                ))
            }
            "utf8_byte_length" => Ok(IslConstraintImpl::Utf8ByteLength(Range::from_ion_element(
                value,
                RangeType::NonNegativeInteger,
            )?)),
            "valid_values" => Ok(IslConstraintImpl::ValidValues(value.try_into()?)),
            _ => Ok(IslConstraintImpl::Unknown(
                constraint_name.to_string(),
                value.to_owned(),
            )),
        }
    }

    // helper method for from_ion_element to get isl type references from given ion element
    fn isl_type_references_from_ion_element(
        isl_version: IonSchemaLanguageVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
        constraint_name: &str,
    ) -> IonSchemaResult<Vec<IslTypeRef>> {
        //TODO: create a method/macro for this ion type check which can be reused
        if value.is_null() {
            return Err(invalid_schema_error_raw(format!(
                "{constraint_name} constraint was a null instead of a list"
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
            .map(|e| IslTypeRef::from_ion_element(isl_version.to_owned(), e, inline_imported_types))
            .collect::<IonSchemaResult<Vec<IslTypeRef>>>()
    }

    // helper method for from_ion_element to get isl fields from given ion element
    fn isl_fields_from_ion_element(
        isl_version: IonSchemaLanguageVersion,
        value: &Element,
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
                IslTypeRef::from_ion_element(isl_version.to_owned(), v, inline_imported_types)
                    .map(|t| (f.text().unwrap().to_owned(), t))
            })
            .collect::<IonSchemaResult<HashMap<String, IslTypeRef>>>()
    }
}

/// Represents the `annotations` constraint
/// `annotations`: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations>`
// The `required` annotation provided on the list of annotations is not represented here,
// requirement of an annotation is represented in the annotation itself by the field `is_required` of `Annotation` struct.
#[derive(Debug, Clone, PartialEq, Eq)]
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

impl TryFrom<&Element> for IslAnnotationsConstraint {
    type Error = IonSchemaError;

    fn try_from(value: &Element) -> IonSchemaResult<Self> {
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

/// Represents the `valid_values` constraint
/// `valid_values`: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations>`
#[derive(Debug, Clone, PartialEq)]
pub struct IslValidValuesConstraint {
    pub(crate) valid_values: Vec<ValidValue>,
}

impl IslValidValuesConstraint {
    /// Provides a way to programmatically construct valid_values constraint
    /// Returns IonSchemaError whenever annotations are provided within ValidValue::Element
    /// only `range` annotations are accepted for ValidValue::Element
    pub fn new(valid_values: Vec<ValidValue>) -> IonSchemaResult<Self> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> = valid_values
            .iter()
            .map(|v| match v {
                ValidValue::Range(r) => Ok(v.to_owned()),
                ValidValue::Element(e) => e.try_into(),
            })
            .collect();
        Ok(Self {
            valid_values: valid_values?,
        })
    }

    pub fn values(&self) -> &Vec<ValidValue> {
        &self.valid_values
    }
}

impl TryFrom<&Element> for IslValidValuesConstraint {
    type Error = IonSchemaError;

    fn try_from(value: &Element) -> IonSchemaResult<Self> {
        if value.annotations().any(|a| a == &text_token("range")) {
            return IslValidValuesConstraint::new(vec![ValidValue::Range(
                Range::from_ion_element(value, RangeType::NumberOrTimestamp)?,
            )]);
        }
        if let Some(values) = value.as_sequence() {
            if value.ion_type() == IonType::List {
                let mut valid_values = vec![];
                let values: IonSchemaResult<Vec<()>> = values
                    .iter()
                    .map(|e| {
                        valid_values.push(e.try_into()?);
                        Ok(())
                    })
                    .collect();
                values?;
                return Ok(IslValidValuesConstraint { valid_values });
            }
        }
        invalid_schema_error(format!(
            "Expected valid_values to be a range or a list of valid values, found {}",
            value.ion_type()
        ))
    }
}

/// Represents the `regex` constraint
/// `regex`: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#regex>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IslRegexConstraint {
    case_insensitive: bool,
    multi_line: bool,
    expression: String,
}

impl IslRegexConstraint {
    pub(crate) fn new(case_insensitive: bool, multi_line: bool, expression: String) -> Self {
        Self {
            case_insensitive,
            multi_line,
            expression,
        }
    }

    pub fn expression(&self) -> &String {
        &self.expression
    }

    pub fn case_insensitive(&self) -> bool {
        self.case_insensitive
    }

    pub fn multi_line(&self) -> bool {
        self.multi_line
    }
}

/// Represents the `timestamp_offset` constraint
/// `timestamp_offset`: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_offset>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IslTimestampOffsetConstraint {
    valid_offsets: Vec<TimestampOffset>,
}

impl IslTimestampOffsetConstraint {
    pub(crate) fn new(valid_offsets: Vec<TimestampOffset>) -> Self {
        Self { valid_offsets }
    }

    pub fn valid_offsets(&self) -> &[TimestampOffset] {
        &self.valid_offsets
    }
}
