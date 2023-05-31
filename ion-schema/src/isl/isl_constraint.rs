use crate::isl;
use crate::isl::isl_import::IslImportType;
use crate::isl::isl_range::{Range, RangeType};
use crate::isl::isl_type_reference::{IslTypeRefImpl, IslVariablyOccurringTypeRef};
use crate::isl::util::{Annotation, Ieee754InterchangeFormat, TimestampOffset, ValidValue};
use crate::isl::IslVersion;
use crate::result::{invalid_schema_error, invalid_schema_error_raw, IonSchemaResult};
use ion_rs::element::Element;
use ion_rs::IonType;
use std::collections::HashMap;
use std::convert::TryInto;

/// Provides public facing APIs for constructing ISL constraints programmatically for ISL 1.0
pub mod v_1_0 {
    use crate::isl::isl_constraint::{
        IslAnnotationsConstraint, IslConstraint, IslConstraintImpl, IslRegexConstraint,
        IslSimpleAnnotationsConstraint, IslTimestampOffsetConstraint, IslValidValuesConstraint,
    };
    use crate::isl::isl_range::{IntegerRange, NonNegativeIntegerRange, Range, RangeImpl};
    use crate::isl::isl_type_reference::{IslTypeRef, IslVariablyOccurringTypeRef};
    use crate::isl::util::{Annotation, TimestampOffset, TimestampPrecision, ValidValue};
    use crate::isl::IslVersion;
    use crate::result::IonSchemaResult;
    use ion_rs::element::Element;

    /// Creates a `type` constraint using the [IslTypeRef] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Type(isl_type.type_reference),
        )
    }

    /// Creates an `all_of` constraint using the [IslTypeRef] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::AllOf(
                isl_types
                    .into()
                    .into_iter()
                    .map(|t| t.type_reference)
                    .collect(),
            ),
        )
    }

    /// Creates an `any_of` constraint using the [IslTypeRef] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::AnyOf(
                isl_types
                    .into()
                    .into_iter()
                    .map(|t| t.type_reference)
                    .collect(),
            ),
        )
    }

    /// Creates a `one_of` constraint using the [IslTypeRef] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::OneOf(
                isl_types
                    .into()
                    .into_iter()
                    .map(|t| t.type_reference)
                    .collect(),
            ),
        )
    }

    /// Creates an `ordered_elements` constraint using the [IslVariablyOccurringTypeRef] referenced inside it
    pub fn ordered_elements<A: Into<Vec<IslVariablyOccurringTypeRef>>>(
        isl_types: A,
    ) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::OrderedElements(isl_types.into().into_iter().collect()),
        )
    }

    /// Creates a `precision` constraint using the range specified in it
    pub fn precision(precision: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Precision(Range::NonNegativeInteger(precision)),
        )
    }

    /// Creates a `scale` constraint using the range specified in it
    pub fn scale(scale: IntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Scale(Range::Integer(scale)),
        )
    }

    /// Creates a `fields` constraint using the field names and [IslVariablyOccurringTypeRef]s referenced inside it
    pub fn fields<I>(fields: I) -> IslConstraint
    where
        I: Iterator<Item = (String, IslVariablyOccurringTypeRef)>,
    {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Fields(fields.map(|(s, t)| (s, t)).collect(), false),
        )
    }

    /// Creates a `not` constraint using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Not(isl_type.type_reference),
        )
    }

    /// Creates a `contains` constraint using the [Element] specified inside it
    pub fn contains<A: Into<Vec<Element>>>(values: A) -> IslConstraint {
        IslConstraint::new(IslVersion::V1_0, IslConstraintImpl::Contains(values.into()))
    }

    /// Creates a `container_length` constraint using the range specified in it
    pub fn container_length(length: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::ContainerLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates a `byte_length` constraint using the range specified in it
    pub fn byte_length(length: RangeImpl<usize>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::ByteLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates a `codepoint_length` constraint using the range specified in it
    pub fn codepoint_length(length: RangeImpl<usize>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::CodepointLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates a `timestamp_precision` constraint using the range specified in it
    pub fn timestamp_precision(precision: RangeImpl<TimestampPrecision>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::TimestampPrecision(Range::TimestampPrecision(precision)),
        )
    }

    /// Creates a `timestamp_offset` constraint using the offset list specified in it
    pub fn timestamp_offset(offsets: Vec<TimestampOffset>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::TimestampOffset(IslTimestampOffsetConstraint::new(offsets)),
        )
    }

    /// Creates an `utf_byte_length` constraint using the range specified in it
    pub fn utf8_byte_length(length: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Utf8ByteLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates an `element` constraint using the [IslTypeRef] referenced inside it
    pub fn element(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Element(isl_type.type_reference, false),
        )
    }

    /// Creates an `annotations` constraint using [str]s and [Element]s specified inside it
    pub fn annotations<'a, A: IntoIterator<Item = &'a str>, B: IntoIterator<Item = Element>>(
        annotations_modifiers: A,
        annotations: B,
    ) -> IslConstraint {
        let annotations_modifiers: Vec<&str> = annotations_modifiers.into_iter().collect();
        let annotations: Vec<Annotation> = annotations
            .into_iter()
            .map(|a| {
                Annotation::new(
                    a.as_text().unwrap().to_owned(),
                    Annotation::is_annotation_required(
                        &a,
                        annotations_modifiers.contains(&"required"),
                    ),
                )
            })
            .collect();
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Annotations(IslAnnotationsConstraint::SimpleAnnotations(
                IslSimpleAnnotationsConstraint::new(
                    annotations_modifiers.contains(&"closed"),
                    annotations_modifiers.contains(&"ordered"),
                    annotations_modifiers.contains(&"required"),
                    annotations,
                ),
            )),
        )
    }

    /// Creates a `valid_values` constraint using the [Element]s specified inside it
    pub fn valid_values_with_values(values: Vec<Element>) -> IonSchemaResult<IslConstraint> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> = values
            .iter()
            .map(|e| ValidValue::from_ion_element(e, IslVersion::V1_0))
            .collect();
        Ok(IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::ValidValues(IslValidValuesConstraint {
                valid_values: valid_values?,
            }),
        ))
    }

    /// Creates a `valid_values` constraint using the [Range] specified inside it
    pub fn valid_values_with_range(range: Range) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::ValidValues(IslValidValuesConstraint {
                valid_values: vec![ValidValue::Range(range)],
            }),
        )
    }

    /// Creates a `regex` constraint using the expression and flags (case_insensitive, multi_line)
    pub fn regex(case_insensitive: bool, multi_line: bool, expression: String) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V1_0,
            IslConstraintImpl::Regex(IslRegexConstraint::new(
                case_insensitive,
                multi_line,
                expression,
            )),
        )
    }
}

/// Provides public facing APIs for constructing ISL constraints programmatically for ISL 2.0
pub mod v_2_0 {
    use crate::isl::isl_constraint::{
        IslAnnotationsConstraint, IslConstraint, IslSimpleAnnotationsConstraint,
    };
    use crate::isl::isl_constraint::{
        IslConstraintImpl, IslTimestampOffsetConstraint, IslValidValuesConstraint,
    };
    use crate::isl::isl_range::{NonNegativeIntegerRange, Range, RangeImpl};
    use crate::isl::isl_type_reference::{IslTypeRef, IslVariablyOccurringTypeRef};
    use crate::isl::util::{
        Annotation, Ieee754InterchangeFormat, TimestampOffset, TimestampPrecision, ValidValue,
    };
    use crate::isl::IslVersion;
    use crate::result::{invalid_schema_error, IonSchemaResult};
    use ion_rs::element::Element;
    use ion_rs::Int;

    /// Creates a `type` constraint using the [IslTypeRef] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Type(isl_type.type_reference),
        )
    }

    /// Creates an `all_of` constraint using the [IslTypeRef] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::AllOf(
                isl_types
                    .into()
                    .into_iter()
                    .map(|t| t.type_reference)
                    .collect(),
            ),
        )
    }

    /// Creates an `any_of` constraint using the [IslTypeRef] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::AnyOf(
                isl_types
                    .into()
                    .into_iter()
                    .map(|t| t.type_reference)
                    .collect(),
            ),
        )
    }

    /// Creates an `one_of` constraint using the [IslTypeRef] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeRef>>>(isl_types: A) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::OneOf(
                isl_types
                    .into()
                    .into_iter()
                    .map(|t| t.type_reference)
                    .collect(),
            ),
        )
    }

    /// Creates an `ordered_elements` constraint using the [IslVariablyOccurringTypeRef] referenced inside it
    pub fn ordered_elements<A: Into<Vec<IslVariablyOccurringTypeRef>>>(
        isl_types: A,
    ) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::OrderedElements(isl_types.into().into_iter().collect()),
        )
    }

    /// Creates a `precision` constraint using the range specified in it
    pub fn precision(precision: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Precision(Range::NonNegativeInteger(precision)),
        )
    }

    /// Creates an `exponent` constraint from a [Range] specifying an exponent range.
    pub fn exponent(exponent: RangeImpl<Int>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Exponent(Range::Integer(exponent)),
        )
    }

    /// Creates a `fields` constraint using the field names and [IslVariablyOccurringTypeRef]s referenced inside it
    pub fn fields<I>(fields: I) -> IslConstraint
    where
        I: Iterator<Item = (String, IslVariablyOccurringTypeRef)>,
    {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Fields(fields.map(|(s, t)| (s, t)).collect(), false),
        )
    }

    /// Creates a `field_names` constraint using the [IslTypeRef] referenced inside it and considers whether distinct elements are required or not
    pub fn field_names(isl_type: IslTypeRef, require_distinct_field_names: bool) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::FieldNames(isl_type.type_reference, require_distinct_field_names),
        )
    }

    /// Creates a `not` constraint using the [IslTypeRef] referenced inside it
    pub fn not(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Not(isl_type.type_reference),
        )
    }

    /// Creates a `contains` constraint using the [Element] specified inside it
    pub fn contains<A: Into<Vec<Element>>>(values: A) -> IslConstraint {
        IslConstraint::new(IslVersion::V2_0, IslConstraintImpl::Contains(values.into()))
    }

    /// Creates a `container_length` constraint using the range specified in it
    pub fn container_length(length: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::ContainerLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates a `byte_length` constraint using the range specified in it
    pub fn byte_length(length: RangeImpl<usize>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::ByteLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates a `codepoint_length` constraint using the range specified in it
    pub fn codepoint_length(length: RangeImpl<usize>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::CodepointLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates a `timestamp_precision` constraint using the range specified in it
    pub fn timestamp_precision(precision: RangeImpl<TimestampPrecision>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::TimestampPrecision(Range::TimestampPrecision(precision)),
        )
    }

    /// Creates a `timestamp_offset` constraint using the offset list specified in it
    pub fn timestamp_offset(offsets: Vec<TimestampOffset>) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::TimestampOffset(IslTimestampOffsetConstraint::new(offsets)),
        )
    }

    /// Creates a `utf8_byte_length` constraint using the range specified in it
    pub fn utf8_byte_length(length: NonNegativeIntegerRange) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Utf8ByteLength(Range::NonNegativeInteger(length)),
        )
    }

    /// Creates an `element` constraint using the [IslTypeRef] referenced inside it and considers whether distinct elements are required or not
    pub fn element(isl_type: IslTypeRef, require_distinct_elements: bool) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Element(isl_type.type_reference, require_distinct_elements),
        )
    }

    /// Creates an `annotations` constraint using a list of valid annotations and specify whether annotations are required or closed or both.
    /// If both `is_required` and `is_closed` are false then this returns an error as ISL 2.0 requires that either annotations are closed or required or both.
    pub fn annotations_simplified<A: IntoIterator<Item = Element>>(
        is_required: bool,
        is_closed: bool,
        annotations: A,
    ) -> IonSchemaResult<IslConstraint> {
        let annotations: Vec<Annotation> = annotations
            .into_iter()
            .map(|a| {
                Annotation::new(
                    a.as_text().unwrap().to_owned(),
                    Annotation::is_annotation_required(&a, is_required),
                )
            })
            .collect();

        if !is_required && !is_closed {
            return invalid_schema_error(
                "annotations constraints must either be required or closed or both.",
            );
        }

        Ok(IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Annotations(IslAnnotationsConstraint::SimpleAnnotations(
                IslSimpleAnnotationsConstraint::new(is_closed, false, is_required, annotations),
            )),
        ))
    }

    /// Creates an `annotations` constraint using an [IslTypeRef].
    pub fn annotations(isl_type: IslTypeRef) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Annotations(IslAnnotationsConstraint::StandardAnnotations(
                isl_type.type_reference,
            )),
        )
    }

    /// Creates a `valid_values` constraint using the [Element]s specified inside it
    pub fn valid_values_with_values(values: Vec<Element>) -> IonSchemaResult<IslConstraint> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> = values
            .iter()
            .map(|e| ValidValue::from_ion_element(e, IslVersion::V2_0))
            .collect();
        Ok(IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::ValidValues(IslValidValuesConstraint {
                valid_values: valid_values?,
            }),
        ))
    }

    /// Creates a `valid_values` constraint using the [Range] specified inside it
    pub fn valid_values_with_range(range: Range) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::ValidValues(IslValidValuesConstraint {
                valid_values: vec![ValidValue::Range(range)],
            }),
        )
    }

    /// Creates a `regex` constraint using the expression and flags (case_insensitive, multi_line)
    pub fn regex(case_insensitive: bool, multi_line: bool, expression: String) -> IslConstraint {
        todo!()
    }

    /// Creates a `ieee754_float` constraint using `Ieee754InterchangeFormat` specified in it.
    pub fn ieee754_float(interchange_format: Ieee754InterchangeFormat) -> IslConstraint {
        IslConstraint::new(
            IslVersion::V2_0,
            IslConstraintImpl::Ieee754Float(interchange_format),
        )
    }
}

/// Represents schema constraints [IslConstraint]
#[derive(Debug, Clone, PartialEq)]
pub struct IslConstraint {
    pub(crate) version: IslVersion,
    pub(crate) constraint: IslConstraintImpl,
}

impl IslConstraint {
    pub(crate) fn new(version: IslVersion, constraint: IslConstraintImpl) -> Self {
        Self {
            constraint,
            version,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IslConstraintImpl {
    AllOf(Vec<IslTypeRefImpl>),
    Annotations(IslAnnotationsConstraint),
    AnyOf(Vec<IslTypeRefImpl>),
    ByteLength(Range),
    CodepointLength(Range),
    Contains(Vec<Element>),
    ContentClosed,
    ContainerLength(Range),
    // Represents Element(type_reference, expected_distinct).
    // For ISL 2.0 true/false is specified based on whether `distinct` annotation is present or not.
    // For ISL 1.0 which doesn't support `distinct` elements this will be (type_reference, false).
    Element(IslTypeRefImpl, bool),
    Exponent(Range),
    // Represents Fields(fields, content_closed)
    // For ISL 2.0 true/false is specified based on whether `closed::` annotation is present or not
    // For ISL 1.0 this will always be (fields, false) as it doesn't support `closed::` annotation on fields constraint
    Fields(HashMap<String, IslVariablyOccurringTypeRef>, bool),
    // Represents FieldNames(type_reference, expected_distinct).
    // For ISL 2.0 true/false is specified based on whether `distinct` annotation is present or not.
    // For ISL 1.0 which doesn't support `field_names` constraint this will be (type_reference, false).
    FieldNames(IslTypeRefImpl, bool),
    Ieee754Float(Ieee754InterchangeFormat),
    Not(IslTypeRefImpl),
    OneOf(Vec<IslTypeRefImpl>),
    OrderedElements(Vec<IslVariablyOccurringTypeRef>),
    Precision(Range),
    Regex(IslRegexConstraint),
    Scale(Range),
    TimestampOffset(IslTimestampOffsetConstraint),
    TimestampPrecision(Range),
    Type(IslTypeRefImpl),
    Unknown(String, Element), // Unknown constraint is used to store open contents
    Utf8ByteLength(Range),
    ValidValues(IslValidValuesConstraint),
}

impl IslConstraintImpl {
    /// Parse constraints inside an [Element] to an [IslConstraint]
    pub fn from_ion_element(
        isl_version: IslVersion,
        constraint_name: &str,
        value: &Element,
        type_name: &str,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<IslConstraintImpl> {
        // TODO: add more constraints to match below
        match constraint_name {
            "all_of" => {
                let types: Vec<IslTypeRefImpl> =
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
                    return invalid_schema_error(
                        "annotations constraint was a null instead of a list",
                    );
                }

                if value.ion_type() == IonType::List {
                    Ok(IslConstraintImpl::Annotations(
                        IslAnnotationsConstraint::SimpleAnnotations(
                            IslSimpleAnnotationsConstraint::from_ion_element(value, isl_version)?,
                        ),
                    ))
                } else if value.ion_type() == IonType::Struct && isl_version == IslVersion::V2_0 {
                    let type_reference: IslTypeRefImpl = IslTypeRefImpl::from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                    )?;

                    Ok(IslConstraintImpl::Annotations(
                        IslAnnotationsConstraint::StandardAnnotations(type_reference),
                    ))
                } else {
                    return invalid_schema_error(format!(
                        "annotations constraint was a {:?} instead of a list",
                        value.ion_type()
                    ));
                }
            }
            "any_of" => {
                let types: Vec<IslTypeRefImpl> =
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
                isl_version,
            )?)),
            "codepoint_length" => Ok(IslConstraintImpl::CodepointLength(Range::from_ion_element(
                value,
                RangeType::NonNegativeInteger,
                isl_version,
            )?)),
            "contains" => {
                if value.is_null() {
                    return invalid_schema_error(
                        "contains constraint was a null instead of a list",
                    );
                }

                if value.ion_type() != IonType::List {
                    return invalid_schema_error(format!(
                        "contains constraint was a {:?} instead of a list",
                        value.ion_type()
                    ));
                }

                let values: Vec<Element> = value
                    .as_sequence()
                    .unwrap()
                    .elements()
                    .map(|e| e.to_owned())
                    .collect();
                Ok(IslConstraintImpl::Contains(values))
            }
            "content" => {
                if value.is_null() {
                    return invalid_schema_error(
                        "content constraint was a null instead of a symbol `closed`",
                    );
                }

                if value.ion_type() != IonType::Symbol {
                    return invalid_schema_error(format!(
                        "content constraint was a {:?} instead of a symbol `closed`",
                        value.ion_type()
                    ));
                }

                if let Some(closed) = value.as_text() {
                    if closed != "closed" {
                        return invalid_schema_error(format!(
                            "content constraint was a {closed} instead of a symbol `closed`"
                        ));
                    }
                }

                Ok(IslConstraintImpl::ContentClosed)
            }

            "container_length" => Ok(IslConstraintImpl::ContainerLength(Range::from_ion_element(
                value,
                RangeType::NonNegativeInteger,
                isl_version,
            )?)),
            "element" => {
                let type_reference: IslTypeRefImpl =
                    IslTypeRefImpl::from_ion_element(isl_version, value, inline_imported_types)?;
                match isl_version {
                    IslVersion::V1_0 => {
                        // for ISL 1.0 `distinct annotation on `element` constraint is not supported which is represented by `false` here
                        Ok(IslConstraintImpl::Element(type_reference, false))
                    }
                    IslVersion::V2_0 => {
                        // return error if there are any annotations other than `distinct` or `$null_or`
                        if value
                            .annotations()
                            .iter()
                            .any(|a| a.text() != Some("distinct") && a.text() != Some("$null_or"))
                        {
                            return invalid_schema_error(
                                "element constraint can only contain `distinct` annotation",
                            );
                        }

                        // verify whether `distinct`annotation is present or not
                        let require_distinct = value.annotations().contains("distinct");

                        Ok(IslConstraintImpl::Element(type_reference, require_distinct))
                    }
                }
            }
            "field_names" => {
                let type_reference =
                    IslTypeRefImpl::from_ion_element(isl_version, value, inline_imported_types)?;
                match isl_version {
                    IslVersion::V1_0 => {
                        // for ISL 1.0 `field_names` constraint does not exist hence `field_names` will be considered as open content
                        Ok(IslConstraintImpl::Unknown(
                            constraint_name.to_string(),
                            value.to_owned(),
                        ))
                    }
                    IslVersion::V2_0 => {
                        // return error if there are any annotations other than `distinct`
                        if value.annotations().len() > 1
                            || value
                                .annotations()
                                .iter()
                                .any(|a| a.text() != Some("distinct"))
                        {
                            return invalid_schema_error(
                                "field_names constraint can only contain `distinct` annotation",
                            );
                        }

                        Ok(IslConstraintImpl::FieldNames(
                            type_reference,
                            value.annotations().contains("distinct"),
                        ))
                    }
                }
            }
            "fields" => {
                let fields: HashMap<String, IslVariablyOccurringTypeRef> =
                    IslConstraintImpl::isl_fields_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                    )?;

                if fields.is_empty() {
                    return invalid_schema_error("fields constraint can not be empty");
                }
                match isl_version {
                    IslVersion::V1_0 => Ok(IslConstraintImpl::Fields(fields, false)),
                    IslVersion::V2_0 => {
                        if value.annotations().len() > 1
                            || value
                                .annotations()
                                .iter()
                                .any(|a| a.text() != Some("closed"))
                        {
                            return invalid_schema_error(
                                "fields constraint may only be annotated with 'closed'",
                            );
                        }
                        Ok(IslConstraintImpl::Fields(
                            fields,
                            value.annotations().contains("closed"),
                        ))
                    }
                }
            }
            "ieee754_float" => {
                if !value.annotations().is_empty() {
                    return invalid_schema_error(
                        "`ieee_754_float` argument must not have annotations",
                    );
                }
                let string_value =
                    value
                        .as_symbol()
                        .map(|s| s.text().unwrap())
                        .ok_or_else(|| {
                            invalid_schema_error_raw(format!(
                                "expected ieee754_float to be one of 'binary16', 'binary32', or 'binary64', but it was: {value}"))
                        })?;
                Ok(IslConstraintImpl::Ieee754Float(string_value.try_into()?))
            }
            "one_of" => {
                let types: Vec<IslTypeRefImpl> =
                    IslConstraintImpl::isl_type_references_from_ion_element(
                        isl_version,
                        value,
                        inline_imported_types,
                        "one_of",
                    )?;
                Ok(IslConstraintImpl::OneOf(types))
            }
            "not" => {
                let type_reference: IslTypeRefImpl =
                    IslTypeRefImpl::from_ion_element(isl_version, value, inline_imported_types)?;
                Ok(IslConstraintImpl::Not(type_reference))
            }
            "type" => {
                let type_reference: IslTypeRefImpl =
                    IslTypeRefImpl::from_ion_element(isl_version, value, inline_imported_types)?;
                Ok(IslConstraintImpl::Type(type_reference))
            }
            "ordered_elements" => {
                if value.is_null() {
                    return invalid_schema_error(
                        "ordered_elements constraint was a null instead of a list",
                    );
                }
                if value.ion_type() != IonType::List {
                    return invalid_schema_error(format!(
                        "ordered_elements constraint was a {:?} instead of a list",
                        value.ion_type()
                    ));
                }

                let types: Vec<IslVariablyOccurringTypeRef> = value
                    .as_sequence()
                    .unwrap()
                    .elements()
                    .map(|e| {
                        IslVariablyOccurringTypeRef::from_ion_element(
                            constraint_name,
                            isl_version,
                            e,
                            inline_imported_types,
                        )
                    })
                    .collect::<IonSchemaResult<Vec<IslVariablyOccurringTypeRef>>>()?;
                Ok(IslConstraintImpl::OrderedElements(types))
            }
            "precision" => Ok(IslConstraintImpl::Precision(Range::from_ion_element(
                value,
                RangeType::Precision,
                isl_version,
            )?)),
            "regex" => {
                let case_insensitive = value.annotations().contains("i");
                let multi_line = value.annotations().contains("m");

                if value
                    .annotations()
                    .iter()
                    .any(|a| a.text().unwrap() != "i" && a.text().unwrap() != "m")
                {
                    return invalid_schema_error(
                        "regex constraint must only contain 'i' or 'm' annotation",
                    );
                }

                let expression = value.as_string().ok_or_else(|| {
                    invalid_schema_error_raw(format!(
                        "expected regex to contain a string expression but found: {}",
                        value.ion_type()
                    ))
                })?;

                if expression.is_empty() {
                    return invalid_schema_error(
                        "regex constraint must contain a non empty expression",
                    );
                }

                Ok(IslConstraintImpl::Regex(IslRegexConstraint::new(
                    case_insensitive,
                    multi_line,
                    expression.to_string(),
                )))
            }
            "scale" => match isl_version {
                IslVersion::V1_0 => Ok(IslConstraintImpl::Scale(Range::from_ion_element(
                    value,
                    RangeType::Any,
                    isl_version,
                )?)),
                IslVersion::V2_0 => {
                    // for ISL 2.0 scale constraint does not exist hence `scale` will be considered as open content
                    Ok(IslConstraintImpl::Unknown(
                        constraint_name.to_string(),
                        value.to_owned(),
                    ))
                }
            },
            "timestamp_precision" => Ok(IslConstraintImpl::TimestampPrecision(
                Range::from_ion_element(value, RangeType::TimestampPrecision, isl_version)?,
            )),
            "exponent" => match isl_version {
                IslVersion::V1_0 => {
                    // for ISL 1.0 exponent constraint does not exist hence `exponent` will be considered as open content
                    Ok(IslConstraintImpl::Unknown(
                        constraint_name.to_string(),
                        value.to_owned(),
                    ))
                }
                IslVersion::V2_0 => Ok(IslConstraintImpl::Exponent(Range::from_ion_element(
                    value,
                    RangeType::Any,
                    isl_version,
                )?)),
            },
            "timestamp_offset" => {
                use IonType::*;
                if value.is_null() {
                    return invalid_schema_error(
                        "expected a list of valid offsets for an `timestamp_offset` constraint, found null",
                    );
                }

                if !value.annotations().is_empty() {
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
                            .elements()
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

                                if !e.annotations().is_empty() {
                                    return invalid_schema_error(format!(
                                        "`timestamp_offset` values may not be annotated, found {e}"
                                    ));
                                }

                                // unwrap here will not panic as we have already verified the ion type to be a string
                                let string_value = e.as_string().unwrap();

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
                isl_version,
            )?)),
            "valid_values" => Ok(IslConstraintImpl::ValidValues(
                IslValidValuesConstraint::from_ion_element(value, isl_version)?,
            )),
            _ => Ok(IslConstraintImpl::Unknown(
                constraint_name.to_string(),
                value.to_owned(),
            )),
        }
    }

    // helper method for from_ion_element to get isl type references from given ion element
    fn isl_type_references_from_ion_element(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
        constraint_name: &str,
    ) -> IonSchemaResult<Vec<IslTypeRefImpl>> {
        //TODO: create a method/macro for this ion type check which can be reused
        if value.is_null() {
            return invalid_schema_error(format!(
                "{constraint_name} constraint was a null instead of a list"
            ));
        }
        if value.ion_type() != IonType::List {
            return invalid_schema_error(format!(
                "{} constraint was a {:?} instead of a list",
                constraint_name,
                value.ion_type()
            ));
        }
        value
            .as_sequence()
            .unwrap()
            .elements()
            .map(|e| IslTypeRefImpl::from_ion_element(isl_version, e, inline_imported_types))
            .collect::<IonSchemaResult<Vec<IslTypeRefImpl>>>()
    }

    // helper method for from_ion_element to get isl fields from given ion element
    fn isl_fields_from_ion_element(
        isl_version: IslVersion,
        value: &Element,
        inline_imported_types: &mut Vec<IslImportType>,
    ) -> IonSchemaResult<HashMap<String, IslVariablyOccurringTypeRef>> {
        if value.is_null() {
            return invalid_schema_error("fields constraint was a null instead of a struct");
        }

        if value.ion_type() != IonType::Struct {
            return invalid_schema_error(format!(
                "fields constraint was a {:?} instead of a struct",
                value.ion_type()
            ));
        }

        let fields_map = value
            .as_struct()
            .unwrap()
            .iter()
            .map(|(f, v)| {
                IslVariablyOccurringTypeRef::from_ion_element(
                    "fields",
                    isl_version,
                    v,
                    inline_imported_types,
                )
                .map(|t| (f.text().unwrap().to_owned(), t))
            })
            .collect::<IonSchemaResult<HashMap<String, IslVariablyOccurringTypeRef>>>()?;

        // verify the map length with struct length to check for duplicates
        if fields_map.len() < value.as_struct().unwrap().len() {
            return invalid_schema_error("fields must be a struct with no repeated field names");
        }

        Ok(fields_map)
    }
}

/// Represents the [annotations] constraint
///
/// [annotations]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum IslAnnotationsConstraint {
    SimpleAnnotations(IslSimpleAnnotationsConstraint),
    StandardAnnotations(IslTypeRefImpl),
}

/// Represents the [simple syntax] for annotations constraint
/// ```ion
/// Grammar: <ANNOTATIONS> ::= annotations: <ANNOTATIONS_MODIFIER>... [ <SYMBOL>... ]
///          <ANNOTATIONS_MODIFIER> ::= required::
///                                   | closed::
/// ```
///
/// [simple syntax]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#simple-syntax
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IslSimpleAnnotationsConstraint {
    pub is_closed: bool,
    pub is_ordered: bool,
    pub is_required: bool,
    pub annotations: Vec<Annotation>,
}

impl IslSimpleAnnotationsConstraint {
    pub fn new(
        is_closed: bool,
        is_ordered: bool,
        is_required: bool,
        annotations: Vec<Annotation>,
    ) -> Self {
        Self {
            is_closed,
            is_ordered,
            is_required,
            annotations,
        }
    }

    pub(crate) fn from_ion_element(
        value: &Element,
        isl_version: IslVersion,
    ) -> IonSchemaResult<Self> {
        let annotation_modifiers: Vec<&str> = value
            .annotations()
            .iter()
            .map(|sym| sym.text().unwrap())
            .collect();

        if (annotation_modifiers
            .iter()
            .any(|a| a != &"closed" && a != &"required")
            || annotation_modifiers.is_empty())
            && isl_version == IslVersion::V2_0
        {
            return invalid_schema_error(
                "annotations constraint must only be annotated with 'required' or 'closed' annotation",
            );
        }

        let annotations: Vec<Annotation> = value
            .as_sequence()
            .unwrap()
            .elements()
            .map(|e| {
                if !e.annotations().is_empty() && isl_version == IslVersion::V2_0 {
                    return invalid_schema_error(
                        "annotations constraint must only contain symbols without any annotations",
                    );
                }
                e.as_symbol()
                    .map(|s| {
                        Annotation::new(
                            s.text().unwrap().to_owned(),
                            Annotation::is_annotation_required(
                                e,
                                annotation_modifiers.contains(&"required"),
                            ),
                        )
                    })
                    .ok_or(invalid_schema_error_raw(
                        "annotations constraint must only contain symbols",
                    ))
            })
            .collect::<IonSchemaResult<Vec<Annotation>>>()?;

        Ok(IslSimpleAnnotationsConstraint::new(
            annotation_modifiers.contains(&"closed"),
            annotation_modifiers.contains(&"ordered"),
            annotation_modifiers.contains(&"required"),
            annotations,
        ))
    }

    pub(crate) fn convert_to_type_reference(&self) -> IonSchemaResult<IslTypeRefImpl> {
        let mut isl_constraints = vec![];
        if self.is_closed {
            isl_constraints.push(isl::isl_constraint::v_2_0::element(
                isl::isl_type_reference::v_2_0::anonymous_type_ref(vec![
                    isl::isl_constraint::v_2_0::valid_values_with_values(
                        self.annotations
                            .iter()
                            .map(|a| Element::symbol(a.value()))
                            .collect(),
                    )?,
                ]),
                false,
            ));
        }

        if self.is_required {
            isl_constraints.push(isl::isl_constraint::v_2_0::contains(
                self.annotations
                    .iter()
                    .map(|a| Element::symbol(a.value()))
                    .collect::<Vec<Element>>(),
            ))
        }

        Ok(isl::isl_type_reference::v_2_0::anonymous_type_ref(isl_constraints).type_reference)
    }
}

/// Represents the [valid_values] constraint
///
/// [valid_values]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations
#[derive(Debug, Clone, PartialEq)]
pub struct IslValidValuesConstraint {
    pub(crate) valid_values: Vec<ValidValue>,
}

impl IslValidValuesConstraint {
    /// Provides a way to programmatically construct valid_values constraint
    /// Returns IonSchemaError whenever annotations are provided within ValidValue::Element
    /// only `range` annotations are accepted for ValidValue::Element
    pub fn new(valid_values: Vec<ValidValue>, isl_version: IslVersion) -> IonSchemaResult<Self> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> = valid_values
            .iter()
            .map(|v| match v {
                ValidValue::Range(r) => Ok(v.to_owned()),
                ValidValue::Element(e) => ValidValue::from_ion_element(e, isl_version),
            })
            .collect();
        Ok(Self {
            valid_values: valid_values?,
        })
    }

    pub fn values(&self) -> &Vec<ValidValue> {
        &self.valid_values
    }

    pub fn from_ion_element(value: &Element, isl_version: IslVersion) -> IonSchemaResult<Self> {
        if value.annotations().contains("range") {
            return IslValidValuesConstraint::new(
                vec![ValidValue::Range(Range::from_ion_element(
                    value,
                    RangeType::NumberOrTimestamp,
                    isl_version,
                )?)],
                isl_version,
            );
        }
        if let Some(values) = value.as_sequence() {
            if value.ion_type() == IonType::List {
                let mut valid_values = vec![];
                let values: IonSchemaResult<Vec<()>> = values
                    .elements()
                    .map(|e| {
                        valid_values.push(ValidValue::from_ion_element(e, isl_version)?);
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

/// Represents the [regex] constraint
///
/// [regex]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#regex
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

/// Represents the [timestamp_offset] constraint
///
/// [timestamp_offset]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_offset
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
