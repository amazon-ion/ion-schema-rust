use crate::ion_path::{IonPath, IonPathElement};
use crate::isl::isl_constraint::{IslConstraint, IslRegexConstraint};
use crate::isl::isl_range::{Range, RangeImpl};
use crate::isl::isl_type_reference::IslTypeRef;
use crate::isl::util::{Annotation, TimestampOffset, TimestampPrecision, ValidValue};
use crate::nfa::{FinalState, NfaBuilder, NfaEvaluation};
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, IonSchemaError, IonSchemaResult,
    ValidationResult,
};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::TypeValidator;
use crate::violation::{Violation, ViolationCode};
use crate::IonSchemaElement;
use ion_rs::value::owned::Element;
use ion_rs::value::{IonElement, IonSequence, IonStruct};
use ion_rs::{Integer, IonType};
use regex::{Regex, RegexBuilder};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::rc::Rc;
use std::str::Chars;

/// Provides validation for schema Constraint
pub trait ConstraintValidator {
    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to `Err(violation)`
    /// if the constraint is violated.
    /// Otherwise, if the value passes the validation against the constraint then returns `Ok(())`.
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult;
}

/// Defines schema Constraints
#[derive(Debug, Clone, PartialEq)]
// TODO: add other constraints
pub enum Constraint {
    AllOf(AllOfConstraint),
    Annotations(AnnotationsConstraint),
    AnyOf(AnyOfConstraint),
    ByteLength(ByteLengthConstraint),
    CodepointLength(CodepointLengthConstraint),
    Contains(ContainsConstraint),
    ContentClosed,
    ContainerLength(ContainerLengthConstraint),
    Element(ElementConstraint),
    Fields(FieldsConstraint),
    Not(NotConstraint),
    OneOf(OneOfConstraint),
    OrderedElements(OrderedElementsConstraint),
    Occurs(OccursConstraint),
    Precision(PrecisionConstraint),
    Regex(RegexConstraint),
    Scale(ScaleConstraint),
    TimestampOffset(TimestampOffsetConstraint),
    TimestampPrecision(TimestampPrecisionConstraint),
    Type(TypeConstraint),
    Unknown(String, Element),
    Utf8ByteLength(Utf8ByteLengthConstraint),
    ValidValues(ValidValuesConstraint),
}

impl Constraint {
    /// Creates a [Constraint::Type] referring to the type represented by the provided [TypeId].
    pub fn type_constraint(type_id: TypeId) -> Constraint {
        Constraint::Type(TypeConstraint::new(type_id))
    }

    /// Creates a [Constraint::AllOf] referring to the types represented by the provided [TypeId]s.
    pub fn all_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::AllOf(AllOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::AnyOf] referring to the types represented by the provided [TypeId]s.
    pub fn any_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::AnyOf(AnyOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::OneOf] referring to the types represented by the provided [TypeId]s.
    pub fn one_of<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::OneOf(OneOfConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::Not] referring to the type represented by the provided [TypeId].
    pub fn not(type_id: TypeId) -> Constraint {
        Constraint::Not(NotConstraint::new(type_id))
    }

    /// Creates a [Constraint::OrderedElements] referring to the types represented by the provided [TypeId]s.
    pub fn ordered_elements<A: Into<Vec<TypeId>>>(type_ids: A) -> Constraint {
        Constraint::OrderedElements(OrderedElementsConstraint::new(type_ids.into()))
    }

    /// Creates a [Constraint::Contains] referring to [Elements] specified inside it
    pub fn contains<A: Into<Vec<Element>>>(values: A) -> Constraint {
        Constraint::Contains(ContainsConstraint::new(values.into()))
    }

    /// Creates a [Constraint::ContainerLength] from a [Range] specifying a length range.
    pub fn container_length(length: RangeImpl<usize>) -> Constraint {
        Constraint::ContainerLength(ContainerLengthConstraint::new(Range::NonNegativeInteger(
            length,
        )))
    }

    /// Creates a [Constraint::ByteLength] from a [Range] specifying a length range.
    pub fn byte_length(length: RangeImpl<usize>) -> Constraint {
        Constraint::ByteLength(ByteLengthConstraint::new(Range::NonNegativeInteger(length)))
    }

    /// Creates a [Constraint::CodePointLength] from a [Range] specifying a length range.
    pub fn codepoint_length(length: RangeImpl<usize>) -> Constraint {
        Constraint::CodepointLength(CodepointLengthConstraint::new(Range::NonNegativeInteger(
            length,
        )))
    }

    /// Creates a [Constraint::Element] referring to the type represented by the provided [TypeId].
    pub fn element(type_id: TypeId) -> Constraint {
        Constraint::Element(ElementConstraint::new(type_id))
    }

    /// Creates a [Constraint::Annotations] using [str]s and [Element]s specified inside it
    pub fn annotations<'a, A: IntoIterator<Item = &'a str>, B: IntoIterator<Item = Element>>(
        annotations_modifiers: A,
        annotations: B,
    ) -> Constraint {
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
        Constraint::Annotations(AnnotationsConstraint::new(
            annotations_modifiers.contains(&"closed"),
            annotations_modifiers.contains(&"ordered"),
            annotations,
        ))
    }

    /// Creates a [Constraint::Precision] from a [Range] specifying a precision range.
    pub fn precision(precision: RangeImpl<usize>) -> Constraint {
        Constraint::Precision(PrecisionConstraint::new(Range::NonNegativeInteger(
            precision,
        )))
    }

    /// Creates a [Constraint::Scale] from a [Range] specifying a precision range.
    pub fn scale(scale: RangeImpl<Integer>) -> Constraint {
        Constraint::Scale(ScaleConstraint::new(Range::Integer(scale)))
    }

    /// Creates a [Constraint::TimestampPrecision] from a [Range] specifying a precision range.
    pub fn timestamp_precision(precision: RangeImpl<TimestampPrecision>) -> Constraint {
        Constraint::TimestampPrecision(TimestampPrecisionConstraint::new(
            Range::TimestampPrecision(precision),
        ))
    }

    /// Creates an [Constraint::TimestampOffset] using the offset list specified in it
    pub fn timestamp_offset(offsets: Vec<TimestampOffset>) -> Constraint {
        Constraint::TimestampOffset(TimestampOffsetConstraint::new(offsets))
    }

    /// Creates a [Constraint::Utf8ByteLength] from a [Range] specifying a length range.
    pub fn utf8_byte_length(length: RangeImpl<usize>) -> Constraint {
        Constraint::Utf8ByteLength(Utf8ByteLengthConstraint::new(Range::NonNegativeInteger(
            length,
        )))
    }

    /// Creates a [Constraint::Fields] referring to the fields represented by the provided field name and [TypeId]s.
    /// By default, fields created using this method will allow open content
    pub fn fields<I>(fields: I) -> Constraint
    where
        I: Iterator<Item = (String, TypeId)>,
    {
        Constraint::Fields(FieldsConstraint::new(fields.collect(), true))
    }

    /// Creates a [Constraint::ValidValues] using the [Element]s specified inside it
    /// Returns an IonSchemaError if any of the Elements have an annotation other than `range`
    pub fn valid_values_with_values(values: Vec<Element>) -> IonSchemaResult<Constraint> {
        let valid_values: IonSchemaResult<Vec<ValidValue>> =
            values.iter().map(|e| e.try_into()).collect();
        Ok(Constraint::ValidValues(ValidValuesConstraint {
            valid_values: valid_values?,
        }))
    }

    /// Creates a [Constraint::ValidValues] using the [Range] specified inside it
    pub fn valid_values_with_range(value: Range) -> Constraint {
        Constraint::ValidValues(ValidValuesConstraint {
            valid_values: vec![ValidValue::Range(value)],
        })
    }

    /// Creates a [Constraint::Regex] from the expression and flags (case_insensitive, multi_line)
    pub fn regex(
        case_insensitive: bool,
        multi_line: bool,
        expression: String,
    ) -> IonSchemaResult<Constraint> {
        let regex = IslRegexConstraint::new(case_insensitive, multi_line, expression);
        Ok(Constraint::Regex(regex.try_into()?))
    }

    /// Parse an [IslConstraint] to a [Constraint]
    pub fn resolve_from_isl_constraint(
        isl_constraint: &IslConstraint,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
        open_content: bool, // this will be used by Fields constraint to verify if open content is allowed or not
    ) -> IonSchemaResult<Constraint> {
        // TODO: add more constraints below
        match isl_constraint {
            IslConstraint::AllOf(type_references) => {
                let all_of: AllOfConstraint = AllOfConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::AllOf(all_of))
            }
            IslConstraint::Annotations(isl_annotations) => {
                Ok(Constraint::Annotations(AnnotationsConstraint::new(
                    isl_annotations.is_closed,
                    isl_annotations.is_ordered,
                    isl_annotations.annotations.to_owned(),
                )))
            }
            IslConstraint::AnyOf(type_references) => {
                let any_of: AnyOfConstraint = AnyOfConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::AnyOf(any_of))
            }
            IslConstraint::ByteLength(byte_length) => Ok(Constraint::ByteLength(
                ByteLengthConstraint::new(byte_length.to_owned()),
            )),
            IslConstraint::CodepointLength(codepoint_length) => Ok(Constraint::CodepointLength(
                CodepointLengthConstraint::new(codepoint_length.to_owned()),
            )),
            IslConstraint::Contains(values) => {
                let contains_constraint: ContainsConstraint =
                    ContainsConstraint::new(values.to_owned());
                Ok(Constraint::Contains(contains_constraint))
            }
            IslConstraint::ContentClosed => Ok(Constraint::ContentClosed),
            IslConstraint::ContainerLength(isl_length) => Ok(Constraint::ContainerLength(
                ContainerLengthConstraint::new(isl_length.to_owned()),
            )),
            IslConstraint::Element(type_reference) => {
                let element_constraint: ElementConstraint =
                    ElementConstraint::resolve_from_isl_constraint(
                        type_reference,
                        type_store,
                        pending_types,
                    )?;
                Ok(Constraint::Element(element_constraint))
            }
            IslConstraint::Fields(fields) => {
                let fields_constraint: FieldsConstraint =
                    FieldsConstraint::resolve_from_isl_constraint(
                        fields,
                        type_store,
                        pending_types,
                        open_content,
                    )?;
                Ok(Constraint::Fields(fields_constraint))
            }
            IslConstraint::OneOf(type_references) => {
                let one_of: OneOfConstraint = OneOfConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::OneOf(one_of))
            }
            IslConstraint::Not(type_references) => {
                let not: NotConstraint = NotConstraint::resolve_from_isl_constraint(
                    type_references,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::Not(not))
            }
            IslConstraint::Type(type_reference) => {
                let type_constraint: TypeConstraint = TypeConstraint::resolve_from_isl_constraint(
                    type_reference,
                    type_store,
                    pending_types,
                )?;
                Ok(Constraint::Type(type_constraint))
            }
            IslConstraint::Occurs(occurs_range) => Ok(Constraint::Occurs(OccursConstraint::new(
                occurs_range.to_owned(),
            ))),
            IslConstraint::OrderedElements(type_references) => {
                let ordered_elements: OrderedElementsConstraint =
                    OrderedElementsConstraint::resolve_from_isl_constraint(
                        type_references,
                        type_store,
                        pending_types,
                    )?;
                Ok(Constraint::OrderedElements(ordered_elements))
            }
            IslConstraint::Precision(precision_range) => Ok(Constraint::Precision(
                PrecisionConstraint::new(precision_range.to_owned()),
            )),
            IslConstraint::Regex(regex) => Ok(Constraint::Regex(regex.to_owned().try_into()?)),
            IslConstraint::Scale(scale_range) => Ok(Constraint::Scale(ScaleConstraint::new(
                scale_range.to_owned(),
            ))),
            IslConstraint::TimestampOffset(timestamp_offset) => Ok(Constraint::TimestampOffset(
                TimestampOffsetConstraint::new(timestamp_offset.valid_offsets().to_vec()),
            )),
            IslConstraint::TimestampPrecision(timestamp_precision_range) => {
                Ok(Constraint::TimestampPrecision(
                    TimestampPrecisionConstraint::new(timestamp_precision_range.to_owned()),
                ))
            }
            IslConstraint::Utf8ByteLength(utf8_byte_length) => Ok(Constraint::Utf8ByteLength(
                Utf8ByteLengthConstraint::new(utf8_byte_length.to_owned()),
            )),
            IslConstraint::ValidValues(valid_values) => {
                Ok(Constraint::ValidValues(ValidValuesConstraint {
                    valid_values: valid_values.values().to_owned(),
                }))
            }
            IslConstraint::Unknown(constraint_name, element) => Ok(Constraint::Unknown(
                constraint_name.to_owned(),
                element.to_owned(),
            )),
        }
    }

    pub fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        match self {
            Constraint::AllOf(all_of) => all_of.validate(value, type_store, ion_path),
            Constraint::Annotations(annotations) => {
                annotations.validate(value, type_store, ion_path)
            }
            Constraint::AnyOf(any_of) => any_of.validate(value, type_store, ion_path),
            Constraint::ByteLength(byte_length) => {
                byte_length.validate(value, type_store, ion_path)
            }
            Constraint::CodepointLength(codepoint_length) => {
                codepoint_length.validate(value, type_store, ion_path)
            }
            Constraint::Contains(contains) => contains.validate(value, type_store, ion_path),
            Constraint::ContentClosed => {
                // No op
                // `content: closed` does not work as a constraint by its own, it needs to be used with other container constraints
                // e.g. `fields`
                // the validation for `content: closed` is done within these other constraints
                Ok(())
            }
            Constraint::ContainerLength(container_length) => {
                container_length.validate(value, type_store, ion_path)
            }
            Constraint::Element(element) => element.validate(value, type_store, ion_path),
            Constraint::Fields(fields) => fields.validate(value, type_store, ion_path),
            Constraint::Not(not) => not.validate(value, type_store, ion_path),
            Constraint::OneOf(one_of) => one_of.validate(value, type_store, ion_path),
            Constraint::Type(type_constraint) => {
                type_constraint.validate(value, type_store, ion_path)
            }
            Constraint::Occurs(occurs) => occurs.validate(value, type_store, ion_path),
            Constraint::OrderedElements(ordered_elements) => {
                ordered_elements.validate(value, type_store, ion_path)
            }
            Constraint::Precision(precision) => precision.validate(value, type_store, ion_path),
            Constraint::Regex(regex) => regex.validate(value, type_store, ion_path),
            Constraint::Scale(scale) => scale.validate(value, type_store, ion_path),
            Constraint::TimestampOffset(timestamp_offset) => {
                timestamp_offset.validate(value, type_store, ion_path)
            }
            Constraint::TimestampPrecision(timestamp_precision) => {
                timestamp_precision.validate(value, type_store, ion_path)
            }
            Constraint::Utf8ByteLength(utf8_byte_length) => {
                utf8_byte_length.validate(value, type_store, ion_path)
            }
            Constraint::ValidValues(valid_values) => {
                valid_values.validate(value, type_store, ion_path)
            }
            Constraint::Unknown(_, _) => {
                // No op
                // `Unknown` represents open content which can be ignored for validation
                Ok(())
            }
        }
    }
}

/// Implements an `all_of` constraint of Ion Schema
/// [all_of]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#all_of
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AllOfConstraint {
    type_ids: Vec<TypeId>,
}

impl AllOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [AllOf] constraint from the given Element
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(AllOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for AllOfConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let mut valid_types = vec![];
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            match type_def.validate(value, type_store, ion_path) {
                Ok(_) => valid_types.push(type_id),
                Err(violation) => violations.push(violation),
            }
        }
        if !violations.is_empty() {
            return Err(Violation::with_violations(
                "all_of",
                ViolationCode::AllTypesNotMatched,
                format!(
                    "value matches {} types, expected {}",
                    valid_types.len(),
                    self.type_ids.len()
                ),
                ion_path,
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements an `any_of` constraint of Ion Schema
/// [any_of]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#any_of
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnyOfConstraint {
    type_ids: Vec<TypeId>,
}

impl AnyOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [AnyOf] constraint from the given Element
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(AnyOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for AnyOfConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let mut valid_types = vec![];
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            match type_def.validate(value, type_store, ion_path) {
                Ok(_) => valid_types.push(type_id),
                Err(violation) => violations.push(violation),
            }
        }
        let total_valid_types = valid_types.len();
        if total_valid_types == 0 {
            return Err(Violation::with_violations(
                "any_of",
                ViolationCode::NoTypesMatched,
                "value matches none of the types",
                ion_path,
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements an `one_of` constraint of Ion Schema
/// [one_of]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#one_of
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OneOfConstraint {
    type_ids: Vec<TypeId>,
}

impl OneOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [OneOf] constraint from the given Element
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(OneOfConstraint::new(resolved_types))
    }
}

impl ConstraintValidator for OneOfConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let mut valid_types = vec![];
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            match type_def.validate(value, type_store, ion_path) {
                Ok(_) => valid_types.push(type_id),
                Err(violation) => violations.push(violation),
            }
        }
        let total_valid_types = valid_types.len();
        match total_valid_types {
            0 => Err(Violation::with_violations(
                "one_of",
                ViolationCode::NoTypesMatched,
                "value matches none of the types",
                ion_path,
                violations,
            )),
            1 => Ok(()),
            _ => Err(Violation::with_violations(
                "one_of",
                ViolationCode::MoreThanOneTypeMatched,
                format!("value matches {total_valid_types} types, expected 1"),
                ion_path,
                violations,
            )),
        }
    }
}

/// Implements a `not` constraint
/// [type]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#not
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NotConstraint {
    type_id: TypeId,
}

impl NotConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create a [Not] constraint from the given Element
    pub fn resolve_from_isl_constraint(
        type_reference: &IslTypeRef,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id =
            IslTypeRef::resolve_type_reference(type_reference, type_store, pending_types)?;
        Ok(NotConstraint::new(type_id))
    }
}

impl ConstraintValidator for NotConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();
        let violation = type_def.validate(value, type_store, ion_path);
        match violation {
            Err(violation) => Ok(()),
            Ok(_) => {
                // if there were no violations for the types then not constraint was unsatisfied
                Err(Violation::new(
                    "not",
                    ViolationCode::TypeMatched,
                    "value unexpectedly matches type",
                    ion_path,
                ))
            }
        }
    }
}

/// Implements a `type` constraint
/// [type]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstraint {
    type_id: TypeId,
}

impl TypeConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create a `type` constraint from the given [Element]
    pub fn resolve_from_isl_constraint(
        type_reference: &IslTypeRef,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id =
            IslTypeRef::resolve_type_reference(type_reference, type_store, pending_types)?;
        Ok(TypeConstraint::new(type_id))
    }
}

impl ConstraintValidator for TypeConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();
        type_def.validate(value, type_store, ion_path)
    }
}

/// Implements an `occurs` constraint of Ion Schema
/// [occurs]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type-definitions
#[derive(Debug, Clone, PartialEq)]
pub struct OccursConstraint {
    occurs_range: Range,
}

impl OccursConstraint {
    pub fn new(occurs_range: Range) -> Self {
        Self { occurs_range }
    }

    pub fn occurs_range(&self) -> &Range {
        &self.occurs_range
    }
}

impl ConstraintValidator for OccursConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // No op
        // `occurs` does not work as a constraint by its own, it needs to be used with other constraints
        // e.g. `ordered_elements`, `fields`, etc.
        // the validation for occurs is done within these other constraints
        Ok(())
    }
}

/// Implements an `ordered_elements` constraint of Ion Schema
/// [ordered_elements]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#ordered_elements
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrderedElementsConstraint {
    type_ids: Vec<TypeId>,
}

impl OrderedElementsConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [OrderedElements] constraint from the given Element
    pub fn resolve_from_isl_constraint(
        type_references: &[IslTypeRef],
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let resolved_types: Vec<TypeId> = type_references
            .iter()
            .map(|t| IslTypeRef::resolve_type_reference(t, type_store, pending_types))
            .collect::<IonSchemaResult<Vec<TypeId>>>()?;
        Ok(OrderedElementsConstraint::new(resolved_types))
    }

    // Builds an NFA state machine based on given type_ids. This is a limited form of NFA where state machine is linear and every transition either leads to itself or the next state.
    //
    // All the states has some transitions between them leading from one state to another or back to itself.
    // All the intermediate states that have a minimum occurrence of 0 are optional states, meaning those states can lead to another state with 0 occurrence event transitions.
    // There are two special cases of transition that need to be handled.
    // For any state whose corresponding `type_id` has an `occurs` where:
    //   * `max >= 2`, that state will have a transition back to itself, allowing for repetition.
    //   * `min == 0`, that state will have a transition that advances to the next state automatically, making an occurrence of that `type_id` optional.
    //
    // Here is an example of how the built NFA would look like for an `ordered_elements` constraint:
    // ```
    // ordered_elements: [
    //     { type: int, occurs: optional },
    //     number,
    //     any
    // ]
    // ```
    // NFA:
    //                              +--------- 0 -----------+
    //                              |                       |
    //                              |                       V
    // I(INITIAL) ----> S1(INTERMEDIATE(0, 1)) -- 1 --> S2(INTERMEDIATE(1, 1)) -- 1 --> S3(INTERMEDIATE(1, 1)) ----> F(FINAL)
    //
    // Validation:
    // Valid input value: `[1, 2, 3]`
    // +------------------------------+
    // | event  | State Visits        |
    // +------------------------------+
    // |   -    |  I: 1               |
    // |   1    |  S1: 1, S2: 1       |
    // |   2    |  S2: 1, S3: 1       |
    // |   3    |  S3: 1              |
    // |   END  |  F: 1               |
    // +------------------------------+
    //
    // Invalid input value: `[1, 2]`
    // +------------------------------+
    // | event  | State Visits        |
    // +------------------------------+
    // |   -    |  I: 1               |
    // |   1    |  S1: 1, S2: 1       |
    // |   2    |  S2: 1, S3: 1       |
    // |   END  |  S3: 1              |
    // +------------------------------+
    // As shown above visit count for `END` doesn't have final state in it which means the value resulted to be invalid.
    //
    fn build_nfa_from_type_ids(type_ids: &[TypeId], type_store: &TypeStore) -> NfaEvaluation {
        let mut nfa_builder = NfaBuilder::new();
        let mut final_states = HashSet::new();
        for (state_id, type_id) in type_ids.iter().enumerate() {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            let occurs_range: Range = type_def.get_occurs_constraint("ordered_elements");

            // unwrap here won't lead to panic as the check for non negative range was already done while parsing ordered_elements constraint
            let (min, max) = occurs_range.non_negative_range_boundaries().unwrap();

            // if the current state is required then that is the only final state till now
            if min > 0 {
                // remove all previous final states
                final_states.clear();
            }

            // add current state as final state to NFA
            final_states.insert(FinalState::new(state_id, min, max));

            if state_id == 0 {
                // add a transition to self for initial state
                nfa_builder.with_transition(state_id, state_id, *type_id, min, max);
                continue;
            }

            // add transition to next state
            nfa_builder.with_transition(state_id - 1, state_id, *type_id, min, max);

            if max > 1 {
                // add a transition to self for states that have  max > 1
                nfa_builder.with_transition(state_id, state_id, *type_id, min, max);
            }
        }

        NfaEvaluation::new(Rc::new(nfa_builder.build(final_states)))
    }
}

impl ConstraintValidator for OrderedElementsConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let violations: Vec<Violation> = vec![];

        let values: Vec<Element> = match &value {
            IonSchemaElement::SingleElement(element) => match element.as_sequence() {
                None => {
                    return Err(Violation::with_violations(
                        "ordered_elements",
                        ViolationCode::TypeMismatched,
                        format!(
                            "expected list/sexp ion found {}",
                            if element.is_null() {
                                format!("{element}")
                            } else {
                                format!("{}", element.ion_type())
                            }
                        ),
                        ion_path,
                        violations,
                    ));
                }
                Some(sequence) => sequence.iter().map(|a| a.to_owned()).collect(),
            },
            IonSchemaElement::Document(document) => document.to_owned(),
        };

        // build nfa for validation
        let mut nfa_evaluation =
            OrderedElementsConstraint::build_nfa_from_type_ids(&self.type_ids, type_store);

        if !values.is_empty() && nfa_evaluation.nfa.get_final_states().is_empty() {
            return Err(Violation::with_violations(
                "ordered_elements",
                ViolationCode::TypeMismatched,
                "one or more ordered elements didn't match",
                ion_path,
                violations,
            ));
        }

        // use nfa_evaluation for validation
        nfa_evaluation.validate_ordered_elements(values, type_store);

        if !nfa_evaluation.has_final_state(type_store) {
            return Err(Violation::with_violations(
                "ordered_elements",
                ViolationCode::TypeMismatched,
                "one or more ordered elements didn't match",
                ion_path,
                violations,
            ));
        }

        Ok(())
    }
}

/// Implements an `fields` constraint of Ion Schema
/// [fields]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#fields
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldsConstraint {
    fields: HashMap<String, TypeId>,
    open_content: bool,
}

impl FieldsConstraint {
    pub fn new(fields: HashMap<String, TypeId>, open_content: bool) -> Self {
        Self {
            fields,
            open_content,
        }
    }

    /// Provides boolean value indicating whether open content is allowed or not for the fields
    pub fn open_content(&self) -> bool {
        self.open_content
    }

    /// Tries to create an [Fields] constraint from the given Element
    pub fn resolve_from_isl_constraint(
        fields: &HashMap<String, IslTypeRef>,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
        open_content: bool, // Indicates if open content is allowed or not for the fields in the container
    ) -> IonSchemaResult<Self> {
        let resolved_fields: HashMap<String, TypeId> = fields
            .iter()
            .map(|(f, t)| {
                IslTypeRef::resolve_type_reference(t, type_store, pending_types)
                    .map(|type_id| (f.to_owned(), type_id))
            })
            .collect::<IonSchemaResult<HashMap<String, TypeId>>>()?;
        Ok(FieldsConstraint::new(resolved_fields, open_content))
    }
}

impl ConstraintValidator for FieldsConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];

        // get struct value
        let ion_struct = value
            .expect_element_of_type(&[IonType::Struct], "fields", ion_path)?
            .as_struct()
            .unwrap();

        // Verify if open content exists in the struct fields
        if !self.open_content() {
            for (field_name, value) in ion_struct.iter() {
                if !self.fields.contains_key(field_name.text().unwrap()) {
                    violations.push(Violation::new(
                        "fields",
                        ViolationCode::InvalidOpenContent,
                        format!("Found open content in the struct: {field_name}: {value}"),
                        ion_path,
                    ));
                }
            }
        }

        // get the values corresponding to the field_name and perform occurs_validation based on the type_def
        for (field_name, type_id) in &self.fields {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            let values: Vec<&Element> = ion_struct.get_all(field_name).collect();

            // add parent value for current field in ion path
            ion_path.push(IonPathElement::Field(field_name.to_owned()));

            // perform occurs validation for type_def for all values of the given field_name
            let occurs_range: Range = type_def.get_occurs_constraint("fields");

            // verify if values follow occurs_range constraint
            if !occurs_range.contains(&(values.len() as i64).into()) {
                violations.push(Violation::new(
                    "fields",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "Expected {} of field {}: found {}",
                        occurs_range,
                        field_name,
                        values.len()
                    ),
                    ion_path,
                ));
            }

            // verify if all the values for this field name are valid according to type_def
            for value in values {
                let schema_element: IonSchemaElement = value.into();
                if let Err(violation) = type_def.validate(&schema_element, type_store, ion_path) {
                    violations.push(violation);
                }
            }

            // remove current field from list of parents
            ion_path.pop();
        }

        // return error if there were any violation found during validation
        if !violations.is_empty() {
            return Err(Violation::with_violations(
                "fields",
                ViolationCode::FieldsNotMatched,
                "value didn't satisfy fields constraint",
                ion_path,
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements Ion Schema's `contains` constraint
/// [contains]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#contains
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContainsConstraint {
    // TODO: convert this into a HashSet once we have an implementation of Hash for Element in ion-rust
    // Reference ion-rust issue: https://github.com/amazon-ion/ion-rust/issues/220
    values: Vec<Element>,
}

impl ContainsConstraint {
    pub fn new(values: Vec<Element>) -> Self {
        Self { values }
    }
}

impl ConstraintValidator for ContainsConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // Create a peekable iterator for given sequence
        let values: Vec<Element> = match &value {
            IonSchemaElement::SingleElement(element) => {
                match element.as_sequence() {
                    None => {
                        // return Violation if value is not an Ion sequence
                        return Err(Violation::new(
                            "contains",
                            ViolationCode::TypeMismatched,
                            &format!(
                                "expected list/sexp found {}",
                                if element.is_null() {
                                    format!("{element}")
                                } else {
                                    format!("{}", element.ion_type())
                                }
                            ),
                            ion_path,
                        ));
                    }
                    Some(ion_sequence) => ion_sequence.iter().map(|a| a.to_owned()).collect(),
                }
            }
            IonSchemaElement::Document(document) => document.to_owned(),
        };

        // add all the missing values found during validation
        let mut missing_values = vec![];

        // for each value in expected values if it does not exist in ion sequence
        // then add it to missing_values to keep track of missing values
        for expected_value in self.values.iter() {
            if !values.contains(expected_value) {
                missing_values.push(expected_value);
            }
        }

        // return Violation if there were any values added to the missing values vector
        if !missing_values.is_empty() {
            return Err(Violation::new(
                "contains",
                ViolationCode::MissingValue,
                format!("{value} has missing value(s): {missing_values:?}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements an `container_length` constraint of Ion Schema
/// [container_length]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#container_length
#[derive(Debug, Clone, PartialEq)]
pub struct ContainerLengthConstraint {
    length_range: Range,
}

impl ContainerLengthConstraint {
    pub fn new(length_range: Range) -> Self {
        Self { length_range }
    }

    pub fn length(&self) -> &Range {
        &self.length_range
    }
}

impl ConstraintValidator for ContainerLengthConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get the size of given value container
        let size = match value {
            IonSchemaElement::SingleElement(element) => {
                // Check for null container
                if element.is_null() {
                    return Err(Violation::new(
                        "container_length",
                        ViolationCode::TypeMismatched,
                        format!("expected a container found {element}"),
                        ion_path,
                    ));
                }

                match element.ion_type() {
                    IonType::List | IonType::SExpression => {
                        element.as_sequence().unwrap().iter().count()
                    }
                    IonType::Struct => element.as_struct().unwrap().iter().count(),
                    _ => {
                        // return Violation if value is not an Ion container
                        return Err(Violation::new(
                            "container_length",
                            ViolationCode::TypeMismatched,
                            format!(
                                "expected a container (a list/sexp/struct) but found {}",
                                element.ion_type()
                            ),
                            ion_path,
                        ));
                    }
                }
            }
            IonSchemaElement::Document(document) => document.len(),
        };

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the container size didn't follow container_length constraint
        if !length_range.contains(&(size as i64).into()) {
            return Err(Violation::new(
                "container_length",
                ViolationCode::InvalidLength,
                format!("expected container length {length_range} found {size}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `byte_length` constraint
/// [byte_length]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#byte_length
#[derive(Debug, Clone, PartialEq)]
pub struct ByteLengthConstraint {
    length_range: Range,
}

impl ByteLengthConstraint {
    pub fn new(length_range: Range) -> Self {
        Self { length_range }
    }

    pub fn length(&self) -> &Range {
        &self.length_range
    }
}

impl ConstraintValidator for ByteLengthConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get the size of given bytes
        let size = value
            .expect_element_of_type(&[IonType::Blob, IonType::Clob], "byte_length", ion_path)?
            .as_bytes()
            .unwrap()
            .len();

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the clob/blob size didn't follow byte_length constraint
        if !length_range.contains(&(size as i64).into()) {
            return Err(Violation::new(
                "byte_length",
                ViolationCode::InvalidLength,
                format!("expected byte length {length_range} found {size}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements an `codepoint_length` constraint of Ion Schema
/// [codepoint_length]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#codepoint_length
#[derive(Debug, Clone, PartialEq)]
pub struct CodepointLengthConstraint {
    length_range: Range,
}

impl CodepointLengthConstraint {
    pub fn new(length_range: Range) -> Self {
        Self { length_range }
    }

    pub fn length(&self) -> &Range {
        &self.length_range
    }
}

impl ConstraintValidator for CodepointLengthConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get the size of given string/symbol Unicode codepoints
        let size = value
            .expect_element_of_type(
                &[IonType::String, IonType::Symbol],
                "codepoint_length",
                ion_path,
            )?
            .as_str()
            .unwrap()
            .chars()
            .count();

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the string/symbol codepoint size didn't follow codepoint_length constraint
        if !length_range.contains(&(size as i64).into()) {
            return Err(Violation::new(
                "codepoint_length",
                ViolationCode::InvalidLength,
                format!("expected codepoint length {length_range} found {size}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements the `element` constraint
/// [element]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#element
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementConstraint {
    type_id: TypeId,
}

impl ElementConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create an element constraint from the given Element
    pub fn resolve_from_isl_constraint(
        type_reference: &IslTypeRef,
        type_store: &mut TypeStore,
        pending_types: &mut PendingTypes,
    ) -> IonSchemaResult<Self> {
        let type_id =
            IslTypeRef::resolve_type_reference(type_reference, type_store, pending_types)?;
        Ok(ElementConstraint::new(type_id))
    }
}

impl ConstraintValidator for ElementConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];

        // this type_id was validated while creating `ElementConstraint` hence the unwrap here is safe
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();

        // validate element constraint for container types
        match value {
            IonSchemaElement::SingleElement(element) => {
                // Check for null container
                if element.is_null() {
                    return Err(Violation::new(
                        "element",
                        ViolationCode::TypeMismatched,
                        format!("expected a container but found {element}"),
                        ion_path,
                    ));
                }

                // validate each element of the given value container
                match element.ion_type() {
                    IonType::List | IonType::SExpression => {
                        for (index, val) in element.as_sequence().unwrap().iter().enumerate() {
                            ion_path.push(IonPathElement::Index(index));
                            let schema_element: IonSchemaElement = val.into();
                            if let Err(violation) =
                                type_def.validate(&schema_element, type_store, ion_path)
                            {
                                violations.push(violation);
                            }
                            ion_path.pop();
                        }
                    }
                    IonType::Struct => {
                        for (field_name, val) in element.as_struct().unwrap().iter() {
                            ion_path
                                .push(IonPathElement::Field(field_name.text().unwrap().to_owned()));
                            let schema_element: IonSchemaElement = val.into();
                            if let Err(violation) =
                                type_def.validate(&schema_element, type_store, ion_path)
                            {
                                violations.push(violation);
                            }
                            ion_path.pop();
                        }
                    }
                    _ => {
                        // return Violation if value is not an Ion container
                        return Err(Violation::new(
                            "element",
                            ViolationCode::TypeMismatched,
                            format!(
                                "expected a container (a list/sexp/struct) but found {}",
                                element.ion_type()
                            ),
                            ion_path,
                        ));
                    }
                }
            }
            IonSchemaElement::Document(document) => {
                for (index, val) in document.iter().enumerate() {
                    ion_path.push(IonPathElement::Index(index));
                    let schema_element: IonSchemaElement = val.into();

                    if let Err(violation) = type_def.validate(&schema_element, type_store, ion_path)
                    {
                        violations.push(violation);
                    }
                    ion_path.pop();
                }
            }
        }

        if !violations.is_empty() {
            return Err(Violation::with_violations(
                "element",
                ViolationCode::ElementMismatched,
                "one or more elements don't satisfy element constraint",
                ion_path,
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements the `annotations` constraint
/// [annotations]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#annotations
// The `required` annotation provided on the list of annotations is not represented here,
// requirement of an annotation is represented in the annotation itself by the field `is_required` of `Annotation` struct.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationsConstraint {
    is_closed: bool,
    is_ordered: bool,
    annotations: Vec<Annotation>,
}

impl AnnotationsConstraint {
    pub fn new(is_closed: bool, is_ordered: bool, annotations: Vec<Annotation>) -> Self {
        Self {
            is_closed,
            is_ordered,
            annotations,
        }
    }

    // Find the required expected annotation from value annotations
    // This is a helper method used by validate_ordered_annotations
    pub fn find_expected_annotation<'a, I: Iterator<Item = &'a str>>(
        &self,
        value_annotations: &mut Peekable<I>,
        expected_annotation: &Annotation,
    ) -> bool {
        // As there are open content possible for annotations that doesn't have list-level `closed` annotation
        // traverse through the next annotations to find this expected, ordered and required annotation
        while Option::is_some(&value_annotations.peek()) && !self.is_closed {
            if expected_annotation.value() == value_annotations.next().unwrap() {
                return true;
            }
        }

        // if we didn't find the expected annotation return false
        false
    }

    pub fn validate_ordered_annotations(
        &self,
        value: &Element,
        type_store: &TypeStore,
        violations: Vec<Violation>,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let mut value_annotations = value
            .annotations()
            .map(|sym| sym.text().unwrap())
            .peekable();

        // iterate over the expected annotations and see if there are any unexpected value annotations found
        for expected_annotation in &self.annotations {
            if let Some(actual_annotation) = value_annotations.peek() {
                if expected_annotation.is_required()
                    && expected_annotation.value() != actual_annotation
                {
                    // iterate over the actual value annotations to find the required expected annotation
                    if !self.find_expected_annotation(&mut value_annotations, expected_annotation) {
                        // missing required expected annotation
                        return Err(Violation::new(
                            "annotations",
                            ViolationCode::AnnotationMismatched,
                            "annotations don't match expectations",
                            ion_path,
                        ));
                    }
                } else if expected_annotation.value() == actual_annotation {
                    let _ = value_annotations.next(); // consume the annotation if its equal to the expected annotation
                }
            } else if expected_annotation.is_required() {
                // we already exhausted value annotations and didn't find the required expected annotation
                return Err(Violation::new(
                    "annotations",
                    ViolationCode::AnnotationMismatched,
                    "annotations don't match expectations",
                    ion_path,
                ));
            }
        }

        if self.is_closed && Option::is_some(&value_annotations.peek()) {
            // check if there are still annotations left at the end of the list
            return Err(Violation::with_violations(
                "annotations",
                ViolationCode::AnnotationMismatched,
                // unwrap as we already verified with peek that there is a value
                format!(
                    "Unexpected annotations found {}",
                    value_annotations.next().unwrap()
                ),
                ion_path,
                violations,
            ));
        }

        Ok(())
    }

    pub fn validate_unordered_annotations(
        &self,
        value: &Element,
        type_store: &TypeStore,
        violations: Vec<Violation>,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // This will be used by a violation to to return all the missing annotations
        let mut missing_annotations: Vec<&Annotation> = vec![];

        let value_annotations: Vec<&str> =
            value.annotations().map(|sym| sym.text().unwrap()).collect();

        for expected_annotation in &self.annotations {
            // verify if the expected_annotation is required and if it matches with value annotation
            if expected_annotation.is_required()
                && !value
                    .annotations()
                    .any(|a| a.text().unwrap() == expected_annotation.value())
            {
                missing_annotations.push(expected_annotation);
            }
        }

        // if missing_annotations is not empty return violation
        if !missing_annotations.is_empty() {
            return Err(Violation::with_violations(
                "annotations",
                ViolationCode::MissingAnnotation,
                format!("missing annotation(s): {missing_annotations:?}"),
                ion_path,
                violations,
            ));
        }

        // if the annotations is annotated with `closed` at list-level then verify
        // there are no unexpected annotations in the value annotations
        if self.is_closed
            && !value_annotations.iter().all(|v| {
                self.annotations
                    .iter()
                    .any(|expected_ann| v == expected_ann.value())
            })
        {
            return Err(Violation::with_violations(
                "annotations",
                ViolationCode::UnexpectedAnnotation,
                "found one or more unexpected annotations",
                ion_path,
                violations,
            ));
        }

        Ok(())
    }
}

impl ConstraintValidator for AnnotationsConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        let violations: Vec<Violation> = vec![];

        match value {
            IonSchemaElement::SingleElement(element) => {
                // validate annotations that have list-level `ordered` annotation
                if self.is_ordered {
                    return self
                        .validate_ordered_annotations(element, type_store, violations, ion_path);
                }

                // validate annotations that does not have list-level `ordered` annotation
                self.validate_unordered_annotations(element, type_store, violations, ion_path)
            }
            IonSchemaElement::Document(document) => {
                // document type can not have annotations
                Err(Violation::new(
                    "annotations",
                    ViolationCode::AnnotationMismatched,
                    "annotations constraint is not applicable for document type",
                    ion_path,
                ))
            }
        }
    }
}

/// Implements Ion Schema's `precision` constraint
/// [precision]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#precision
#[derive(Debug, Clone, PartialEq)]
pub struct PrecisionConstraint {
    precision_range: Range,
}

impl PrecisionConstraint {
    pub fn new(precision_range: Range) -> Self {
        Self { precision_range }
    }

    pub fn precision(&self) -> &Range {
        &self.precision_range
    }
}

impl ConstraintValidator for PrecisionConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get precision of decimal value
        let value_precision = value
            .expect_element_of_type(&[IonType::Decimal], "precision", ion_path)?
            .as_decimal()
            .unwrap()
            .precision();

        // get isl decimal precision as a range
        let precision_range: &Range = self.precision();

        // return a Violation if the value didn't follow precision constraint
        if !precision_range.contains(&(value_precision as i64).into()) {
            return Err(Violation::new(
                "precision",
                ViolationCode::InvalidLength,
                format!("expected precision {precision_range} found {value_precision}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `scale` constraint
/// [scale]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#scale
#[derive(Debug, Clone, PartialEq)]
pub struct ScaleConstraint {
    scale_range: Range,
}

impl ScaleConstraint {
    pub fn new(scale_range: Range) -> Self {
        Self { scale_range }
    }

    pub fn scale(&self) -> &Range {
        &self.scale_range
    }
}

impl ConstraintValidator for ScaleConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get scale of decimal value
        let value_scale = value
            .expect_element_of_type(&[IonType::Decimal], "precision", ion_path)?
            .as_decimal()
            .unwrap()
            .scale();

        // get isl decimal scale as a range
        let scale_range: &Range = self.scale();

        // return a Violation if the value didn't follow scale constraint
        if !scale_range.contains(&(value_scale).into()) {
            return Err(Violation::new(
                "scale",
                ViolationCode::InvalidLength,
                format!("expected scale {scale_range} found {value_scale}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `timestamp_precision` constraint
/// [timestamp_precision]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_precision
#[derive(Debug, Clone, PartialEq)]
pub struct TimestampPrecisionConstraint {
    timestamp_precision_range: Range,
}

impl TimestampPrecisionConstraint {
    pub fn new(scale_range: Range) -> Self {
        Self {
            timestamp_precision_range: scale_range,
        }
    }

    pub fn timestamp_precision(&self) -> &Range {
        &self.timestamp_precision_range
    }
}

impl ConstraintValidator for TimestampPrecisionConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get timestamp value
        let timestamp_value = value
            .expect_element_of_type(&[IonType::Timestamp], "timestamp_precision", ion_path)?
            .as_timestamp()
            .unwrap();

        // get isl timestamp precision as a range
        let precision_range: &Range = self.timestamp_precision();

        // return a Violation if the value didn't follow timestamp precision constraint
        if !precision_range.contains(&(timestamp_value.to_owned()).into()) {
            return Err(Violation::new(
                "precision",
                ViolationCode::InvalidLength,
                format!(
                    "expected precision {} found {:?}",
                    precision_range,
                    timestamp_value.precision()
                ),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `valid_values` constraint
/// [valid_values]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#valid_values
#[derive(Debug, Clone, PartialEq)]
pub struct ValidValuesConstraint {
    valid_values: Vec<ValidValue>,
}

impl ValidValuesConstraint {
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
}

impl Display for ValidValuesConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "[ ")?;
        let mut itr = self.valid_values.iter();
        if let Some(item) = itr.next() {
            write!(f, "{item}")?;
        }
        for item in itr {
            write!(f, ", {item}")?;
        }
        write!(f, " ]")
    }
}

impl ConstraintValidator for ValidValuesConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        match value {
            IonSchemaElement::SingleElement(value) => {
                for valid_value in &self.valid_values {
                    match valid_value {
                        ValidValue::Range(range) => match value.ion_type() {
                            IonType::Integer
                            | IonType::Float
                            | IonType::Decimal
                            | IonType::Timestamp => {
                                if range.contains(value) {
                                    return Ok(());
                                }
                            }
                            _ => {}
                        },
                        ValidValue::Element(element) => {
                            // get value without annotations
                            let value = value.to_owned().with_annotations(vec![]);

                            if element == &value {
                                return Ok(());
                            }
                        }
                    };
                }
                Err(Violation::new(
                    "valid_values",
                    ViolationCode::InvalidValue,
                    format!(
                        "expected valid_values to be from {}, found {}",
                        &self, value
                    ),
                    ion_path,
                ))
            }
            IonSchemaElement::Document(document) => Err(Violation::new(
                "valid_values",
                ViolationCode::InvalidValue,
                format!(
                    "expected valid_values to be from {}, found {}",
                    &self, value
                ),
                ion_path,
            )),
        }
    }
}

/// Implements Ion Schema's `regex` constraint
/// [regex]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#regex
#[derive(Debug, Clone)]
pub struct RegexConstraint {
    expression: Regex,
    case_insensitive: bool,
    multiline: bool,
}

impl RegexConstraint {
    fn new(expression: Regex, case_insensitive: bool, multiline: bool) -> Self {
        Self {
            expression,
            case_insensitive,
            multiline,
        }
    }

    /// Converts given string to a pattern based on regex features supported by Ion Schema Specification
    /// For more information: `<https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#regex>`
    fn convert_to_pattern(expression: String) -> IonSchemaResult<String> {
        let mut sb = String::new();
        let mut si = expression.as_str().chars().peekable();

        while let Some(ch) = si.next() {
            match ch {
                '[' => {
                    // push the starting bracket `[` to the result string
                    // and then parse the next characters using parse_char_class
                    sb.push(ch);
                    RegexConstraint::parse_char_class(&mut sb, &mut si)?;
                }
                '(' => {
                    sb.push(ch);
                    if let Some(ch) = si.next() {
                        if ch == '?' {
                            return invalid_schema_error(format!("invalid character {ch}"));
                        }
                        sb.push(ch)
                    }
                }
                '\\' => {
                    if let Some(ch) = si.next() {
                        match ch {
                            // Note: Ion text a backslash must itself be escaped, so correct escaping
                            // of below characters requires two backslashes, e.g.: \\.
                            '.' | '^' | '$' | '|' | '?' | '*' | '+' | '\\' | '[' | ']' | '('
                            | ')' | '{' | '}' | 'w' | 'W' | 'd' | 'D' => {
                                sb.push('\\');
                                sb.push(ch)
                            }
                            's' => sb.push_str("[ \\f\\n\\r\\t]"),
                            'S' => sb.push_str("[^ \\f\\n\\r\\t]"),
                            _ => {
                                return invalid_schema_error(format!(
                                    "invalid escape character {ch}",
                                ))
                            }
                        }
                    }
                }
                // TODO: remove below match statement once we have fixed issue: https://github.com/amazon-ion/ion-rust/issues/399
                '\r' => sb.push('\n'), // Replace '\r' with '\n'
                _ => sb.push(ch),
            }
            RegexConstraint::parse_quantifier(&mut sb, &mut si)?;
        }

        Ok(sb)
    }

    fn parse_char_class(sb: &mut String, si: &mut Peekable<Chars<'_>>) -> IonSchemaResult<()> {
        while let Some(ch) = si.next() {
            sb.push(ch);
            match ch {
                '&' => {
                    if si.peek() == Some(&'&') {
                        return invalid_schema_error("'&&' is not supported in a character class");
                    }
                }
                '[' => return invalid_schema_error("'[' must be escaped within a character class"),
                '\\' => {
                    if let Some(ch2) = si.next() {
                        match ch2 {
                            // escaped `[` or ']' are allowed within character class
                            '[' | ']' | '\\' => sb.push(ch2),
                            // not supporting pre-defined char classes (i.e., \d, \s, \w)
                            // as user is specifying a new char class
                            _ => {
                                return invalid_schema_error(format!(
                                    "invalid sequence '\\{ch2}' in character class"
                                ))
                            }
                        }
                    }
                }
                ']' => return Ok(()),
                _ => {}
            }
        }

        invalid_schema_error("character class missing ']'")
    }

    fn parse_quantifier(sb: &mut String, si: &mut Peekable<Chars<'_>>) -> IonSchemaResult<()> {
        let initial_length = sb.len();
        if let Some(ch) = si.peek().cloned() {
            match ch {
                '?' | '*' | '+' => {
                    if let Some(ch) = si.next() {
                        sb.push(ch);
                    }
                }
                '{' => {
                    // we know next is `{` so unwrap it and add it to the result string
                    let ch = si.next().unwrap();
                    sb.push(ch);
                    // process occurrences specified within `{` and `}`
                    let mut complete = false;
                    for ch in si.by_ref() {
                        match ch {
                            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | ',' => {
                                sb.push(ch)
                            }
                            '}' => {
                                sb.push(ch);
                                // at this point occurrences are completely specified
                                complete = true;
                                break;
                            }
                            _ => return invalid_schema_error(format!("invalid character {ch}")),
                        }
                    }

                    if !complete {
                        return invalid_schema_error("range quantifier missing '}'");
                    }
                }
                _ => {}
            }
            if sb.len() > initial_length {
                if let Some(ch) = si.peek().cloned() {
                    match ch {
                        '?' => return invalid_schema_error(format!("invalid character {ch}")),

                        '+' => return invalid_schema_error(format!("invalid character {ch}")),
                        _ => {}
                    }
                }
            }
        }

        Ok(())
    }
}

impl ConstraintValidator for RegexConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get string value and return violation if its not a string or symbol type
        let string_value = value
            .expect_element_of_type(&[IonType::String, IonType::Symbol], "regex", ion_path)?
            .as_str()
            .unwrap();

        // create regular expression
        let re = Regex::new(r"\r").unwrap();
        let result = re.replace_all(string_value, "\n");
        let value = result.to_string();

        // verify if given value matches regular expression
        if !self.expression.is_match(value.as_str()) {
            return Err(Violation::new(
                "regex",
                ViolationCode::RegexMismatched,
                format!("{} doesn't match regex {}", value, self.expression),
                ion_path,
            ));
        }

        Ok(())
    }
}

impl TryFrom<IslRegexConstraint> for RegexConstraint {
    type Error = IonSchemaError;

    fn try_from(isl_regex: IslRegexConstraint) -> Result<Self, Self::Error> {
        let pattern = RegexConstraint::convert_to_pattern(isl_regex.expression().to_owned())?;

        let regex = RegexBuilder::new(pattern.as_str())
            .case_insensitive(isl_regex.case_insensitive())
            .multi_line(isl_regex.multi_line())
            .build()
            .map_err(|e| {
                invalid_schema_error_raw(format!("Invalid regex {}", isl_regex.expression()))
            })?;

        Ok(RegexConstraint::new(
            regex,
            isl_regex.case_insensitive(),
            isl_regex.multi_line(),
        ))
    }
}

impl PartialEq for RegexConstraint {
    fn eq(&self, other: &Self) -> bool {
        self.expression.as_str().eq(other.expression.as_str())
            && self.case_insensitive == other.case_insensitive
            && self.multiline == other.multiline
    }
}

/// Implements Ion Schema's `utf8_byte_length` constraint
/// [utf8_byte_length]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#utf8_byte_length
#[derive(Debug, Clone, PartialEq)]
pub struct Utf8ByteLengthConstraint {
    length_range: Range,
}

impl Utf8ByteLengthConstraint {
    pub fn new(length_range: Range) -> Self {
        Self { length_range }
    }

    pub fn length(&self) -> &Range {
        &self.length_range
    }
}

impl ConstraintValidator for Utf8ByteLengthConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get the size of given bytes
        let size = value
            .expect_element_of_type(
                &[IonType::String, IonType::Symbol],
                "utf8_byte_length",
                ion_path,
            )?
            .as_str()
            .unwrap()
            .len();

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the string/symbol size didn't follow utf8_byte_length constraint
        if !length_range.contains(&(size as i64).into()) {
            return Err(Violation::new(
                "utf8_byte_length",
                ViolationCode::InvalidLength,
                format!("expected utf8 byte length {length_range} found {size}"),
                ion_path,
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `timestamp_offset` constraint
/// [timestamp_offset]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#timestamp_offset
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimestampOffsetConstraint {
    valid_offsets: Vec<TimestampOffset>,
}

impl TimestampOffsetConstraint {
    pub fn new(valid_offsets: Vec<TimestampOffset>) -> Self {
        Self { valid_offsets }
    }

    pub fn valid_offsets(&self) -> &Vec<TimestampOffset> {
        &self.valid_offsets
    }
}

impl ConstraintValidator for TimestampOffsetConstraint {
    fn validate(
        &self,
        value: &IonSchemaElement,
        type_store: &TypeStore,
        ion_path: &mut IonPath,
    ) -> ValidationResult {
        // get timestamp value
        let timestamp_value = value
            .expect_element_of_type(&[IonType::Timestamp], "timestamp_offset", ion_path)?
            .as_timestamp()
            .unwrap();

        // get isl timestamp precision as a range
        let valid_offsets: &Vec<TimestampOffset> = self.valid_offsets();

        // return a Violation if the value didn't follow timestamp precision constraint
        if !valid_offsets.contains(&timestamp_value.offset().into()) {
            let formatted_valid_offsets: Vec<String> =
                valid_offsets.iter().map(|t| format!("{t}")).collect();

            return Err(Violation::new(
                "timestamp_offset",
                ViolationCode::InvalidLength,
                format!(
                    "expected timestamp offset from {:?} found {}",
                    formatted_valid_offsets,
                    <Option<i32> as Into<TimestampOffset>>::into(timestamp_value.offset())
                ),
                ion_path,
            ));
        }

        Ok(())
    }
}
