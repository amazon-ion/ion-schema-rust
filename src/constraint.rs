use crate::isl::isl_constraint::{IslConstraint, IslValidValuesConstraint};
use crate::isl::isl_type_reference::IslTypeRef;
use crate::isl::util::{Annotation, Range};
use crate::result::{IonSchemaResult, ValidationResult};
use crate::system::{PendingTypes, TypeId, TypeStore};
use crate::types::{TypeDefinition, TypeValidator};
use crate::violation::{Violation, ViolationCode};
use ion_rs::value::owned::OwnedElement;
use ion_rs::value::{Element, Sequence, Struct, SymbolToken};
use ion_rs::IonType;
use std::collections::HashMap;
use std::iter::Peekable;

/// Provides validation for schema Constraint
pub trait ConstraintValidator {
    /// Checks this constraint against the provided value,
    /// adding [Violation]s and/or [ViolationChild]ren to `Err(violation)`
    /// if the constraint is violated.
    /// Otherwise, if the value passes the validation against the constraint then returns `Ok(())`.
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult;
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
    Scale(ScaleConstraint),
    TimestampPrecision(TimestampPrecisionConstraint),
    Type(TypeConstraint),
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

    /// Creates a [Constraint::Contains] referring to [OwnedElements] specified inside it
    pub fn contains<A: Into<Vec<OwnedElement>>>(values: A) -> Constraint {
        Constraint::Contains(ContainsConstraint::new(values.into()))
    }

    /// Creates a [Constraint::ContainerLength] from a [Range] specifying a length range.
    pub fn container_length(length: Range) -> Constraint {
        if !matches!(length, Range::IntegerNonNegative(_, _)) {
            panic!("container_length constraint must have a range of type IntegerNonNegative")
        }
        Constraint::ContainerLength(ContainerLengthConstraint::new(length))
    }

    /// Creates a [Constraint::ByteLength] from a [Range] specifying a length range.
    pub fn byte_length(length: Range) -> Constraint {
        if !matches!(length, Range::IntegerNonNegative(_, _)) {
            panic!("byte_length constraint must have a range of type IntegerNonNegative")
        }
        Constraint::ByteLength(ByteLengthConstraint::new(length))
    }

    /// Creates a [Constraint::CodePointLength] from a [Range] specifying a length range.
    pub fn codepoint_length(length: Range) -> Constraint {
        if !matches!(length, Range::IntegerNonNegative(_, _)) {
            panic!("codepoint_length constraint must have a range of type IntegerNonNegative")
        }
        Constraint::CodepointLength(CodepointLengthConstraint::new(length))
    }

    /// Creates a [Constraint::Element] referring to the type represented by the provided [TypeId].
    pub fn element(type_id: TypeId) -> Constraint {
        Constraint::Element(ElementConstraint::new(type_id))
    }

    /// Creates a [Constraint::Annotations] using [str]s and [OwnedElement]s specified inside it
    pub fn annotations<
        'a,
        A: IntoIterator<Item = &'a str>,
        B: IntoIterator<Item = OwnedElement>,
    >(
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
    pub fn precision(precision: Range) -> Constraint {
        if !matches!(precision, Range::IntegerNonNegative(_, _)) {
            panic!("precision constraint must have a range of type IntegerNonNegative")
        }
        Constraint::Precision(PrecisionConstraint::new(precision))
    }

    /// Creates a [Constraint::Scale] from a [Range] specifying a precision range.
    pub fn scale(scale: Range) -> Constraint {
        if !matches!(scale, Range::Integer(_, _)) {
            panic!("scale constraint must have a range of type Integer")
        }
        Constraint::Scale(ScaleConstraint::new(scale))
    }

    /// Creates a [Constraint::TimestampPrecision] from a [Range] specifying a precision range.
    pub fn timestamp_precision(precision: Range) -> Constraint {
        if !matches!(precision, Range::TimestampPrecision(_, _)) {
            panic!("timestamp_precision constraint must have a range of type TimestampPrecision")
        }
        Constraint::TimestampPrecision(TimestampPrecisionConstraint::new(precision))
    }

    /// Creates a [Constraint::Fields] referring to the fields represented by the provided field name and [TypeId]s.
    /// By default, fields created using this method will allow open content
    pub fn fields<I>(fields: I) -> Constraint
    where
        I: Iterator<Item = (String, TypeId)>,
    {
        Constraint::Fields(FieldsConstraint::new(fields.collect(), true))
    }

    /// Creates a [Constraint::ValidValues] using the [OwnedElements] specified inside it
    pub fn valid_values_with_values(values: Vec<OwnedElement>) -> Constraint {
        Constraint::ValidValues(ValidValuesConstraint::Values(values))
    }

    /// Creates a [Constraint::ValidValues] using the [Range] specified inside it
    pub fn valid_values_with_range(values: Range) -> Constraint {
        Constraint::ValidValues(ValidValuesConstraint::Range(values))
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
            IslConstraint::Scale(scale_range) => Ok(Constraint::Scale(ScaleConstraint::new(
                scale_range.to_owned(),
            ))),
            IslConstraint::TimestampPrecision(timestamp_precision_range) => {
                Ok(Constraint::TimestampPrecision(
                    TimestampPrecisionConstraint::new(timestamp_precision_range.to_owned()),
                ))
            }
            IslConstraint::ValidValues(valid_values) => {
                Ok(Constraint::ValidValues(match valid_values {
                    IslValidValuesConstraint::Range(range) => {
                        ValidValuesConstraint::Range(range.to_owned())
                    }
                    IslValidValuesConstraint::Values(values) => {
                        ValidValuesConstraint::Values(values.to_owned())
                    }
                }))
            }
        }
    }

    pub fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        match self {
            Constraint::AllOf(all_of) => all_of.validate(value, type_store),
            Constraint::Annotations(annotations) => annotations.validate(value, type_store),
            Constraint::AnyOf(any_of) => any_of.validate(value, type_store),
            Constraint::ByteLength(byte_length) => byte_length.validate(value, type_store),
            Constraint::CodepointLength(codepoint_length) => {
                codepoint_length.validate(value, type_store)
            }
            Constraint::Contains(contains) => contains.validate(value, type_store),
            Constraint::ContentClosed => {
                // No op
                // `content: closed` does not work as a constraint by its own, it needs to be used with other container constraints
                // e.g. `fields`
                // the validation for `content: closed` is done within these other constraints
                Ok(())
            }
            Constraint::ContainerLength(container_length) => {
                container_length.validate(value, type_store)
            }
            Constraint::Element(element) => element.validate(value, type_store),
            Constraint::Fields(fields) => fields.validate(value, type_store),
            Constraint::Not(not) => not.validate(value, type_store),
            Constraint::OneOf(one_of) => one_of.validate(value, type_store),
            Constraint::Type(type_constraint) => type_constraint.validate(value, type_store),
            Constraint::Occurs(occurs) => occurs.validate(value, type_store),
            Constraint::OrderedElements(ordered_elements) => {
                ordered_elements.validate(value, type_store)
            }
            Constraint::Precision(precision) => precision.validate(value, type_store),
            Constraint::Scale(scale) => scale.validate(value, type_store),
            Constraint::TimestampPrecision(timestamp_precision) => {
                timestamp_precision.validate(value, type_store)
            }
            Constraint::ValidValues(valid_values) => valid_values.validate(value, type_store),
        }
    }
}

/// Implements an `all_of` constraint of Ion Schema
/// [all_of]: https://amzn.github.io/ion-schema/docs/spec.html#all_of
#[derive(Debug, Clone, PartialEq)]
pub struct AllOfConstraint {
    type_ids: Vec<TypeId>,
}

impl AllOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [AllOf] constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let mut valid_types = vec![];
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            match type_def.validate(value, type_store) {
                Ok(_) => valid_types.push(type_id),
                Err(violation) => violations.push(violation),
            }
        }
        if !violations.is_empty() {
            return Err(Violation::with_violations(
                "all_of",
                ViolationCode::AllTypesNotMatched,
                &format!(
                    "value matches {} types, expected {}",
                    valid_types.len(),
                    self.type_ids.len()
                ),
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements an `any_of` constraint of Ion Schema
/// [any_of]: https://amzn.github.io/ion-schema/docs/spec.html#any_of
#[derive(Debug, Clone, PartialEq)]
pub struct AnyOfConstraint {
    type_ids: Vec<TypeId>,
}

impl AnyOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [AnyOf] constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let mut valid_types = vec![];
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            match type_def.validate(value, type_store) {
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
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements an `one_of` constraint of Ion Schema
/// [one_of]: https://amzn.github.io/ion-schema/docs/spec.html#one_of
#[derive(Debug, Clone, PartialEq)]
pub struct OneOfConstraint {
    type_ids: Vec<TypeId>,
}

impl OneOfConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [OneOf] constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];
        let mut valid_types = vec![];
        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            match type_def.validate(value, type_store) {
                Ok(_) => valid_types.push(type_id),
                Err(violation) => violations.push(violation),
            }
        }
        let total_valid_types = valid_types.len();
        return match total_valid_types {
            0 => Err(Violation::with_violations(
                "one_of",
                ViolationCode::NoTypesMatched,
                "value matches none of the types",
                violations,
            )),
            1 => Ok(()),
            _ => Err(Violation::with_violations(
                "one_of",
                ViolationCode::MoreThanOneTypeMatched,
                &format!("value matches {} types, expected 1", total_valid_types),
                violations,
            )),
        };
    }
}

/// Implements a `not` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#not
#[derive(Debug, Clone, PartialEq)]
pub struct NotConstraint {
    type_id: TypeId,
}

impl NotConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create a [Not] constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();
        let violation = type_def.validate(value, type_store);
        match violation {
            Err(violation) => Ok(()),
            Ok(_) => {
                // if there were no violations for the types then not constraint was unsatisfied
                Err(Violation::new(
                    "not",
                    ViolationCode::TypeMatched,
                    "value unexpectedly matches type",
                ))
            }
        }
    }
}

/// Implements a `type` constraint
/// [type]: https://amzn.github.io/ion-schema/docs/spec.html#type
#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstraint {
    type_id: TypeId,
}

impl TypeConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create a [Type] constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();
        type_def.validate(value, type_store)
    }
}

/// Implements an `occurs` constraint of Ion Schema
/// [occurs]: https://amzn.github.io/ion-schema/docs/spec.html#type-definitions
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // No op
        // `occurs` does not work as a constraint by its own, it needs to be used with other constraints
        // e.g. `ordered_elements`, `fields`, etc.
        // the validation for occurs is done within these other constraints
        Ok(())
    }
}

/// Implements an `ordered_elements` constraint of Ion Schema
/// [ordered_elements]: https://amzn.github.io/ion-schema/docs/spec.html#ordered_elements
#[derive(Debug, Clone, PartialEq)]
pub struct OrderedElementsConstraint {
    type_ids: Vec<TypeId>,
}

impl OrderedElementsConstraint {
    pub fn new(type_ids: Vec<TypeId>) -> Self {
        Self { type_ids }
    }

    /// Tries to create an [OrderedElements] constraint from the given OwnedElement
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

    /// Validates a type_def for occurs constraint using values_iter
    fn type_def_occurs_validation<'a>(
        type_def: &TypeDefinition,
        values_iter: &mut Peekable<Box<dyn Iterator<Item = &OwnedElement> + 'a>>,
        type_store: &TypeStore,
    ) -> ValidationResult {
        let occurs_range: Range = type_def.get_occurs_constraint("ordered_elements");

        // use this counter to keep track of valid values for given type_def
        let mut count: i64 = 0;

        // consume elements to reach the minimum required values for this type
        while let Some(value) =
            values_iter.next_if(|v| !occurs_range.contains(&count.into()).unwrap())
        {
            if type_def.is_valid(value, type_store) {
                count += 1;
            } else {
                // there's not enough values of this expected type
                return Err(Violation::new(
                    "ordered_elements",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "Expected {:?} of type {:?}: found {}",
                        occurs_range, type_def, count
                    ),
                ));
            }
        }

        // greedily take as many values as we can of this type without going out
        // of the maximum of the range
        while values_iter.peek() != None && occurs_range.contains(&(count + 1).into()).unwrap() {
            // don't consume it until we know it's valid for the type
            if let Some(value) = values_iter.peek() {
                if type_def.is_valid(value, type_store) {
                    let _ = values_iter.next(); // consume it as it is valid
                    count += 1;
                } else {
                    // if the value doesn't match this type_def, then we'll break out of the while
                    // loop and check the value against the next type_def.
                    break;
                }
            }
        }

        // verify if there is no values left to validate and if it follows `occurs` constraint for this expected type
        if values_iter.peek() == None && !occurs_range.contains(&count.into()).unwrap() {
            // there's not enough values of this expected type
            return Err(Violation::new(
                "ordered_elements",
                ViolationCode::TypeMismatched,
                &format!(
                    "Expected {:?} of type {:?}: found {}",
                    occurs_range, type_def, count
                ),
            ));
        }

        // if the type_def validation passes all the above checks return Ok(())
        Ok(())
    }
}

impl ConstraintValidator for OrderedElementsConstraint {
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let violations: Vec<Violation> = vec![];

        // Create a peekable iterator for given sequence
        let mut values_iter = match value.as_sequence() {
            None => {
                return Err(Violation::with_violations(
                    "ordered_elements",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected list/sexp ion found {}",
                        if value.is_null() {
                            format!("{:?}", value)
                        } else {
                            format!("{}", value.ion_type())
                        }
                    ),
                    violations,
                ));
            }
            Some(sequence) => sequence.iter().peekable(),
        };

        for type_id in &self.type_ids {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            OrderedElementsConstraint::type_def_occurs_validation(
                type_def,
                &mut values_iter,
                type_store,
            )?;
        }

        return if values_iter.peek() != None {
            // check if there still values left at the end of sequence (list/sexp), when we have already
            // completed visiting through all of the ordered elements type_defs
            Err(Violation::with_violations(
                "ordered_elements",
                ViolationCode::TypeMismatched,
                // unwrap as we already verified with peek that there is a value
                &format!("Unexpected type found {:?}", values_iter.next().unwrap()),
                violations,
            ))
        } else {
            Ok(())
        };
    }
}

/// Implements an `fields` constraint of Ion Schema
/// [fields]: https://amzn.github.io/ion-schema/docs/spec.html#fields
#[derive(Debug, Clone, PartialEq)]
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

    /// Tries to create an [Fields] constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];

        // Create a peekable iterator for given struct
        let ion_struct = match value.as_struct() {
            None => {
                return Err(Violation::with_violations(
                    "fields",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected struct ion found {}",
                        if value.is_null() {
                            format!("{:?}", value)
                        } else {
                            format!("{}", value.ion_type())
                        }
                    ),
                    violations,
                ));
            }
            Some(ion_struct) => ion_struct,
        };

        // Verify if open content exists in the struct fields
        if !self.open_content() {
            for (field_name, value) in ion_struct.iter() {
                if !self.fields.contains_key(field_name.text().unwrap()) {
                    violations.push(Violation::new(
                        "fields",
                        ViolationCode::InvalidOpenContent,
                        &format!(
                            "Found open content in the struct: {:?}: {:?}",
                            field_name, value
                        ),
                    ));
                }
            }
        }

        // get the values corresponding to the field_name and perform occurs_validation based on the type_def
        for (field_name, type_id) in &self.fields {
            let type_def = type_store.get_type_by_id(*type_id).unwrap();
            let values: Vec<&OwnedElement> = ion_struct.get_all(field_name).collect();

            // perform occurs validation for type_def for all values of the given field_name
            let occurs_range: Range = type_def.get_occurs_constraint("fields");

            // verify if values follow occurs_range constraint
            if !occurs_range
                .contains(&(values.len() as i64).into())
                .unwrap()
            {
                violations.push(Violation::new(
                    "fields",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "Expected {:?} of field {:?}: found {}",
                        occurs_range,
                        field_name,
                        values.len()
                    ),
                ));
            }

            // verify if all the values for this field name are valid according to type_def
            for value in values {
                if let Err(violation) = type_def.validate(value, type_store) {
                    violations.push(violation);
                }
            }
        }

        // return error if there were any violation found during validation
        if !violations.is_empty() {
            return Err(Violation::with_violations(
                "fields",
                ViolationCode::FieldsNotMatched,
                "value didn't satisfy fields constraint",
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements Ion Schema's `contains` constraint
/// [contains]: https://amzn.github.io/ion-schema/docs/spec.html#contains
#[derive(Debug, Clone, PartialEq)]
pub struct ContainsConstraint {
    // TODO: convert this into a HashSet once we have an implementation of Hash for OwnedElement in ion-rust
    // Reference ion-rust issue: https://github.com/amzn/ion-rust/issues/220
    values: Vec<OwnedElement>,
}

impl ContainsConstraint {
    pub fn new(values: Vec<OwnedElement>) -> Self {
        Self { values }
    }
}

impl ConstraintValidator for ContainsConstraint {
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        match value.as_sequence() {
            None => {
                // return Violation if value is not an Ion sequence
                return Err(Violation::new(
                    "contains",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected list/sexp found {}",
                        if value.is_null() {
                            format!("{:?}", value)
                        } else {
                            format!("{}", value.ion_type())
                        }
                    ),
                ));
            }
            Some(ion_sequence) => {
                // add all the missing values found during validation
                let mut missing_values = vec![];

                // for each value in expected values if it does not exist in ion sequence
                // then add it to missing_values to keep track of missing values
                for expected_value in self.values.iter() {
                    if !ion_sequence.iter().any(|v| v == expected_value) {
                        missing_values.push(expected_value);
                    }
                }

                // return Violation if there were any values added to the missing values vector
                if !missing_values.is_empty() {
                    return Err(Violation::new(
                        "contains",
                        ViolationCode::MissingValue,
                        &format!("{:?} has missing value(s): {:?}", value, missing_values),
                    ));
                }
            }
        };

        Ok(())
    }
}

/// Implements an `container_length` constraint of Ion Schema
/// [container_length]: https://amzn.github.io/ion-schema/docs/spec.html#container_length
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // Check for null container
        if value.is_null() {
            return Err(Violation::new(
                "container_length",
                ViolationCode::TypeMismatched,
                &format!("expected a container found {:?}", value),
            ));
        }

        // get the size of given value container
        let size = match value.ion_type() {
            IonType::List | IonType::SExpression => value.as_sequence().unwrap().iter().count(),
            IonType::Struct => value.as_struct().unwrap().iter().count(),
            _ => {
                // return Violation if value is not an Ion container
                return Err(Violation::new(
                    "container_length",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected a container (a list/sexp/struct) but found {}",
                        value.ion_type()
                    ),
                ));
            }
        };

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the container size didn't follow container_length constraint
        if !length_range.contains(&(size as i64).into()).unwrap() {
            return Err(Violation::new(
                "container_length",
                ViolationCode::InvalidLength,
                &format!(
                    "expected container length {:?} found {}",
                    length_range, size
                ),
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `byte_length` constraint
/// [byte_length]: https://amzn.github.io/ion-schema/docs/spec.html#byte_length
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // get the size of given bytes
        let size = match value.as_bytes() {
            Some(bytes) => bytes.len(),
            _ => {
                // return Violation if value is not an clob/blob
                return Err(Violation::new(
                    "byte_length",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected a clob/blob but found {}",
                        if value.is_null() {
                            format!("{:?}", value)
                        } else {
                            format!("{}", value.ion_type())
                        }
                    ),
                ));
            }
        };

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the clob/blob size didn't follow byte_length constraint
        if !length_range.contains(&(size as i64).into()).unwrap() {
            return Err(Violation::new(
                "byte_length",
                ViolationCode::InvalidLength,
                &format!("expected byte length {:?} found {}", length_range, size),
            ));
        }

        Ok(())
    }
}

/// Implements an `codepoint_length` constraint of Ion Schema
/// [codepoint_length]: https://amzn.github.io/ion-schema/docs/spec.html#codepoint_length
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // get the size of given string/symbol Unicode codepoints
        let size = match value.as_str() {
            Some(text) => text.chars().count(),
            _ => {
                // return Violation if value is not string/symbol
                return Err(Violation::new(
                    "codepoint_length",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected a string/symbol but found {}",
                        if value.is_null() {
                            format!("{:?}", value)
                        } else {
                            format!("{}", value.ion_type())
                        }
                    ),
                ));
            }
        };

        // get isl length as a range
        let length_range: &Range = self.length();

        // return a Violation if the string/symbol codepoint size didn't follow codepoint_length constraint
        if !length_range.contains(&(size as i64).into()).unwrap() {
            return Err(Violation::new(
                "codepoint_length",
                ViolationCode::InvalidLength,
                &format!(
                    "expected codepoint length {:?} found {}",
                    length_range, size
                ),
            ));
        }

        Ok(())
    }
}

/// Implements the `element` constraint
/// [element]: https://amzn.github.io/ion-schema/docs/spec.html#element
#[derive(Debug, Clone, PartialEq)]
pub struct ElementConstraint {
    type_id: TypeId,
}

impl ElementConstraint {
    pub fn new(type_id: TypeId) -> Self {
        Self { type_id }
    }

    /// Tries to create an element constraint from the given OwnedElement
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let mut violations: Vec<Violation> = vec![];

        // Check for null container
        if value.is_null() {
            return Err(Violation::new(
                "element",
                ViolationCode::TypeMismatched,
                &format!("expected a container but found {:?}", value),
            ));
        }

        // this type_id was validated while creating `ElementConstraint` hence the unwrap here is safe
        let type_def = type_store.get_type_by_id(self.type_id).unwrap();

        // validate each element of the given value container
        match value.ion_type() {
            IonType::List | IonType::SExpression => {
                for val in value.as_sequence().unwrap().iter() {
                    if let Err(violation) = type_def.validate(val, type_store) {
                        violations.push(violation);
                    }
                }
            }
            IonType::Struct => {
                for (field_name, val) in value.as_struct().unwrap().iter() {
                    if let Err(violation) = type_def.validate(val, type_store) {
                        violations.push(violation);
                    }
                }
            }
            _ => {
                // return Violation if value is not an Ion container
                return Err(Violation::new(
                    "element",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected a container (a list/sexp/struct) but found {}",
                        value.ion_type()
                    ),
                ));
            }
        }

        if !violations.is_empty() {
            return Err(Violation::with_violations(
                "element",
                ViolationCode::ElementMismatched,
                "one or more elements don't satisfy element constraint",
                violations,
            ));
        }
        Ok(())
    }
}

/// Implements the `annotations` constraint
/// [annotations]: https://amzn.github.io/ion-schema/docs/spec.html#annotations
// The `required` annotation provided on the list of annotations is not represented here,
// requirement of an annotation is represented in the annotation itself by the field `is_required` of `Annotation` struct.
#[derive(Debug, Clone, PartialEq)]
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
        while value_annotations.peek() != None && !self.is_closed {
            if expected_annotation.value() == value_annotations.next().unwrap() {
                return true;
            }
        }

        // if we didn't find the expected annotation return false
        false
    }

    pub fn validate_ordered_annotations(
        &self,
        value: &OwnedElement,
        type_store: &TypeStore,
        violations: Vec<Violation>,
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
                ));
            }
        }

        if self.is_closed && value_annotations.peek() != None {
            // check if there are still annotations left at the end of the list
            return Err(Violation::with_violations(
                "annotations",
                ViolationCode::AnnotationMismatched,
                // unwrap as we already verified with peek that there is a value
                &format!(
                    "Unexpected annotations found {:?}",
                    value_annotations.next().unwrap()
                ),
                violations,
            ));
        }

        Ok(())
    }

    pub fn validate_unordered_annotations(
        &self,
        value: &OwnedElement,
        type_store: &TypeStore,
        violations: Vec<Violation>,
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
                &format!("missing annotation(s): {:?}", missing_annotations),
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
                violations,
            ));
        }

        Ok(())
    }
}

impl ConstraintValidator for AnnotationsConstraint {
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        let violations: Vec<Violation> = vec![];

        // validate annotations that have list-level `ordered` annotation
        if self.is_ordered {
            return self.validate_ordered_annotations(value, type_store, violations);
        }
        // validate annotations that does not have list-level `ordered` annotation
        self.validate_unordered_annotations(value, type_store, violations)
    }
}

/// Implements Ion Schema's `precision` constraint
/// [precision]: https://amzn.github.io/ion-schema/docs/spec.html#precision
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // get the precision of given decimal
        let value_precision = match value.as_decimal() {
            Some(decimal_value) => decimal_value.precision(),
            _ => {
                // return Violation if value is not decimal
                let error_message = if value.is_null() {
                    format!("expected a decimal but found {:?}", value)
                } else {
                    format!("expected a decimal but found {}", value.ion_type())
                };
                return Err(Violation::new(
                    "precision",
                    ViolationCode::TypeMismatched,
                    &error_message,
                ));
            }
        };

        // get isl decimal precision as a range
        let precision_range: &Range = self.precision();

        // return a Violation if the value didn't follow precision constraint
        if !precision_range
            .contains(&(value_precision as i64).into())
            .unwrap()
        {
            return Err(Violation::new(
                "precision",
                ViolationCode::InvalidLength,
                &format!(
                    "expected precision {:?} found {}",
                    precision_range, value_precision
                ),
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `scale` constraint
/// [scale]: https://amzn.github.io/ion-schema/docs/spec.html#scale
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // get the scale of given decimal
        let value_scale = match value.as_decimal() {
            Some(decimal_value) => decimal_value.scale(),
            _ => {
                // return Violation if value is not decimal
                let error_message = if value.is_null() {
                    format!("expected a decimal but found {:?}", value)
                } else {
                    format!("expected a decimal but found {}", value.ion_type())
                };
                return Err(Violation::new(
                    "scale",
                    ViolationCode::TypeMismatched,
                    &error_message,
                ));
            }
        };

        // get isl decimal scale as a range
        let scale_range: &Range = self.scale();

        // return a Violation if the value didn't follow scale constraint
        if !scale_range.contains(&(value_scale).into()).unwrap() {
            return Err(Violation::new(
                "scale",
                ViolationCode::InvalidLength,
                &format!("expected scale {:?} found {}", scale_range, value_scale),
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `timestamp_precision` constraint
/// [timestamp_precision]: https://amzn.github.io/ion-schema/docs/spec.html#timestamp_precision
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
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        // get the precision of given timestamp
        let timestamp_value = match value.as_timestamp() {
            Some(timestamp_value) => timestamp_value,
            _ => {
                // return Violation if value is not timestamp
                let error_message = if value.is_null() {
                    format!("expected a timestamp but found {:?}", value)
                } else {
                    format!("expected a timestamp but found {}", value.ion_type())
                };
                return Err(Violation::new(
                    "timestamp_precision",
                    ViolationCode::TypeMismatched,
                    &error_message,
                ));
            }
        };

        // get isl timestamp precision as a range
        let precision_range: &Range = self.timestamp_precision();

        // return a Violation if the value didn't follow timestamp precision constraint
        if !precision_range
            .contains(&(timestamp_value.to_owned()).into())
            .unwrap()
        {
            return Err(Violation::new(
                "precision",
                ViolationCode::InvalidLength,
                &format!(
                    "expected precision {:?} found {:?}",
                    precision_range,
                    timestamp_value.precision()
                ),
            ));
        }

        Ok(())
    }
}

/// Implements Ion Schema's `valid_values` constraint
/// [valid_values]: https://amzn.github.io/ion-schema/docs/spec.html#valid_values
#[derive(Debug, Clone, PartialEq)]
pub enum ValidValuesConstraint {
    Range(Range),
    Values(Vec<OwnedElement>),
}

impl ValidValuesConstraint {}

impl ConstraintValidator for ValidValuesConstraint {
    fn validate(&self, value: &OwnedElement, type_store: &TypeStore) -> ValidationResult {
        return match self {
            ValidValuesConstraint::Range(range) => match value.ion_type() {
                IonType::Integer | IonType::Float | IonType::Decimal | IonType::Timestamp => {
                    if range.contains(value).unwrap() {
                        Ok(())
                    } else {
                        Err(Violation::new(
                            "valid_values",
                            ViolationCode::InvalidValue,
                            &format!(
                                "expected valid_values to be from {:?} found {:?}",
                                &self, value
                            ),
                        ))
                    }
                }
                _ => Err(Violation::new(
                    "valid_values",
                    ViolationCode::TypeMismatched,
                    &format!(
                        "expected valid_values to be a range or list but found {:?}",
                        value.ion_type()
                    ),
                )),
            },
            ValidValuesConstraint::Values(values) => {
                // get value without annotations
                let value = value.to_owned().with_annotations(vec![]);

                if values.contains(&value) {
                    Ok(())
                } else {
                    Err(Violation::new(
                        "valid_values",
                        ViolationCode::InvalidValue,
                        &format!(
                            "expected valid_values to be from {:?}, found {:?}",
                            &self, value
                        ),
                    ))
                }
            }
        };
    }
}
