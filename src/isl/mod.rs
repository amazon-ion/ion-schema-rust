pub mod isl_constraint;
pub mod isl_type;
pub mod isl_type_reference;

/// Provides a way to construct ISL types/constraints programmatically into an internal representation.
///
/// This module consists of three submodules that implement public facing API for constructing ISL types/constraint:
///
/// * `isl_type` module represents a public facing schema type [IslType] which converts given ion content in the schema file
/// into an internal representation of ISL types(not-yet-resolved types). It stores [IslConstraint]s defined within the given type.
/// Also, this eventually gets converted to a solid [TypeRef](which contains a unique TypeId for the given type
/// and a resolved type TypeDefinition) using [TypeStore].
///
/// * `isl_constraint` module represents a public facing API for schema constraints [IslConstraint]
/// which converts given ion content in the schema file into an internal representation of ISL constraint(not-yet-resolved constraints).
/// This stores [IslTypeRef]s for the given ISL constraint.
///
/// * `isl_type_reference` module provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]: https://amzn.github.io/ion-schema/docs/spec.html#grammar
///
/// The given schema is loaded with a two phase approach:
/// 1. Phase 1: Constructing an internal representation of ISL types/constraints from given schema file.
///             This phase creates all [IslType],  [IslConstraint], [IslTypeRef] structs from the ion content in schema file.
/// 2. Phase 2: Constructing resolved types/constraints from internal representation of ISL types/constraints(not-yet-resolved types/constraints).
///             This is done by loading all types into [Schema] as below:
///                 a. Convert all [IslType] → [TypeDefinition], [IslConstraint] → [Constraint], [IslTypeRef] → [TypeDefinition]
///                 b. While doing (a) store all [TypeDefinition] in the [TypeStore](which could help
///                    returning resolved types in a schema) and store generated [TypeId] in the constraint.

#[cfg(test)]
mod isl_tests {
    use crate::isl::isl_constraint::IslConstraint;
    use crate::isl::isl_type::{AnonymousIslType, IslType, NamedIslType};
    use crate::isl::isl_type_reference::IslTypeRef;
    use ion_rs::value::reader::element_reader;
    use ion_rs::value::reader::ElementReader;
    use ion_rs::IonType;
    use rstest::*;

    // helper function to create NamedIslType for isl tests
    fn load_named_type(text: &str) -> IslType {
        IslType::NamedIslType(
            NamedIslType::parse_from_owned_element(
                &element_reader()
                    .read_one(text.as_bytes())
                    .expect("parsing failed unexpectedly"),
            )
            .unwrap(),
        )
    }

    // helper function to create AnonymousIslType for isl tests
    fn load_anonymous_type(text: &str) -> IslType {
        IslType::AnonymousIslType(
            AnonymousIslType::parse_from_owned_element(
                &element_reader()
                    .read_one(text.as_bytes())
                    .expect("parsing failed unexpectedly"),
            )
            .unwrap(),
        )
    }

    #[rstest(
    isl_type1,isl_type2,
    case::type_constraint_with_anonymous_type(
        load_anonymous_type(r#" // For a schema with single anonymous type
                {type: int}
            "#),
        IslType::AnonymousIslType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))
    ),
    case::type_constraint_with_named_type(
        load_named_type(r#" // For a schema with named type
                type:: { name: my_int, type: int }
            "#),
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))
    ),
    case::type_constraint_with_self_reference_type(
        load_named_type(r#" // For a schema with self reference type
                type:: { name: my_int, type: my_int }
            "#),
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))]))
    ),
    case::type_constraint_with_nested_self_reference_type(
        load_named_type(r#" // For a schema with nested self reference type
                type:: { name: my_int, type: { type: my_int } }
            "#),
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))]))
    ),
    case::type_constraint_with_nested_type(
        load_named_type(r#" // For a schema with nested types
                type:: { name: my_int, type: { type: int } }
            "#),
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))])))]))
    ),
    case::type_constraint_with_nested_multiple_types(
        load_named_type(r#" // For a schema with nested multiple types
                type:: { name: my_int, type: { type: int }, type: { type: my_int } }
            "#),
        IslType::NamedIslType(NamedIslType::new("my_int".to_owned(), vec![IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))), IslConstraint::Type(IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::NamedType("my_int".to_owned()))])))]))
    ),
    case::all_of_constraint(
        load_anonymous_type(r#" // For a schema with all_of type as below:
                { all_of: [{ type: int }] }
            "#),
        IslType::AnonymousIslType(AnonymousIslType::new(vec![IslConstraint::AllOf(vec![IslTypeRef::AnonymousType(AnonymousIslType::new(vec![IslConstraint::Type(IslTypeRef::CoreIslType(IonType::Integer))]))])]))
    ),
    )]
    fn owned_struct_to_isl_type(isl_type1: IslType, isl_type2: IslType) {
        // assert if both the IslType are same in terms of constraints and name
        assert_eq!(isl_type1, isl_type2);
    }
}
