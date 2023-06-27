//! Provides a way to construct ISL types and constraints programmatically.
//!
//! This module consists of three submodules that help constructing ISL types/constraint:
//!
//! * `constraints` module provides schema constraints [IslConstraint]
//! which converts given ion content in the schema file into an ISL constraint(not-yet-resolved constraints).
//!
//! [IslConstraint]: IslConstraint
//!
//! ## Example usage of `version_based_isl` module to create an `IslSchema` using `IslType`:
//! ```
//! use ion_rs::element::Element;
//! use ion_schema::isl::version_based_isl::constraints::IslConstraint;
//! use ion_schema::isl::version_based_isl::{IslSchema, IslTypeRef, IslV1_0};
//! use ion_schema::isl::version_based_isl::constraints::open_content::OpenContent;
//! use crate::ion_schema::isl::version_based_isl::IslType;
//! use crate::ion_schema::isl::version_based_isl::constraints::r#type::Type;
//!
//! // below code represents an ISL type:
//! // type:: {
//! //      name:my_type_name,
//! //      type: int,
//! //      all_of: [
//! //          { type: bool }
//! //      ]
//! //  }
//! let isl_type: IslType<IslV1_0> = IslType::named(
//!     // represents the `name` of the defined type
//!     "my_type_name".to_owned(),
//!     vec![
//!         // represents the `type: int` constraint
//!         IslConstraint::type_constraint(
//!             IslTypeRef::named("int")
//!         ),
//!         // represents `all_of` with anonymous type `{ type: bool }` constraint
//!         IslConstraint::all_of(
//!             vec![
//!                 IslTypeRef::anonymous(
//!                     vec![
//!                         IslConstraint::type_constraint(
//!                             IslTypeRef::named("bool")
//!                         )
//!                     ]
//!                 )
//!             ]
//!         ),
//!         // represents some open content which will be ignored while doing validation
//!         IslConstraint::open_content("open_content".to_owned(), Element::string("This is an open content!"))
//!     ]
//! );
//!
//! // create an ISL schema using above IslType
//! let isl_schema = IslSchema::new("my_schema", vec![], vec![isl_type], vec![], vec![]);
//! assert_eq!(isl_schema.types().len(), 1);
//! assert_eq!(isl_schema.types()[0].name(), &Some("my_type_name".to_owned()));
//! assert_eq!(isl_schema.types()[0].constraints().len(), 3);
//! // verify that the last constraint is actually an open content field
//! assert!(matches!(isl_schema.types()[0].constraints()[2], IslConstraint::Unknown(_)));
//! ```

use crate::isl::isl_range::Range;
use crate::isl::isl_type_reference::NullabilityModifier;
use crate::isl::version_based_isl::constraints::IslConstraint;
use ion_rs::element::Element;
use ion_rs::IonType;
use std::marker::PhantomData;

pub mod constraints;

pub trait IslVersionTrait {}

pub struct IslV1_0 {}
impl IslVersionTrait for IslV1_0 {}

// TODO: This can be a trait if we have get a new minor version of ISL
#[derive(Debug, Clone)]
pub struct IslV2_0 {}
impl IslVersionTrait for IslV2_0 {}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserReservedFields<V: IslVersionTrait> {
    schema_header_fields: Vec<String>,
    schema_footer_fields: Vec<String>,
    type_fields: Vec<String>,
    phantom: PhantomData<V>,
}

impl UserReservedFields<IslV2_0> {
    pub(crate) fn new(
        schema_header_fields: Vec<String>,
        schema_footer_fields: Vec<String>,
        type_fields: Vec<String>,
    ) -> Self {
        Self {
            schema_header_fields,
            schema_footer_fields,
            type_fields,
            phantom: Default::default(),
        }
    }
}

/// Provides an internal representation of an schema file
#[derive(Debug, Clone)]
pub struct IslSchema<V: IslVersionTrait> {
    /// Represents an id for the given ISL model
    id: String,
    /// Represents the user defined reserved fields
    /// For ISL 2.0 this contains the use reserved fields that are defined within schema header,
    /// Otherwise, it is None.
    user_reserved_fields: Option<UserReservedFields<IslV2_0>>,
    /// Represents all the IslImports inside the schema file.
    /// For more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports
    imports: Vec<IslImport>,
    /// Represents all the IslType defined in this schema file.
    /// For more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#type-definitions
    types: Vec<IslType<V>>,
    /// Represents all the inline IslImportTypes in this schema file.
    inline_imported_types: Vec<IslImportType>,
    /// Represents open content as `Element`s
    /// Note: This doesn't preserve the information about where the open content is placed in the schema file.
    /// e.g. This method doesn't differentiate between following schemas with open content:
    /// ```ion
    /// $foo
    /// $bar
    /// type::{ name: foo, codepoint_length: 1 }
    /// ```
    ///
    /// ```ion
    /// type::{ name: foo, codepoint_length: 1 }
    /// $foo
    /// $bar
    /// ```
    open_content: Vec<Element>,
    phantom: PhantomData<V>,
}

impl<V: IslVersionTrait> IslSchema<V> {
    pub fn new<A: AsRef<str>>(
        id: A,
        imports: Vec<IslImport>,
        types: Vec<IslType<V>>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> Self {
        Self {
            id: id.as_ref().to_owned(),
            user_reserved_fields: None,
            imports,
            types,
            inline_imported_types: inline_imports,
            open_content,
            phantom: Default::default(),
        }
    }

    pub fn with_user_reserved_fields(
        self,
        user_reserved_fields: UserReservedFields<IslV2_0>,
    ) -> Self {
        Self {
            id: self.id,
            user_reserved_fields: Some(user_reserved_fields),
            imports: self.imports,
            types: self.types,
            inline_imported_types: self.inline_imported_types,
            open_content: self.open_content,
            phantom: Default::default(),
        }
    }

    pub fn id(&self) -> String {
        self.id.to_owned()
    }

    pub fn imports(&self) -> &[IslImport] {
        &self.imports
    }

    pub fn types(&self) -> &[IslType<V>] {
        &self.types
    }

    pub fn inline_imported_types(&self) -> &[IslImportType] {
        &self.inline_imported_types
    }

    /// Provides top level open content for given schema
    /// For open content defined within type definitions use IslType#open_content()
    pub fn open_content(&self) -> &Vec<Element> {
        &self.open_content
    }

    /// Provide user reserved field defined in the given schema for ISL 2.0,
    /// Otherwise returns None
    pub fn user_reserved_fields(&self) -> Option<&UserReservedFields<IslV2_0>> {
        self.user_reserved_fields.as_ref()
    }
}

/// Represents both named and anonymous [IslType]s and can be converted to a resolved type definition
/// Named ISL type grammar: `type:: { name: <NAME>, <CONSTRAINT>...}`
/// Anonymous ISL type grammar: `{ <CONSTRAINT>... }`
#[derive(Debug, Clone, PartialEq)]
pub struct IslType<V: IslVersionTrait> {
    name: Option<String>,
    constraints: Vec<IslConstraint<V>>,
    // Represents the ISL type struct in string format for anonymous type definition
    // For named type definition & programmatically created type definition, this will be `None`
    pub(crate) isl_type_struct: Option<Element>,
    phantom: PhantomData<V>,
}

impl<V: IslVersionTrait> IslType<V> {
    pub(crate) fn new(name: Option<String>, constraints: Vec<IslConstraint<V>>) -> Self {
        Self {
            name,
            constraints,
            isl_type_struct: None,
            phantom: Default::default(),
        }
    }

    /// Creates a named [IslType] using the [IslConstraint] defined within it
    pub fn named<A: Into<String>, B: Into<Vec<IslConstraint<V>>>>(
        name: A,
        constraints: B,
    ) -> IslType<V> {
        IslType::new(Some(name.into()), constraints.into())
    }

    /// Creates an anonymous [IslType] using the [IslConstraint] defined within it
    pub fn anonymous<A: Into<Vec<IslConstraint<V>>>>(constraints: A) -> IslType<V> {
        IslType::new(None, constraints.into())
    }

    /// Provides a name if the ISL type is named type definition
    /// Otherwise returns None
    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    /// Provides open content that is there in the type definition
    pub fn open_content(&self) -> Vec<(&String, &Element)> {
        let mut open_contents = vec![];
        for constraint in &self.constraints {
            if let IslConstraint::Unknown(open_content) = constraint {
                open_contents.push((open_content.field_name(), open_content.field_value()))
            }
        }
        open_contents
    }

    /// Provides the underlying constraints of [IslType]
    pub fn constraints(&self) -> &[IslConstraint<V>] {
        &self.constraints
    }
}

/// Provides an internal representation of a schema type reference.
/// The type reference grammar is defined in the [Ion Schema Spec]
///
/// [Ion Schema spec]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#grammar
#[derive(Debug, Clone, PartialEq)]
pub enum IslTypeRef<V: IslVersionTrait> {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String, NullabilityModifier),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType, NullabilityModifier),
    /// represents an unnamed type definition reference
    Anonymous(IslType<V>, NullabilityModifier),
}

impl<V: IslVersionTrait> IslTypeRef<V> {
    /// Creates a named [IslTypeRef] using the name of the type referenced inside it
    pub fn named<A: Into<String>>(name: A) -> IslTypeRef<V> {
        IslTypeRef::Named(name.into(), NullabilityModifier::Nothing)
    }

    /// Creates a nullable [IslTypeRef] using the [IonType] referenced inside it
    pub fn nullable_built_in(name: IonType) -> IslTypeRef<IslV1_0> {
        IslTypeRef::Named(format!("{name}"), NullabilityModifier::Nullable)
    }

    /// Creates an anonymous [IslTypeRef] using the [IslConstraint]s referenced inside it
    pub fn anonymous<A: Into<Vec<IslConstraint<V>>>>(constraints: A) -> IslTypeRef<V> {
        IslTypeRef::Anonymous(
            IslType::new(None, constraints.into()),
            NullabilityModifier::Nothing,
        )
    }

    /// Creates an [IslVariablyOccurringTypeRef] using the [IslConstraint]s and [Range] referenced inside it
    pub fn variably_occurring(
        type_ref: IslTypeRef<V>,
        occurs: Range,
    ) -> IslVariablyOccurringTypeRef<V> {
        IslVariablyOccurringTypeRef::new(type_ref, occurs)
    }

    /// Creates a nullable [IslTypeRef] using the name of the type referenced inside it for ISL 2.0
    pub fn null_or_named_type_ref<A: Into<String>>(name: A) -> IslTypeRef<IslV2_0> {
        IslTypeRef::Named(name.into(), NullabilityModifier::NullOr)
    }
}

/// Represents a [variably occurring type reference] that will be used by `ordered_elements` and `fields` constraints.
///  
/// ```ion
/// Grammar: <VARIABLY_OCCURRING_TYPE_ARGUMENT> ::= { <OCCURS>, <CONSTRAINT>... }
///                                      | <TYPE_ARGUMENT>
///
///         <OCCURS> ::= occurs: <INT>
///            | occurs: <RANGE_INT>
///            | occurs: optional
///            | occurs: required
/// ```
///
/// [variably occurring type reference]: https://amazon-ion.github.io/ion-schema/docs/isl-2-0/spec#variably-occurring-type-arguments
#[derive(Debug, Clone, PartialEq)]
pub struct IslVariablyOccurringTypeRef<V: IslVersionTrait> {
    type_ref: IslTypeRef<V>,
    occurs: Range,
}

impl<V: IslVersionTrait> IslVariablyOccurringTypeRef<V> {
    pub(crate) fn new(type_ref: IslTypeRef<V>, occurs: Range) -> Self {
        Self { type_ref, occurs }
    }
}

/// Represents an [import] in an ISL schema.
///
/// [import]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IslImport {
    Schema(String),
    Type(IslImportType),
    TypeAlias(IslImportType),
}

/// Represents typed and type aliased [IslImport]s
/// Typed import grammar: `{ id: <ID>, type: <TYPE_NAME> }`
/// Type aliased import grammar: `{ id: <ID>, type: <TYPE_NAME>, as: <TYPE_ALIAS> }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IslImportType {
    id: String,
    type_name: String,
    alias: Option<String>,
}
