//! Provides a way to construct ISL types and constraints programmatically.
//!
//! This module consists of three submodules that help constructing ISL types/constraint:
//!
//! * `constraints` module provides schema constraints [IslConstraint]
//! which converts given ion content in the schema file into an ISL constraint(not-yet-resolved constraints).
//!
//! [IslConstraint]: IslConstraint
//!
//! ## Example usage of `version_based_isl` module to create an `IslSchema` using `IslType` for ISl 1.0:
//!```
//! use ion_rs::element::Element;
//! use ion_schema::isl::version_based_isl::constraints::IslConstraint;
//! use ion_schema::isl::version_based_isl::{IslSchema, IslSchemaHeader, IslTypeArgument, IslV1_0, UserReservedFields};
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
//! //      ],
//! //      open_content: "This is an open content!"
//! //  }
//! let isl_type: IslType<IslV1_0> = IslType::named(
//!     // represents the `name` of the defined type
//!     "my_type_name".to_owned(),
//!     vec![
//!         // represents the `type: int` constraint
//!         IslConstraint::type_constraint(
//!             IslTypeArgument::named("int")
//!         ),
//!         // represents `all_of` with anonymous type `{ type: bool }` constraint
//!         IslConstraint::all_of(
//!             vec![
//!                 IslTypeArgument::anonymous(
//!                     vec![
//!                         IslConstraint::type_constraint(
//!                             IslTypeArgument::named("bool")
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
//! let isl_schema_header = IslSchemaHeader::<IslV1_0>::new(vec![], vec![]);
//! let mut  isl_schema = IslSchema::new("my_schema", vec![isl_type], vec![], vec![]);
//! isl_schema = isl_schema.with_schema_header(isl_schema_header);
//!
//! assert_eq!(isl_schema.types().len(), 1);
//! assert_eq!(isl_schema.types()[0].name(), &Some("my_type_name".to_owned()));
//! assert_eq!(isl_schema.types()[0].constraints().len(), 3);
//! // verify that the last constraint is actually an open content field
//! assert!(matches!(isl_schema.types()[0].constraints()[2], IslConstraint::Unknown(_)));
//! ```
//!
//! ## Example usage of `version_based_isl` module to create an `IslSchema` using `IslType` for ISL 2.0:
//! ```
//! use ion_rs::element::Element;
//! use ion_schema::isl::version_based_isl::constraints::IslConstraint;
//! use ion_schema::isl::version_based_isl::{IslSchema, IslSchemaHeader, IslTypeArgument, IslV2_0, UserReservedFields};
//! use ion_schema::isl::version_based_isl::constraints::open_content::OpenContent;
//! use crate::ion_schema::isl::version_based_isl::IslType;
//! use crate::ion_schema::isl::version_based_isl::constraints::r#type::Type;
//!
//! // below code represents an ISL type:
//! // type:: {
//! //      name:my_type_name,
//! //      type: int,
//! //  }
//! let isl_type: IslType<IslV2_0> = IslType::named(
//!     // represents the `name` of the defined type
//!     "my_type_name".to_owned(),
//!     vec![
//!         // represents the `type: int` constraint
//!         IslConstraint::type_constraint(
//!             IslTypeArgument::named("int")
//!         ),
//!     ]
//! );
//!
//! let mut isl_schema_header =  IslSchemaHeader::<IslV2_0>::new(vec![], vec![]); // no open content or imports are there in schema header
//! // Performing the following operation with ISL 1.0 schema would result in a compile time error.
//! isl_schema_header = isl_schema_header.with_user_reserved_fields(vec!["foo".to_string(), "bar".to_string()], vec![], vec![]);
//!
//! // create an ISL schema using above IslType
//! let mut  isl_schema = IslSchema::new("my_schema", vec![isl_type], vec![], vec![]);
//!isl_schema = isl_schema.with_schema_header(isl_schema_header);
//!
//! assert_eq!(isl_schema.types().len(), 1);
//! // verify that the schema has user reserved fields
//! assert_eq!(isl_schema.schema_header().as_ref().unwrap().user_reserved_fields().schema_header_fields().len(), 2);
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

/// Represents user reserved fields that declares reserved symbols to be used for open content fields.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct UserReservedFields {
    schema_header_fields: Vec<String>,
    schema_footer_fields: Vec<String>,
    type_fields: Vec<String>,
}

impl UserReservedFields {
    pub fn new(
        schema_header_fields: Vec<String>,
        schema_footer_fields: Vec<String>,
        type_fields: Vec<String>,
    ) -> Self {
        Self {
            schema_header_fields,
            schema_footer_fields,
            type_fields,
        }
    }

    pub fn schema_header_fields(&self) -> &Vec<String> {
        &self.schema_header_fields
    }

    pub fn schema_footer_fields(&self) -> &Vec<String> {
        &self.schema_footer_fields
    }

    pub fn type_fields(&self) -> &Vec<String> {
        &self.type_fields
    }
}

/// Represents a schema header
#[derive(Debug, Clone)]
pub struct IslSchemaHeader<V: IslVersionTrait> {
    /// Represents open content as `Element`s in the schema header
    open_content: Vec<Element>,
    /// Represents all the IslImports inside the schema file.
    /// For more information: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports
    imports: Vec<IslImport>,
    /// Represents the use reserved fields that are defined within schema header,
    user_reserved_fields: UserReservedFields,
    _version: PhantomData<V>,
}

impl<V: IslVersionTrait> IslSchemaHeader<V> {
    pub fn open_content(&self) -> &[Element] {
        &self.open_content
    }

    pub fn imports(&self) -> &[IslImport] {
        &self.imports
    }
}

impl IslSchemaHeader<IslV1_0> {
    pub fn new(open_content: Vec<Element>, imports: Vec<IslImport>) -> Self {
        Self {
            open_content,
            imports,
            user_reserved_fields: UserReservedFields::default(),
            _version: Default::default(),
        }
    }
}

impl IslSchemaHeader<IslV2_0> {
    pub fn new(open_content: Vec<Element>, imports: Vec<IslImport>) -> Self {
        Self {
            open_content,
            imports,
            user_reserved_fields: UserReservedFields::default(),
            _version: Default::default(),
        }
    }

    pub fn with_user_reserved_fields(
        self,
        schema_header_fields: Vec<String>,
        schema_footer_fields: Vec<String>,
        type_fields: Vec<String>,
    ) -> Self {
        Self {
            user_reserved_fields: UserReservedFields {
                schema_header_fields,
                schema_footer_fields,
                type_fields,
            },
            ..self
        }
    }

    pub fn user_reserved_fields(&self) -> &UserReservedFields {
        &self.user_reserved_fields
    }
}

/// Represents a schema footer
#[derive(Debug, Clone)]
pub struct IslSchemaFooter {
    /// Represents open content as `Element`s in the schema footer
    open_content: Vec<Element>,
}

impl IslSchemaFooter {
    pub fn new(open_content: Vec<Element>) -> Self {
        Self { open_content }
    }
}

/// Represents a schema file
#[derive(Debug, Clone)]
pub struct IslSchema<V: IslVersionTrait> {
    /// Represents an id for the given ISL model
    id: String,
    /// Represents the schema header if its present which contains imports and any open content provided in the header.
    /// For ISL 2.0 schema header also contains user reserved fields.
    /// If the schema header is not present this is None.
    schema_header: Option<IslSchemaHeader<V>>,
    /// Represents the schema footer if its present which contains any open content provided in the footer.
    /// If the schema footer is not present this is None.
    schema_footer: Option<IslSchemaFooter>,
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
    _version: PhantomData<V>,
}

impl<V: IslVersionTrait> IslSchema<V> {
    pub fn new<A: AsRef<str>>(
        id: A,
        types: Vec<IslType<V>>,
        inline_imports: Vec<IslImportType>,
        open_content: Vec<Element>,
    ) -> Self {
        Self {
            id: id.as_ref().to_owned(),
            schema_header: None,
            schema_footer: None,
            types,
            inline_imported_types: inline_imports,
            open_content,
            _version: Default::default(),
        }
    }

    pub fn with_schema_header(self, schema_header: IslSchemaHeader<V>) -> Self {
        Self {
            schema_header: Some(schema_header),
            ..self
        }
    }

    pub fn with_schema_footer(self, schema_footer: IslSchemaFooter) -> Self {
        Self {
            schema_footer: Some(schema_footer),
            ..self
        }
    }

    pub fn id(&self) -> String {
        self.id.to_owned()
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

    /// Provides the schema header value if present,
    /// Otherwise returns None.
    pub fn schema_header(&self) -> &Option<IslSchemaHeader<V>> {
        &self.schema_header
    }

    /// Provides the schema footer value if present,
    /// Otherwise returns None.
    pub fn schema_footer(&self) -> &Option<IslSchemaFooter> {
        &self.schema_footer
    }
}

/// Represents both named and anonymous [IslType]s and can be converted to a resolved type definition
/// Named ISL type grammar: `type:: { name: <NAME>, <CONSTRAINT>...}`
/// Anonymous ISL type grammar: `{ <CONSTRAINT>... }`
#[derive(Debug, Clone, PartialEq)]
pub struct IslType<V: IslVersionTrait> {
    name: Option<String>,
    constraints: Vec<IslConstraint<V>>,
    /// Represents the ISL type struct in string format for anonymous type definition
    /// For named type definition & programmatically created type definition, this will be `None`
    pub(crate) isl_type_struct: Option<Element>,
    _version: PhantomData<V>,
}

impl<V: IslVersionTrait> IslType<V> {
    pub(crate) fn new(name: Option<String>, constraints: Vec<IslConstraint<V>>) -> Self {
        Self {
            name,
            constraints,
            isl_type_struct: None,
            _version: Default::default(),
        }
    }

    /// Creates a named [IslType] using the [IslConstraint] defined within it
    pub fn named<A: Into<String>, B: Into<Vec<IslConstraint<V>>>>(name: A, constraints: B) -> Self {
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
pub enum IslTypeArgument<V: IslVersionTrait> {
    /// Represents a reference to a named type (including aliases and built-in types)
    Named(String, NullabilityModifier),
    /// Represents a type reference defined as an inlined import of a type from another schema
    TypeImport(IslImportType, NullabilityModifier),
    /// represents an unnamed type definition reference
    Anonymous(IslType<V>, NullabilityModifier),
}

impl<V: IslVersionTrait> IslTypeArgument<V> {
    /// Creates a named [IslTypeArgument] using the name of the type referenced inside it
    pub fn named<A: Into<String>>(name: A) -> IslTypeArgument<V> {
        IslTypeArgument::Named(name.into(), NullabilityModifier::Nothing)
    }

    /// Creates an anonymous [IslTypeArgument] using the [IslConstraint]s referenced inside it
    pub fn anonymous<A: Into<Vec<IslConstraint<V>>>>(constraints: A) -> Self {
        IslTypeArgument::Anonymous(
            IslType::new(None, constraints.into()),
            NullabilityModifier::Nothing,
        )
    }
}

impl IslTypeArgument<IslV1_0> {
    /// Creates a nullable [IslTypeArgument] using the [IonType] referenced inside it
    pub fn nullable_built_in(name: IonType) -> IslTypeArgument<IslV1_0> {
        IslTypeArgument::Named(format!("{name}"), NullabilityModifier::Nullable)
    }
}

impl IslTypeArgument<IslV2_0> {
    /// Creates a nullable [IslTypeArgument] using the name of the type referenced inside it for ISL 2.0
    pub fn null_or_named_type_ref<A: Into<String>>(name: A) -> IslTypeArgument<IslV2_0> {
        IslTypeArgument::Named(name.into(), NullabilityModifier::NullOr)
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
pub struct IslVariablyOccurringTypeArgument<V: IslVersionTrait> {
    type_arg: IslTypeArgument<V>,
    occurs: Range,
}

impl<V: IslVersionTrait> IslVariablyOccurringTypeArgument<V> {
    pub(crate) fn new(type_arg: IslTypeArgument<V>, occurs: Range) -> Self {
        Self { type_arg, occurs }
    }

    /// Creates an [IslVariablyOccurringTypeArgument] using the [IslConstraint]s and [Range] referenced inside it
    pub fn variably_occurring(
        type_arg: IslTypeArgument<V>,
        occurs: Range,
    ) -> IslVariablyOccurringTypeArgument<V> {
        IslVariablyOccurringTypeArgument::new(type_arg, occurs)
    }
}

/// Represents an [import] in an ISL schema.
///
/// [import]: https://amazon-ion.github.io/ion-schema/docs/isl-1-0/spec#imports
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IslImport {
    Schema(String),
    Type(IslImportType),
}

/// Represents typed and type aliased [IslImport]s
/// Typed import grammar: `{ id: <ID>, type: <TYPE_NAME> }`
/// Type aliased import grammar: `{ id: <ID>, type: <TYPE_NAME>, as: <TYPE_ALIAS> }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IslImportType {
    schema_id: String,
    type_name: String,
    alias: Option<String>,
}
