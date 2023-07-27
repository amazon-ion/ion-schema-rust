use crate::isl::version_based_isl::constraints::all_of::AllOf;
use crate::isl::version_based_isl::constraints::any_of::AnyOf;
use crate::isl::version_based_isl::constraints::not::Not;
use crate::isl::version_based_isl::constraints::one_of::OneOf;
use crate::isl::version_based_isl::constraints::open_content::OpenContent;
use crate::isl::version_based_isl::constraints::r#type::Type;
use crate::isl::version_based_isl::{IslTypeArgument, IslVersionTrait};
use ion_rs::element::Element;

pub mod all_of;
pub mod any_of;
pub mod not;
pub mod one_of;
pub mod open_content;
pub mod r#type;

#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint<V: IslVersionTrait> {
    AllOf(AllOf<V>),
    AnyOf(AnyOf<V>),
    OneOf(OneOf<V>),
    Not(Not<V>),
    Type(Type<V>),
    OpenContent(OpenContent),
}

impl<V: IslVersionTrait> IslConstraint<V> {
    /// Creates a `type` constraint using the [IslTypeArgument] referenced inside it
    // type is rust keyword hence this method is named type_constraint unlike other ISL constraint methods
    pub fn type_constraint(isl_type_ref: IslTypeArgument<V>) -> IslConstraint<V> {
        IslConstraint::Type(Type::new(isl_type_ref))
    }

    /// Creates an `all_of` constraint using the [IslTypeArgument] referenced inside it
    pub fn all_of<A: Into<Vec<IslTypeArgument<V>>>>(isl_type_refs: A) -> IslConstraint<V> {
        IslConstraint::AllOf(AllOf::new(isl_type_refs.into()))
    }

    /// Creates an `any_of` constraint using the [IslTypeArgument] referenced inside it
    pub fn any_of<A: Into<Vec<IslTypeArgument<V>>>>(isl_type_refs: A) -> IslConstraint<V> {
        IslConstraint::AnyOf(AnyOf::new(isl_type_refs.into()))
    }

    /// Creates a `one_of` constraint using the [IslTypeArgument] referenced inside it
    pub fn one_of<A: Into<Vec<IslTypeArgument<V>>>>(isl_type_refs: A) -> IslConstraint<V> {
        IslConstraint::OneOf(OneOf::new(isl_type_refs.into()))
    }

    /// Creates a `not` constraint using the [IslTypeArgument] referenced inside it
    pub fn not(isl_type_ref: IslTypeArgument<V>) -> IslConstraint<V> {
        IslConstraint::Not(Not::new(isl_type_ref))
    }

    /// Creates open content using the field name and value referenced inside it
    /// Note: This open content has no effect
    pub fn open_content(field_name: String, field_value: Element) -> IslConstraint<V> {
        IslConstraint::OpenContent(OpenContent::new(field_name, field_value))
    }
}
