use crate::isl::isl_type_reference::IslTypeRef;

/// Represents a public facing API for schema constraints [IslConstraint] which stores IslTypeRef
#[derive(Debug, Clone, PartialEq)]
pub enum IslConstraint {
    AllOf(Vec<IslTypeRef>),
    Type(IslTypeRef),
}
