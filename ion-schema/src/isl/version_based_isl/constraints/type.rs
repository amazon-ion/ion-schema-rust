use crate::isl::version_based_isl::{IslTypeRef, IslVersionTrait};

#[derive(Debug, Clone, PartialEq)]
pub struct Type<V: IslVersionTrait> {
    type_ref: IslTypeRef<V>,
}

impl<V: IslVersionTrait> Type<V> {
    pub fn new(type_ref: IslTypeRef<V>) -> Self {
        Self { type_ref }
    }

    pub fn type_ref(&self) -> &IslTypeRef<V> {
        &self.type_ref
    }
}
