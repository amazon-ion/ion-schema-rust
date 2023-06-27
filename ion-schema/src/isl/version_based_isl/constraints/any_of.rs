use crate::isl::version_based_isl::{IslTypeRef, IslVersionTrait};

#[derive(Debug, Clone, PartialEq)]
pub struct AnyOf<V: IslVersionTrait> {
    type_refs: Vec<IslTypeRef<V>>,
}

impl<V: IslVersionTrait> AnyOf<V> {
    pub fn new(type_refs: Vec<IslTypeRef<V>>) -> Self {
        Self { type_refs }
    }

    pub fn type_refs(&self) -> &Vec<IslTypeRef<V>> {
        &self.type_refs
    }
}
