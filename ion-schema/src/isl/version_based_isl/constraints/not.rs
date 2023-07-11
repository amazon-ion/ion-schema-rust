use crate::isl::version_based_isl::{IslTypeArgument, IslVersionTrait};

#[derive(Debug, Clone, PartialEq)]
pub struct Not<V: IslVersionTrait> {
    type_arg: IslTypeArgument<V>,
}

impl<V: IslVersionTrait> Not<V> {
    pub fn new(type_arg: IslTypeArgument<V>) -> Self {
        Self { type_arg }
    }

    pub fn type_arg(&self) -> &IslTypeArgument<V> {
        &self.type_arg
    }
}
