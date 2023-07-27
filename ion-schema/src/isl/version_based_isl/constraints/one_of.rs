use crate::isl::version_based_isl::{IslTypeArgument, IslVersionTrait};

#[derive(Debug, Clone, PartialEq)]
pub struct OneOf<V: IslVersionTrait> {
    type_args: Vec<IslTypeArgument<V>>,
}

impl<V: IslVersionTrait> OneOf<V> {
    pub fn new(type_args: Vec<IslTypeArgument<V>>) -> Self {
        Self { type_args }
    }

    pub fn type_args(&self) -> &[IslTypeArgument<V>] {
        &self.type_args
    }
}
