use crate::isl::version_based_isl::IslVersionTrait;
use ion_rs::element::Element;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq)]
pub struct OpenContent<V: IslVersionTrait> {
    field_name: String,
    field_value: Element,
    phantom: PhantomData<V>,
}

impl<V: IslVersionTrait> OpenContent<V> {
    pub fn new(field_name: String, field_value: Element) -> OpenContent<V> {
        OpenContent {
            field_name,
            field_value,
            phantom: Default::default(),
        }
    }

    pub fn field_name(&self) -> &String {
        &self.field_name
    }
    pub fn field_value(&self) -> &Element {
        &self.field_value
    }
}
