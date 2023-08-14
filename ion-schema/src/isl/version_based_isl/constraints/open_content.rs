use ion_rs::element::Element;

#[derive(Debug, Clone, PartialEq)]
pub struct OpenContent {
    field_name: String,
    field_value: Element,
}

impl OpenContent {
    pub fn new(field_name: String, field_value: Element) -> OpenContent {
        OpenContent {
            field_name,
            field_value,
        }
    }

    pub fn field_name(&self) -> &String {
        &self.field_name
    }
    pub fn field_value(&self) -> &Element {
        &self.field_value
    }
}
