use ion_rs::*;
use std::ops::Deref;

// Represents a single test case.
// This means a single instance of a test case if the test is parameterized.
// E.g. "name must be a symbol [3]"
#[derive(Debug, Clone)]
pub struct TestCase {
    pub description: String,
    pub details: TestCaseDetails,
}

#[derive(Debug, Clone)]
pub enum TestCaseDetails {
    Schema {
        schema_text: String,
        expect_valid: bool,
    },
    InvalidType {
        type_text: String,
    },
    Value {
        type_id: String,
        value_text: String,
        expect_valid: bool,
    },
}

/// Transparent wrapper around Vec<TestCase> so that we can implement TryFrom
pub struct TestCaseVec(Vec<TestCase>);

impl Deref for TestCaseVec {
    type Target = Vec<TestCase>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Element> for TestCaseVec {
    type Error = String;
    fn try_from(value: Element) -> Result<Self, Self::Error> {
        let test_case_struct = match value.as_struct() {
            None => Err(format!("Malformed test case: {value}")),
            Some(struct_) => Ok(struct_),
        }?;

        let mut the_vec_of_test_cases = vec![];
        if test_case_struct.get("description").is_some() {
            let base_description = test_case_struct.get_required_text_field("description")?;

            if test_case_struct.get("valid_schemas").is_some()
                || test_case_struct.get("invalid_schemas").is_some()
            {
                let valid_schemas = test_case_struct.get_optional_list_field("valid_schemas")?;
                let invalid_schemas =
                    test_case_struct.get_optional_list_field("invalid_schemas")?;

                let width = (valid_schemas.len() + invalid_schemas.len())
                    .to_string()
                    .len();

                for (i, value) in valid_schemas.elements().enumerate() {
                    let schema_text = value
                        .as_sequence()
                        .ok_or(format!("Malformed test case; the valid schemas must be embedded as s-expressions: {value}"))?
                        .elements()
                        .fold("".to_string(), |s, e| format!("{s}\n{e}"));
                    the_vec_of_test_cases.push(TestCase {
                        description: format!("{base_description} [{i:0width$}]"),
                        details: TestCaseDetails::Schema {
                            schema_text,
                            expect_valid: true,
                        },
                    });
                }

                let i0 = valid_schemas.len();
                for (i, value) in invalid_schemas.elements().enumerate() {
                    let schema_text = value
                        .as_sequence()
                        .ok_or(format!("Malformed test case; the invalid schemas must be embedded as s-expressions: {value}"))?
                        .elements()
                        .fold("".to_string(), |s, e| format!("{s}\n{e}"));
                    the_vec_of_test_cases.push(TestCase {
                        description: format!("{base_description} [{:0width$}]", i0 + i),
                        details: TestCaseDetails::Schema {
                            schema_text,
                            expect_valid: false,
                        },
                    });
                }
            } else if test_case_struct.get("invalid_types").is_some() {
                let invalid_types = test_case_struct.get_required_list_field("invalid_types")?;
                let width = invalid_types.elements().count().to_string().len();
                for (i, value) in invalid_types.elements().enumerate() {
                    the_vec_of_test_cases.push(TestCase {
                        description: format!("{base_description} [{i:0width$}]"),
                        details: TestCaseDetails::InvalidType {
                            type_text: format!("{value}"),
                        },
                    })
                }
            } else {
                return Err(format!("Malformed test case - unknown test type: {value}"));
            }
        } else if test_case_struct.get("type").is_some()
            && (test_case_struct.get("should_accept_as_valid").is_some()
                || test_case_struct.get("should_reject_as_invalid").is_some())
        {
            let type_id = test_case_struct.get_required_text_field("type")?;

            let valid_values =
                test_case_struct.get_optional_list_field("should_accept_as_valid")?;
            let width = valid_values.len().to_string().len();
            for (i, value) in valid_values.elements().enumerate() {
                the_vec_of_test_cases.push(TestCase {
                    description: format!("value should be valid for type {type_id} [{i:0width$}]"),
                    details: TestCaseDetails::Value {
                        type_id: type_id.to_string(),
                        value_text: format!("{value}"),
                        expect_valid: true,
                    },
                })
            }

            let invalid_values =
                test_case_struct.get_optional_list_field("should_reject_as_invalid")?;
            let width = invalid_values.len().to_string().len();
            for (i, value) in invalid_values.elements().enumerate() {
                the_vec_of_test_cases.push(TestCase {
                    description: format!(
                        "value should be invalid for type {type_id} [{i:0width$}]"
                    ),
                    details: TestCaseDetails::Value {
                        type_id: type_id.to_string(),
                        value_text: format!("{value}"),
                        expect_valid: false,
                    },
                })
            }
        } else {
            return Err("malformed test case".to_string());
        }

        if the_vec_of_test_cases.len() == 1 {
            let the_test_case = the_vec_of_test_cases.pop().unwrap();
            the_vec_of_test_cases.push(TestCase {
                description: the_test_case
                    .description
                    .strip_suffix(" [0]")
                    .unwrap()
                    .to_string(),
                ..the_test_case
            })
        }
        Ok(TestCaseVec(the_vec_of_test_cases))
    }
}

// Helpers to DRY up the code when getting values from the test case struct
trait StructUtils {
    fn get_required_text_field(&self, field_name: &str) -> Result<&str, String>;
    fn get_required_list_field(&self, field_name: &str) -> Result<List, String>;
    fn get_optional_list_field(&self, field_name: &str) -> Result<List, String>;
}

impl StructUtils for Struct {
    fn get_required_text_field(&self, field_name: &str) -> Result<&str, String> {
        if self.get_all(field_name).count() > 1 {
            Err(format!(
                "Malformed test case - field '{}' is repeated: {}",
                field_name,
                Element::from(self.clone())
            ))
        } else {
            let field = self.get(field_name).ok_or(format!(
                "Malformed test case - field '{}' is missing: {}",
                field_name,
                Element::from(self.clone())
            ))?;
            field.as_text().ok_or(format!(
                "Malformed test case - field '{}' must be text: {}",
                field_name,
                Element::from(self.clone())
            ))
        }
    }

    fn get_required_list_field(&self, field_name: &str) -> Result<List, String> {
        if self.get(field_name).is_none() {
            Err(format!(
                "Malformed test case - field '{}' is missing: {}",
                field_name,
                Element::from(self.clone())
            ))
        } else {
            self.get_optional_list_field(field_name)
        }
    }

    fn get_optional_list_field(&self, field_name: &str) -> Result<List, String> {
        if self.get_all(field_name).count() > 1 {
            Err(format!(
                "Malformed test case - field '{}' is repeated: {}",
                field_name,
                Element::from(self.clone())
            ))
        } else if let Some(field) = self.get(field_name) {
            let list = field.as_sequence().ok_or(format!(
                "Malformed test case - field '{}' must be a list: {}",
                field_name,
                Element::from(self.clone())
            ))?;
            Ok(ion_rs::List(list.clone()))
        } else {
            Ok(ion_rs::List(Element::sequence_builder().build()))
        }
    }
}
