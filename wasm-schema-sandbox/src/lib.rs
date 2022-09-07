use ion_schema::authority::{DocumentAuthority, MapDocumentAuthority};
use ion_schema::external::ion_rs::value::owned::OwnedElement;
use ion_schema::external::ion_rs::value::reader::{element_reader, ElementReader};
use ion_schema::external::ion_rs::IonResult;
use ion_schema::result::{IonSchemaResult, ValidationResult};
use ion_schema::schema::Schema;
use ion_schema::system::SchemaSystem;
use ion_schema::types::TypeRef;
use std::rc::Rc;
use std::str;
use wasm_bindgen::prelude::*;

extern crate web_sys;

// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

// Verify if the given value is valid and print violation for invalid value
fn check_value(value: OwnedElement, type_ref: &TypeRef) -> ValidationResult {
    type_ref.validate(&value)
}

fn load(text: &str) -> IonResult<OwnedElement> {
    element_reader().read_one(text.as_bytes())
}

#[wasm_bindgen]
pub struct SchemaValidationResult {
    result: bool,
    violation: String,
    value: String,
    has_error: bool,
    error: String,
}

#[wasm_bindgen]
impl SchemaValidationResult {
    #[wasm_bindgen(constructor)]
    pub fn new(
        r: bool,
        v: String,
        val: String,
        has_error: bool,
        error: String,
    ) -> SchemaValidationResult {
        SchemaValidationResult {
            result: r,
            violation: v,
            value: val,
            has_error,
            error,
        }
    }

    pub fn result(&self) -> bool {
        self.result
    }

    pub fn set_result(&mut self, val: bool) {
        self.result = val;
    }

    pub fn violation(&self) -> String {
        self.violation.to_owned()
    }

    pub fn set_violation(&mut self, val: String) {
        self.violation = val;
    }

    pub fn value(&self) -> String {
        self.value.to_owned()
    }

    pub fn set_value(&mut self, val: String) {
        self.value = val;
    }

    pub fn error(&self) -> String {
        self.error.to_owned()
    }

    pub fn set_error(&mut self, val: String) {
        self.error = val;
    }

    pub fn has_error(&self) -> bool {
        self.has_error.to_owned()
    }

    pub fn set_has_error(&mut self, val: bool) {
        self.has_error = val;
    }
}

#[wasm_bindgen]
pub fn validate(ion: &str, schema: &str, schema_type: &str) -> SchemaValidationResult {
    // map with (id, ion content) to represent `sample_number` schema
    let map_authority = [("sample_number.isl", schema)];

    log!("inside schema validation function");
    // Create a MapDocumentAuthority using a map like above with
    // schema id as key and schema as value
    let map_document_authority = MapDocumentAuthority::new(map_authority);
    let document_authorities: Vec<Box<dyn DocumentAuthority>> =
        vec![Box::new(map_document_authority)];

    // Create a new schema system using given document authorities
    let mut schema_system = SchemaSystem::new(document_authorities);

    log!("created schema system successfully!");

    // Provide schema id for the schema you want to load (schema_id is the schema file name here)
    let schema_id = "sample_number.isl";

    // Load schema
    let schema_result: IonSchemaResult<Rc<Schema>> = schema_system.load_schema(schema_id);

    match &schema_result {
        Ok(_) => {}
        Err(_error) => {
            // TODO: once we have a better Display for _error, replace the error portion of SchemaValidationResult to take this _error
            return SchemaValidationResult::new(
                false,
                "".to_string(),
                ion.to_string(),
                true,
                "Can not load given schema!\nPlease check Ion schema syntax for given schema."
                    .to_string(),
            );
        }
    };

    let schema: Rc<Schema> = schema_result.unwrap();

    log!("loaded schema successfully!");

    // This example uses a schema that was created using how to load a schema section (`my_schema.isl`)?
    // Retrieve a particular type from this schema
    let type_ref_result: Option<TypeRef> = schema.get_type(schema_type);

    let type_ref = match &type_ref_result {
        Some(type_ref) => type_ref,
        None => {
            return SchemaValidationResult::new(
                false,
                "".to_string(),
                ion.to_string(),
                true,
                format!(
                    "Type definition: {} does not exist in this schema",
                    schema_type
                ),
            )
        }
    };

    log!(
        "{}",
        format!("got type definition for: {} successfully!", schema_type)
    );

    // get OwnedElement from given ion text
    let value_result = load(ion);

    let value = match value_result {
        Ok(v) => v,
        Err(_) => {
            return SchemaValidationResult::new(
                false,
                "".to_string(),
                ion.to_string(),
                true,
                format!(
                    "Can not parse given Ion value: {} for validation",
                    ion.to_string()
                ),
            )
        }
    };

    log!("loaded ion value successfully!");

    // Validate data based on the type: 'my_int_type'
    let result = check_value(value.to_owned(), &type_ref);

    log!("validation complete!");

    let violation = match &result {
        Ok(_) => "".to_string(),
        Err(violation) => {
            format!("{:#?}", violation)
        }
    };

    log!("Creating validation result....");

    let result: SchemaValidationResult = SchemaValidationResult::new(
        result.is_ok(),
        violation.to_owned(),
        format!("{}", value),
        false,
        "".to_string(),
    );

    log!("Schema validation was successful!");

    result
}
