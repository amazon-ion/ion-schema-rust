use ion_schema::authority::{DocumentAuthority, MapDocumentAuthority};

use ion_schema::external::ion_rs::element::Element;
use ion_schema::external::ion_rs::IonResult;
use ion_schema::result::IonSchemaResult;
use ion_schema::schema::Schema;
use ion_schema::system::SchemaSystem;
use ion_schema::types::TypeDefinition;
use std::str;
use std::sync::Arc;
use wasm_bindgen::prelude::*;

extern crate web_sys;

// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

fn load(text: &str) -> IonResult<Element> {
    Element::read_one(text.as_bytes())
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
    // Provide schema id for the schema you want to load (schema_id is the schema file name here)
    // This will be the id of schema, provided within the map authority defined below
    let schema_id = "schema.isl";

    // map with (id, ion content) to represent schema
    // the id here represents the schema id and for this map authority `schema.isl` is used as a unique id
    let map_authority = [(schema_id, schema)];

    log!("inside schema validation function");
    // Create a MapDocumentAuthority using a map like above with
    // schema id as key and schema as value
    let map_document_authority = MapDocumentAuthority::new(map_authority);
    let document_authorities: Vec<Box<dyn DocumentAuthority>> =
        vec![Box::new(map_document_authority)];

    // Create a new schema system using given document authorities
    let mut schema_system = SchemaSystem::new(document_authorities);

    log!("created schema system successfully!");

    // Load schema
    let schema_result: IonSchemaResult<Arc<Schema>> = schema_system.load_schema(schema_id);

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

    let schema: Arc<Schema> = schema_result.unwrap();

    log!("loaded schema successfully!");

    // Retrieve a particular type from this schema
    let type_ref_result: Option<TypeDefinition> = schema.get_type(schema_type);

    let type_ref = match &type_ref_result {
        Some(type_ref) => type_ref,
        None => {
            return SchemaValidationResult::new(
                false,
                "".to_string(),
                ion.to_string(),
                true,
                format!("Type definition: {schema_type} does not exist in this schema"),
            )
        }
    };

    log!(
        "{}",
        format!("got type definition for: {schema_type} successfully!")
    );

    // get Element from given ion text
    let value_result = load(ion);

    let value = match value_result {
        Ok(v) => v,
        Err(_) => {
            return SchemaValidationResult::new(
                false,
                "".to_string(),
                ion.to_string(),
                true,
                format!("Can not parse given Ion value: {ion} for validation"),
            )
        }
    };

    log!("loaded ion value successfully!");

    // Validate data based on `schema_type`
    let result = type_ref.validate(&value);

    log!("validation complete!");

    let violation = match &result {
        Ok(_) => "".to_string(),
        Err(violation) => {
            //TODO: Once we have a Display implementation with proper indentation for nested violations change the following
            format!("{violation:#?}")
        }
    };

    log!("Creating validation result....");

    let result: SchemaValidationResult = SchemaValidationResult::new(
        result.is_ok(),
        violation,
        format!("{value}"),
        false,
        "".to_string(),
    );

    log!("Schema validation was successful!");

    result
}
