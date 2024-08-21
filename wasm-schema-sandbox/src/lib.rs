use ion_schema::authority::{DocumentAuthority, MapDocumentAuthority};

use ion_schema::external::ion_rs::element::Element;
use ion_schema::external::ion_rs::IonResult;
use ion_schema::result::IonSchemaResult;
use ion_schema::schema::Schema;
use ion_schema::system::SchemaSystem;
use ion_schema::types::TypeDefinition;
use ion_schema::violation::Violation;
use ion_schema::IonSchemaElement;
use js_sys::Array;
use serde::{Deserialize, Serialize};
use serde_wasm_bindgen::to_value;
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

fn load_all(text: &str) -> IonResult<Vec<Element>> {
    Element::read_all(text.as_bytes())
}

#[derive(Serialize, Deserialize)]
pub struct ViolationResult {
    ion_path: String,
    code: String,
    message: String,
}

#[wasm_bindgen]
pub struct SchemaValidationResult {
    result: bool,
    violations: Array,
    value: String,
    has_error: bool,
    error: String,
}

#[wasm_bindgen]
impl SchemaValidationResult {
    #[wasm_bindgen(constructor)]
    pub fn new(
        r: bool,
        v: Array,
        val: String,
        has_error: bool,
        error: String,
    ) -> SchemaValidationResult {
        SchemaValidationResult {
            result: r,
            violations: v,
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

    pub fn violations(&self) -> Array {
        self.violations.to_owned()
    }
}

#[wasm_bindgen]
pub fn validate(
    ion: &str,
    schema: &str,
    schema_type: &str,
    is_document: bool,
) -> SchemaValidationResult {
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
                Array::new(),
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
                Array::new(),
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
    let values_result = load_all(ion);

    let value = match values_result {
        Ok(v) if is_document => IonSchemaElement::Document(v),
        Ok(v) => IonSchemaElement::SingleElement(v[0].to_owned()),
        Err(_) => {
            return SchemaValidationResult::new(
                false,
                Array::new(),
                ion.to_string(),
                true,
                format!("Can not parse given Ion value: {ion} for validation"),
            )
        }
    };

    log!("loaded ion value successfully!");

    // Validate data based on `schema_type`
    let result = type_ref.validate(value.to_owned());

    log!("validation complete!");

    let violations: Vec<&Violation> = match &result {
        Ok(_) => vec![],
        Err(violation) => violation.flattened_violations(),
    };

    log!("Creating validation result....");

    let violations_result = Array::new();
    for violation in violations {
        let v_result = ViolationResult {
            ion_path: format!("{}", violation.ion_path()),
            code: format!("{}", violation.code()),
            message: violation.message().to_string(),
        };
        violations_result.push(&to_value(&v_result).unwrap());
    }

    let result: SchemaValidationResult = SchemaValidationResult::new(
        result.is_ok(),
        violations_result,
        format!("{value}"),
        false,
        "".to_string(),
    );

    log!("Schema validation was successful!");

    result
}
