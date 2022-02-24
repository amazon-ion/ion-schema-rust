use ion_rs::result::IonError;
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::reader::{element_reader, ElementReader};
use ion_rs::value::{Element, Sequence};
use ion_schema::authority::FileSystemDocumentAuthority;
use ion_schema::system::{SchemaSystem, TypeStore};
use ion_schema::types::{TypeDefinitionImpl, TypeValidator};
use std::fs;
use std::path::Path;

const TEST_ROOT_DIR: &str = "ion-schema-tests/";

const SCHEMA_TEST_LIST: &[&str] = &[
    "ion-schema-tests/constraints/all_of/core_types.isl",
    "ion-schema-tests/constraints/all_of/empty_type.isl",
    "ion-schema-tests/constraints/all_of/invalid.isl",
    "ion-schema-tests/constraints/any_of/core_types.isl",
    "ion-schema-tests/constraints/any_of/empty_type.isl",
    "ion-schema-tests/constraints/any_of/invalid.isl",
    "ion-schema-tests/constraints/not/empty_type.isl",
    "ion-schema-tests/constraints/not/$string.isl",
    "ion-schema-tests/constraints/not/invalid.isl",
    "ion-schema-tests/constraints/not/nested.isl",
    "ion-schema-tests/constraints/not/string.isl",
    "ion-schema-tests/constraints/one_of/core_types.isl",
    "ion-schema-tests/constraints/one_of/empty_type.isl",
    "ion-schema-tests/constraints/one_of/invalid.isl",
    "ion-schema-tests/constraints/type/empty_type.isl",
    "ion-schema-tests/constraints/type/validation_any.isl",
    "ion-schema-tests/constraints/type/validation_int.isl",
    "ion-schema-tests/core_types/any.isl",
    "ion-schema-tests/core_types/blob.isl",
    "ion-schema-tests/core_types/bool.isl",
    "ion-schema-tests/core_types/clob.isl",
    "ion-schema-tests/core_types/decimal.isl",
    "ion-schema-tests/core_types/float.isl",
    "ion-schema-tests/core_types/int.isl",
    "ion-schema-tests/core_types/list.isl",
    "ion-schema-tests/core_types/lob.isl",
    "ion-schema-tests/core_types/number.isl",
    "ion-schema-tests/core_types/sexp.isl",
    "ion-schema-tests/core_types/struct.isl",
    "ion-schema-tests/core_types/symbol.isl",
    "ion-schema-tests/core_types/text.isl",
    "ion-schema-tests/core_types/timestamp.isl",
    "ion-schema-tests/ion_types/$any.isl",
    "ion-schema-tests/ion_types/$blob.isl",
    "ion-schema-tests/ion_types/$bool.isl",
    "ion-schema-tests/ion_types/$clob.isl",
    "ion-schema-tests/ion_types/$decimal.isl",
    "ion-schema-tests/ion_types/$float.isl",
    "ion-schema-tests/ion_types/$int.isl",
    "ion-schema-tests/ion_types/$list.isl",
    "ion-schema-tests/ion_types/$lob.isl",
    "ion-schema-tests/ion_types/$null.isl",
    "ion-schema-tests/ion_types/$number.isl",
    "ion-schema-tests/ion_types/$sexp.isl",
    "ion-schema-tests/ion_types/$struct.isl",
    "ion-schema-tests/ion_types/$symbol.isl",
    "ion-schema-tests/ion_types/$text.isl",
    "ion-schema-tests/ion_types/$timestamp.isl",
];

#[test]
fn validation_tests() {
    // create a schema system for validation test
    let authority = FileSystemDocumentAuthority::new(Path::new(TEST_ROOT_DIR));
    let mut schema_system = SchemaSystem::new(vec![Box::new(authority)]);

    for path in SCHEMA_TEST_LIST {
        print!("Reading {}... ", path);

        // get the schema content from given schema file path
        let ion_content = fs::read(path).unwrap();
        let iterator = element_reader().iterate_over(&ion_content).unwrap();
        let schema_content = iterator
            .collect::<Result<Vec<OwnedElement>, IonError>>()
            .unwrap();

        let type_store = &mut TypeStore::new();
        let mut invalid_values: Vec<OwnedElement> = vec![];
        let mut valid_values: Vec<OwnedElement> = vec![];
        let mut type_def: Option<TypeDefinitionImpl> = None;
        let mut test_failed: bool = false;

        for element in schema_content {
            let annotations: Vec<&OwnedSymbolToken> = element.annotations().collect();
            if annotations.contains(&&text_token("invalid_type")) {
                // check for an invalid type validation
                let type_def_result = schema_system.schema_type_from_element(&element, type_store);
                if type_def_result.is_ok() {
                    println!("ERROR: expected error for invalid type {:?}", element);
                    test_failed = true;
                    break;
                }
            } else if annotations.contains(&&text_token("invalid")) {
                // get invalid values to validate
                invalid_values = element
                    .as_sequence()
                    .unwrap()
                    .iter()
                    .map(|v| v.to_owned())
                    .collect();
            } else if annotations.contains(&&text_token("valid")) {
                // get valid values to validate
                valid_values = element
                    .as_sequence()
                    .unwrap()
                    .iter()
                    .map(|v| v.to_owned())
                    .collect();
            } else if annotations.contains(&&text_token("type")) {
                // get type definition for type validation
                type_def = Some(
                    schema_system
                        .schema_type_from_element(&element, type_store)
                        .unwrap(),
                );
            } else {
                continue;
            }
        }

        match type_def {
            None => {}
            Some(schema_type) => {
                for valid_value in valid_values {
                    match schema_type.validate(&valid_value, type_store) {
                        Ok(_) => {
                            continue;
                        }
                        Err(error) => {
                            println!("ERROR: {} for {:?}", error, valid_value);
                            test_failed = true;
                        }
                    };
                }

                if test_failed {
                    // move to next test file if any test failed
                    continue;
                }

                for invalid_value in invalid_values {
                    match schema_type.validate(&invalid_value, type_store) {
                        Ok(_) => {
                            println!(
                                "ERROR: expected invalid value to return error for {:?}",
                                &invalid_value
                            );
                            test_failed = true;
                            break;
                        }
                        Err(_) => {
                            continue;
                        }
                    };
                }
            }
        }

        // print "OK" for a successful validation test
        if !test_failed {
            println!("OK");
        }
    }
}
