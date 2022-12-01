use ion_rs::result::IonError;
use ion_rs::value::owned::{text_token, Element};
use ion_rs::value::reader::{element_reader, ElementReader};
use ion_rs::value::{IonElement, IonSequence};
use ion_rs::Symbol;
use ion_schema::authority::FileSystemDocumentAuthority;
use ion_schema::system::{SchemaSystem, TypeId, TypeStore};
use ion_schema::types::TypeRef;
use rstest::*;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;
use test_generator::test_resources;

const TEST_ROOT_DIR: &str = "../ion-schema-tests/";

// following test files will be ignored while testing
// `test-resources` don't support to provide a skip-list of test files
const SKIP_LIST: &[&str] = &[
    "ion-schema-tests/constraints/all_of/validation.isl",
    "ion-schema-tests/constraints/fields/validation_base.isl",
    "ion-schema-tests/constraints/fields/validation_complex.isl",
    "ion-schema-tests/constraints/fields/validation_inlined_type.isl",
    "ion-schema-tests/constraints/fields/validation_nested.isl",
    "ion-schema-tests/constraints/any_of/validation.isl",
    "ion-schema-tests/constraints/one_of/validation.isl",
    "ion-schema-tests/constraints/type/validation.isl",
    "ion-schema-tests/constraints/content/validation_closed.isl",
    "ion-schema-tests/constraints/content/validation_mixed.isl",
    "ion-schema-tests/constraints/contains/validation.isl",
    "ion-schema-tests/constraints/contains/various_values.isl",
    "ion-schema-tests/constraints/element/validation_inline_import.isl",
    "ion-schema-tests/constraints/element/validation_int.isl",
    "ion-schema-tests/constraints/element/validation_named_type.isl",
    "ion-schema-tests/constraints/precision/validation.isl",
    "ion-schema-tests/constraints/scale/validation.isl",
    "ion-schema-tests/constraints/scale/invalid.isl",
    "ion-schema-tests/constraints/timestamp_precision/validation.isl",
    "ion-schema-tests/constraints/valid_values/all_types.isl",
    "ion-schema-tests/constraints/valid_values/validation_list.isl",
    "ion-schema-tests/constraints/valid_values/validation_int_range.isl",
    "ion-schema-tests/constraints/valid_values/validation_list_mix_types_ranges.isl",
    "ion-schema-tests/constraints/valid_values/validation_list_timestamp_ranges.isl",
    "ion-schema-tests/constraints/valid_values/validation_ranges.isl",
    "ion-schema-tests/constraints/valid_values/validation_timestamp_range.isl",
];

#[test_resources("ion-schema-tests/core_types/*.isl")]
#[test_resources("ion-schema-tests/constraints/all_of/*.isl")]
#[test_resources("ion-schema-tests/constraints/any_of/*.isl")]
#[test_resources("ion-schema-tests/constraints/fields/*.isl")]
#[test_resources("ion-schema-tests/constraints/not/invalid.isl")]
#[test_resources("ion-schema-tests/constraints/not/nested.isl")]
#[test_resources("ion-schema-tests/constraints/not/string.isl")]
#[test_resources("ion-schema-tests/constraints/one_of/*.isl")]
#[test_resources("ion-schema-tests/constraints/type/*.isl")]
#[test_resources("ion-schema-tests/constraints/content/*.isl")]
#[test_resources("ion-schema-tests/constraints/contains/*.isl")]
#[test_resources("ion-schema-tests/constraints/element/*.isl")]
#[test_resources("ion-schema-tests/constraints/annotations/*.isl")]
#[test_resources("ion-schema-tests/constraints/precision/*.isl")]
#[test_resources("ion-schema-tests/constraints/scale/*.isl")]
#[test_resources("ion-schema-tests/constraints/timestamp_precision/*.isl")]
#[test_resources("ion-schema-tests/constraints/valid_values/*.isl")]
#[test_resources("ion-schema-tests/constraints/regex/*.isl")]
#[test_resources("ion-schema-tests/constraints/utf8_byte_length/*.isl")]
// `test_resources` breaks for test-case names containing `$` and it doesn't allow
// to rename test-case names hence using `rstest` for `$*.isl` test files
// For more information: https://github.com/frehberg/test-generator/issues/11
#[rstest(
    path_string,
    case::nullable_any_type("ion-schema-tests/ion_types/$any.isl"),
    case::nullable_blob_type("ion-schema-tests/ion_types/$blob.isl"),
    case::nullable_bool_type("ion-schema-tests/ion_types/$bool.isl"),
    case::nullable_clob_type("ion-schema-tests/ion_types/$clob.isl"),
    case::nullable_decimal_type("ion-schema-tests/ion_types/$decimal.isl"),
    case::nullable_float_type("ion-schema-tests/ion_types/$float.isl"),
    case::nullable_int_type("ion-schema-tests/ion_types/$int.isl"),
    case::nullable_list_type("ion-schema-tests/ion_types/$list.isl"),
    case::nullable_lob_type("ion-schema-tests/ion_types/$lob.isl"),
    case::nullable_null_type("ion-schema-tests/ion_types/$null.isl"),
    case::nullable_number_type("ion-schema-tests/ion_types/$number.isl"),
    case::nullable_sexp_type("ion-schema-tests/ion_types/$sexp.isl"),
    case::nullable_string_type("ion-schema-tests/ion_types/$string.isl"),
    case::nullable_strcut_type("ion-schema-tests/ion_types/$struct.isl"),
    case::nullable_symbol_type("ion-schema-tests/ion_types/$symbol.isl"),
    case::nullable_text_type("ion-schema-tests/ion_types/$text.isl"),
    case::nullable_timestamp_type("ion-schema-tests/ion_types/$timestamp.isl")
)]
fn validation_tests(path_string: &str) {
    // The #[test_resources] macro provides file paths that are relative to the cargo workspace root
    // but when this test is run, it seems the current directory is the "ion-schema/" directory, so
    // we need to adjust the path to ensure that they are relative to the correct location.
    let mut path_buf = PathBuf::from_str(TEST_ROOT_DIR).unwrap();
    if path_string.starts_with("ion-schema-tests") {
        path_buf.pop();
    }
    path_buf.push(path_string);
    let path = path_buf.as_path();

    print!("{}...", path.to_str().unwrap());

    // create a set of all skip list file paths
    let paths_to_skip = skip_list_as_set(SKIP_LIST);

    // ignore the files that are in SKIP_LIST
    if paths_to_skip.contains(&PathBuf::from_str(path_string).unwrap()) {
        println!("IGNORED");
        return;
    }
    // create a schema system for validation test
    let authority = FileSystemDocumentAuthority::new(Path::new(TEST_ROOT_DIR));
    let mut schema_system = SchemaSystem::new(vec![Box::new(authority)]);

    // get the schema content from given schema file path
    let ion_content = fs::read(path)
        .unwrap_or_else(|e| panic!("Could not read from given file {} \n {}", path_string, e));
    let iterator = element_reader()
        .iterate_over(&ion_content)
        .unwrap_or_else(|_| {
            panic!(
                "Could not get owned elements from schema file: {}",
                path_string
            )
        });
    let schema_content = iterator
        .collect::<Result<Vec<Element>, IonError>>()
        .unwrap_or_else(|_| {
            panic!(
                "Could not get owned elements from schema file: {}",
                path_string
            )
        });

    let type_store = &mut TypeStore::default();
    let mut invalid_values: Vec<Element> = vec![];
    let mut valid_values: Vec<Element> = vec![];
    let mut type_id: Option<TypeId> = None;

    // store all the errors encountered while testing
    let mut failed_tests = vec![];

    for element in schema_content {
        let annotations: Vec<&Symbol> = element.annotations().collect();
        if annotations.contains(&&text_token("invalid_type")) {
            // check for an invalid type validation
            let type_def_result = schema_system.schema_type_from_element(&element, type_store);
            if type_def_result.is_ok() {
                failed_tests.push(format!("Expected error for invalid type: {:?}", element));
            }
        } else if annotations.contains(&&text_token("invalid")) {
            // get invalid values to validate
            invalid_values = element
                .as_sequence()
                .expect("The `invalid` annotation can only appear on a list or s-expression.")
                .iter()
                .map(|v| v.to_owned())
                .collect();
        } else if annotations.contains(&&text_token("valid")) {
            // get valid values to validate
            valid_values = element
                .as_sequence()
                .expect("The `valid` annotation can only appear on a list or s-expression.")
                .iter()
                .map(|v| v.to_owned())
                .collect();
        } else if annotations.contains(&&text_token("type")) {
            // get type definition for type validation
            type_id = Some(
                schema_system
                    .schema_type_from_element(&element, type_store)
                    .unwrap_or_else(|_| {
                        panic!("Could not get schema type from owned element {:?}", element)
                    }),
            );
        } else {
            continue;
        }
    }

    let type_ref = type_id.and_then(|id| Some(TypeRef::new(id, Rc::from(type_store.to_owned()))));

    if let Some(schema_type) = type_ref {
        for valid_value in valid_values {
            if let Err(error) = schema_type.validate(&valid_value) {
                failed_tests.push(format!("{} for {:?}", error, valid_value));
            }
        }

        for invalid_value in invalid_values {
            if schema_type.validate(&invalid_value).is_ok() {
                failed_tests.push(format!(
                    "Expected error for invalid value: {:?}",
                    &invalid_value
                ));
            }
        }
    }

    if failed_tests.is_empty() {
        // print "OK" for a successful test
        println!("OK");
    } else {
        // print all errors found during test
        println!("ERROR");
        for failed_test in failed_tests {
            println!("{}", failed_test);
        }
        panic!("Found error in ion schema tests")
    }
}

// Converts the provided slice of strings to a HashSet of paths
fn skip_list_as_set(files_to_skip: &[&str]) -> BTreeSet<PathBuf> {
    let mut skip_set = BTreeSet::new();
    for file in files_to_skip {
        skip_set.insert(PathBuf::from_str(file).unwrap());
    }
    skip_set
}
