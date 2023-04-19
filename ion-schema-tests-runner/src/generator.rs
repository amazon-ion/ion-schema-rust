use crate::generator::util::*;
use crate::model::{TestCaseDetails, TestCaseVec};
use ion_rs::element::Element;
use ion_rs::IonType;
use ion_schema::isl::IslVersion;
use proc_macro2::{Literal, TokenStream, TokenTree};
use quote::{format_ident, quote};
use regex::Regex;
use std::convert::TryInto;
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

/// Function or closure for checking whether to skip/ignore a test case.
/// The [String] argument is the Rust test name (E.g. foo::bar::my_test), and the function should
/// return [true] iff the test should be marked with `#[ignore]`.
pub type ExcludeFilter = Rc<dyn Fn(String) -> bool>;

/// Context that gets passed around internally in the generator.
#[derive(Clone)]
struct Context {
    pub(crate) root_dir: PathBuf,
    pub(crate) current_dir: PathBuf,
    pub(crate) exclude: ExcludeFilter,
}

/// Entry point for generating Ion Schema Tests test case functions.
pub fn generate_ion_schema_tests(root_dir_path: &Path, exclude: ExcludeFilter) -> TokenStream {
    let relative_path = PathBuf::from(root_dir_path);
    let mut absolute_path = std::env::current_dir().unwrap();
    absolute_path.push(relative_path);

    let mod_name_string =
        util::escape_to_ident(root_dir_path.file_stem().unwrap().to_str().unwrap());
    let mod_name = format_ident!("r#{}", mod_name_string);

    let ctx = Context {
        root_dir: root_dir_path.to_path_buf(),
        current_dir: root_dir_path.to_path_buf(),
        exclude: exclude.clone(),
    };

    let mod_preamble_ts = generate_preamble(root_dir_path);
    let mod_contents_ts = generate_submodule_content_for_path(ctx);

    quote! {
        #[cfg(test)]
        mod #mod_name {

            #mod_preamble_ts

            #mod_contents_ts
        }
    }
}

/// Generates a submodule for the given file path.
/// If the current path is a directory, the module contents will be more nested modules. If the
/// current path is a file, calls the function to generate all the test cases for the given file.
/// Panics if the path is not a file or a directory.
fn generate_submodule_content_for_path(ctx: Context) -> TokenStream {
    let path = ctx.current_dir.as_path();
    if path.is_dir() {
        let mut inner_ts_vec: Vec<TokenStream> = vec![];
        for dir in fs::read_dir(path).unwrap() {
            let current_dir = dir.unwrap().path();
            let dir_name = current_dir.file_stem().unwrap().to_str().unwrap();
            let mod_name_string = &util::escape_to_ident(dir_name);
            let mod_name = format_ident!("r#{}", mod_name_string);
            let mod_content = generate_submodule_content_for_path(Context {
                current_dir,
                ..ctx.clone()
            });
            inner_ts_vec.push(quote! {
                mod #mod_name {
                    use super::*;
                    #mod_content
                }
            })
        }
        join_token_streams(inner_ts_vec)
    } else if path.is_file() {
        generate_test_cases_for_file(ctx.clone())
    } else {
        unimplemented!("Not a directory or a file? {:?}", path)
    }
}

/// Generates all of the test cases for an ISL test file.
fn generate_test_cases_for_file(ctx: Context) -> TokenStream {
    let path_string = ctx.current_dir.to_str().unwrap();

    let schema_id = match ctx.current_dir.strip_prefix(ctx.root_dir) {
        Ok(p) => p
            .to_str()
            .unwrap_or_else(|| panic!("Path cannot be converted to a string: {p:?}")),
        Err(e) => panic!("Path is not in the root {path_string} – {e}"),
    };

    let mut test_case_tokens: Vec<TokenStream> = Vec::new();

    /// Macro is only for the [generate_test_cases_for_file] function, so it makes some assumptions
    /// about what variables are in scope—namely [ctx] and [test_case_tokens].
    macro_rules! maybe_ignore {
        ($test_case_name:expr) => {
            if (ctx.exclude)(format!("{}::{}", path_to_mod_name(ctx.current_dir.as_path()), escape_to_ident(&$test_case_name))) {
                test_case_tokens.push(quote!( #[ignore] ));
            }
        };
    }

    maybe_ignore!("should_be_a_valid_schema");
    let schema_id_literal = TokenTree::from(Literal::string(schema_id));
    test_case_tokens.push(quote!(
        #[test]
        fn should_be_a_valid_schema() {
            __new_schema_system().load_schema(#schema_id_literal).unwrap();
        }
    ));

    // get the schema content from given schema file path
    let ion_content = fs::read(ctx.current_dir.as_path())
        .unwrap_or_else(|e| panic!("Unable to read {path_string} – {e}"));
    let schema_content = Element::read_all(&ion_content)
        .unwrap_or_else(|e| panic!("Error in {path_string} – {e:?}"));

    let isl_version = find_isl_version(&schema_content);

    for element in schema_content {
        if element.annotations().contains("$test") {
            let test_cases: TestCaseVec = element
                .try_into()
                .unwrap_or_else(|e| panic!("Error in {path_string} - {e}"));

            for test_case in test_cases.iter() {
                let test_case = test_case.clone();
                let description = test_case.description;
                let test_case_name = escape_to_ident(&description);
                maybe_ignore!(test_case_name);
                match test_case.details {
                    TestCaseDetails::Schema {
                        schema_text,
                        expect_valid,
                    } => test_case_tokens.push(generate_schema_case_for_document(
                        &description,
                        &schema_text,
                        expect_valid,
                    )),
                    TestCaseDetails::InvalidType { type_text } => test_case_tokens.push(
                        generate_invalid_type_case(&description, &type_text, &isl_version),
                    ),
                    TestCaseDetails::Value {
                        type_id,
                        value_text,
                        expect_valid,
                    } => {
                        let test_case_name = escape_to_ident(&description);
                        maybe_ignore!(test_case_name);
                        test_case_tokens.push(generate_value_test_case(
                            &description,
                            schema_id,
                            &type_id,
                            &value_text,
                            expect_valid,
                        ))
                    }
                }
            }
        }
    }

    join_token_streams(test_case_tokens)
}

/// find ISL version from schema content
fn find_isl_version(schema_content: &[Element]) -> IslVersion {
    // ISL version marker regex
    let isl_version_marker: Regex = Regex::new(r"^\$ion_schema_\d.*$").unwrap();

    for value in schema_content {
        // if find a type definition or a schema header before finding any version marker then this is ISL 1.0
        if value.ion_type() == IonType::Struct
            && (value.annotations().contains("type")
                || value.annotations().contains("schema_header"))
        {
            // default ISL 1.0 version will be returned
            break;
        }
        // verify if value is an ISL version marker and if it has valid format
        if value.ion_type() == IonType::Symbol
            && isl_version_marker.is_match(value.as_text().unwrap())
        {
            // This implementation supports Ion Schema 1.0 and Ion Schema 2.0
            return match value.as_text().unwrap() {
                "$ion_schema_1_0" => IslVersion::V1_0,
                "$ion_schema_2_0" => IslVersion::V2_0,
                _ => unimplemented!("Unsupported Ion Schema Language version: {}", value),
            };
        }
    }

    // default ISL version 1.0 if no version marker is found
    IslVersion::V1_0
}

/// Generates a test case to assert that some Ion text is or is not a valid ISL schema document.
fn generate_schema_case_for_document(
    description: &str,
    schema_text: &str,
    expect_valid: bool,
) -> TokenStream {
    let schema_isl_text_token = TokenTree::from(Literal::string(schema_text));
    let test_name = format_ident!("{}", util::escape_to_ident(description));
    quote!(
        #[test]
        fn #test_name() -> Result<(), String> {
            __assert_schema_validity(#schema_isl_text_token, #expect_valid)
        }
    )
}

/// Generates tests case for valid/invalid values for an ISL type.
fn generate_value_test_case(
    description: &str,
    schema_id: &str,
    type_id: &str,
    value_text: &str,
    expect_valid: bool,
) -> TokenStream {
    let schema_id_literal = TokenTree::from(Literal::string(schema_id));
    let type_id_literal = TokenTree::from(Literal::string(type_id));
    let value_literal = TokenTree::from(Literal::string(value_text));
    let test_case_ident = format_ident!("{}", util::escape_to_ident(description));
    quote! {
        #[test]
        fn #test_case_ident() -> Result<(), String> {
            __assert_value_validity_for_type(#value_literal, #schema_id_literal, #type_id_literal, #expect_valid)
        }
    }
}

/// Generates a test case for an invalid ISL type.
fn generate_invalid_type_case(
    description: &str,
    invalid_type_text: &str,
    isl_version: &IslVersion,
) -> TokenStream {
    let invalid_type_text_token = TokenTree::from(Literal::string(invalid_type_text));
    let test_name = format_ident!("{}", util::escape_to_ident(description));
    let version = match isl_version {
        IslVersion::V1_0 => "$ion_schema_1_0",
        IslVersion::V2_0 => "$ion_schema_2_0",
    };
    let isl_version = TokenTree::from(Literal::string(version));
    quote! {
        #[test]
        fn #test_name() -> Result<(), String> {
            __assert_type_is_invalid(#invalid_type_text_token, #isl_version)
        }
    }
}

/// Generates common functions that are used by the specific test cases.
/// All of the generated test cases delegate to these functions for any non-trivial logic.
fn generate_preamble(root_dir_path: &Path) -> TokenStream {
    let root_dir_token = TokenTree::from(Literal::string(root_dir_path.to_str().unwrap()));

    quote! {
        use ion_rs::element::Sequence;
        use std::hash::{Hash, Hasher};

        /// Gets the root directory for the test suite.
        fn __get_test_source_root() -> &'static str { #root_dir_token }

        /// Creates a default schema system, with an Authority for the Ion Schema tests suite
        fn __new_schema_system() -> ion_schema::system::SchemaSystem {
            let authority = ion_schema::authority::FileSystemDocumentAuthority::new(std::path::Path::new(__get_test_source_root()));
            ion_schema::system::SchemaSystem::new(vec![Box::new(authority)])
        }

        /// Creates a default schema system, with an Authority for the Ion Schema tests suite, and
        /// one additional schema. This is necessary because [SchemaSystem] doesn't expose any
        /// methods for creating a new, anonymous schema.
        fn __new_schema_system_with_additional_schema(id: &str, content: &str) -> ion_schema::system::SchemaSystem {
            let fs_authority = ion_schema::authority::FileSystemDocumentAuthority::new(std::path::Path::new(__get_test_source_root()));
            let map_authority = ion_schema::authority::MapDocumentAuthority::new([(id, content)]);
            ion_schema::system::SchemaSystem::new(vec![Box::new(fs_authority), Box::new(map_authority)])
        }

        /// Asserts that some Ion text is or is not a valid ISL schema document
        fn __assert_schema_validity(schema_isl: &str, expect_valid: bool) -> Result<(), String> {
            let mut schema_system: ion_schema::system::SchemaSystem = __new_schema_system_with_additional_schema(
                "__schema__",
                schema_isl
            );
            let schema = schema_system.load_schema("__schema__");
            if schema.is_ok() == expect_valid {
                Ok(())
            } else {
                match schema {
                    Ok(_) => Err("Expected schema to be invalid".to_string()),
                    Err(e) => Err(format!("{:?}", e)),
                }
            }
        }

        /// Asserts that some Ion text is not a valid ISL type.
        /// Since [SchemaSystem] does not expose any methods for creating new, anonymous types, we
        /// wrap the type definition in a named type within a new schema.
        fn __assert_type_is_invalid(type_def: &str, isl_version: &str) -> Result<(), String> {
            let schema_isl_text = format!("{} type::{{ name: __invalid__, type: {} }}", isl_version, type_def);
            let mut schema_system: ion_schema::system::SchemaSystem = __new_schema_system_with_additional_schema("__invalid_type__", &*schema_isl_text);
            let schema = schema_system.load_schema("__invalid_type__");
            if schema.is_ok() {
                Err(format!("Expected type to be invalid: {}", type_def))
            } else {
                Ok(())
            }
        }

        /// Asserts that a value is or is not valid for a given ISL type.
        fn __assert_value_validity_for_type(value_ion: &str, schema_id: &str, type_id: &str, expect_valid: bool) -> Result<(), String> {
            let schema = __new_schema_system().load_schema(schema_id).unwrap();
            let isl_type = schema.get_type(type_id).unwrap();
            let value: ion_rs::element::Element = ion_rs::element::Element::read_one(value_ion.as_bytes()).unwrap();
            let prepared_value: ion_schema::IonSchemaElement = if value.annotations().contains("document") && value.ion_type() == ion_rs::IonType::SExp {
                let element_vec = value.as_sequence()
                    .unwrap_or_else(|| unreachable!("We already confirmed that this is a s-expression."))
                    .elements()
                    .map(|it| it.to_owned())
                    .collect::<Vec<_>>();
                ion_schema::IonSchemaElement::Document(element_vec)
            } else {
                ion_schema::IonSchemaElement::SingleElement(value)
            };
            let validation_result = isl_type.validate(prepared_value);
            if validation_result.is_ok() == expect_valid {
                Ok(())
            } else {
                match validation_result {
                    Ok(_) => {
                        Err(format!("Expected {} to be invalid for type {}", value_ion, type_id))
                    },
                    Err(violation) => {
                        Err(format!("{}", violation))
                    },
                }
            }
        }
    }
}

mod util {
    use super::*;
    use std::path::{Component, PathBuf};

    /// Computes the fully qualified module name for an ISL test file path.
    pub fn path_to_mod_name<P: Into<PathBuf>>(p: P) -> String {
        p.into()
            .components()
            .map(|c| match c {
                Component::Normal(s) => {
                    let pb: PathBuf = s.into();
                    escape_to_ident(pb.file_stem().unwrap().to_str().unwrap())
                }
                _ => "".to_string(),
            })
            .fold("".to_string(), |cur, nxt| cur + "::" + &*nxt)
    }

    /// Joins a [Vec] of [TokenStream] into a single, new [TokenStream]
    pub fn join_token_streams(ts_vec: Vec<TokenStream>) -> TokenStream {
        let mut joined_token_stream = TokenStream::new();
        for ts in ts_vec {
            joined_token_stream.extend(ts.clone());
        }
        joined_token_stream
    }

    /// Escapes test descriptions and file path elements to only have legal identifier characters.
    /// All non-word characters (i.e. `[a-zA-Z0-9]`) are replaced by "\_", and if the string starts
    /// with a number, then a "\_" is prepended.
    ///
    /// This function is safe to call multiple times—i.e. f(f(x)) == f(x)
    pub fn escape_to_ident(s: &str) -> String {
        // Using a regex instead of just looping through the string was ~1m45s instead of ~6s to
        // compile AND run the tests.
        // let re = Regex::new(r"\W").unwrap();
        // let escaped: String = re.replace_all(s, "_").into();
        let mut escaped = String::new();
        for c in s.chars() {
            if c.is_ascii_alphanumeric() || c == '_' {
                escaped.push(c);
            } else {
                escaped.push('_');
            }
        }
        if escaped.chars().next().unwrap().is_numeric() {
            format!("_{escaped}")
        } else {
            escaped
        }
    }

    #[cfg(test)]
    mod escape_to_ident_tests {
        use super::escape_to_ident;

        #[test]
        fn test_repeatability() {
            let s = "98fwjknsjd0__=+234, s'";
            assert_eq!(escape_to_ident(s), escape_to_ident(&escape_to_ident(s)))
        }
    }
}

// Only asserts that the generator doesn't panic. It's much easier to debug macro panics because
// we have this test case. Any problems with the generated code will be caught when the macro is
// invoked.
#[cfg(test)]
mod generator_does_not_panic {
    use super::generate_ion_schema_tests;
    use std::path::Path;
    use std::rc::Rc;

    #[test]
    fn ion_schema_1_0() {
        test("../ion-schema-tests/ion_schema_1_0")
    }

    #[test]
    fn ion_schema_2_0() {
        test("../ion-schema-tests/ion_schema_2_0")
    }

    fn test(path: &str) {
        let _ts: proc_macro2::TokenStream =
            generate_ion_schema_tests(Path::new(path), Rc::new(|_| false));
    }
}
