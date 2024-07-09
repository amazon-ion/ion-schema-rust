use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    ignored(
        // Not fully implemented yet.
        "imports",
        "constraints::ordered_elements",
        // Failing because of https://github.com/amazon-ion/ion-rust/issues/399
        "constraints::regex::value_should_be_invalid_for_type_regex_unescaped_newline__2_",
    )
);
