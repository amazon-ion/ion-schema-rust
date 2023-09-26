use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    // Support for ISL 2.0 is not completely implemented yet, so some tests are ignored.
    ignored(
        "imports",
        "constraints::ordered_elements",
        "constraints::precision",
        "constraints::regex::value_should_be_invalid_for_type_regex_unescaped_newline__2_", // https://github.com/amazon-ion/ion-rust/issues/399
    )
);
