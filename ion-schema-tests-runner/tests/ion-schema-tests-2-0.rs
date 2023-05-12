use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    // Support for ISL 2.0 is not completely implemented yet, so some tests are ignored.
    ignored(
        "open_content",
        "imports",
        "schema::*",
        "constraints::all_of",
        "constraints::any_of",
        "constraints::byte_length",
        "constraints::codepoint_length",
        "constraints::container_length",
        "constraints::contains",
        "constraints::not",
        "constraints::one_of",
        "constraints::ordered_elements",
        "constraints::precision",
        "constraints::regex::value_should_be_invalid_for_type_regex_unescaped_newline__2_", // https://github.com/amazon-ion/ion-rust/issues/399
        "constraints::timestamp_offset",
        "constraints::timestamp_precision",
        "constraints::type",
        "constraints::utf8_byte_length",
        "constraints::valid_values",
        "constraints::valid_values-ranges"
    )
);
