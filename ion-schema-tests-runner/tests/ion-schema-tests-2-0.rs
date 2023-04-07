use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    // Support for ISL 2.0 is not implemented yet, so all tests are ignored.
    ignored(
        "open_content",
        "imports",
        "schema::*",
        "constraints::all_of",
        "constraints::annotations_simplified",
        "constraints::annotations_standard",
        "constraints::any_of",
        "constraints::byte_length",
        "constraints::codepoint_length",
        "constraints::container_length",
        "constraints::contains",
        "constraints::element.*element_with_nullable_type", // This can be removed once we have implementation for `$null_or` annotation as per ISL 2.0
        "constraints::field_names",
        "constraints::fields",
        "constraints::ieee754_float",
        "constraints::not",
        "constraints::one_of",
        "constraints::ordered_elements",
        "constraints::precision",
        "constraints::regex",
        "constraints::regex-invalid",
        "constraints::timestamp_offset",
        "constraints::timestamp_precision",
        "constraints::type",
        "constraints::utf8_byte_length",
        "constraints::valid_values",
        "constraints::valid_values-ranges"
    )
);
