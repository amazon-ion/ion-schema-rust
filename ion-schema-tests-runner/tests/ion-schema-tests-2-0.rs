use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    // Support for ISL 2.0 is not completely implemented yet, so some tests are ignored.
    ignored(
        "open_content",
        "imports",
        "schema::*",
        "constraints::contains",
        "constraints::ordered_elements",
        "constraints::precision",
        "constraints::regex::value_should_be_invalid_for_type_regex_unescaped_newline__2_", // https://github.com/amazon-ion/ion-rust/issues/399
        "constraints::timestamp_precision",
        // following tests are related to: https://github.com/amazon-ion/ion-rust/pull/553
        "constraints::valid_values_ranges::value_should_be_valid_for_type_valid_values_range_timestamp_known_offset__12_",
        "constraints::valid_values_ranges::value_should_be_valid_for_type_valid_values_range_timestamp_known_offset__13_",
        "constraints::valid_values_ranges::value_should_be_valid_for_type_valid_values_range_timestamp_unknown_offset__12_",
        "constraints::valid_values_ranges::value_should_be_valid_for_type_valid_values_range_timestamp_unknown_offset__13_"
    )
);
