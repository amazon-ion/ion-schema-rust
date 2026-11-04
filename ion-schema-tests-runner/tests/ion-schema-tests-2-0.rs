use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    ignored(
        // Import cycles are not supported yet. See https://github.com/amazon-ion/ion-schema-rust/issues/228
        "imports::cycles.*",
        // The `*.invalid-isl.ion` files are invalid schemas that are only there to be imported by
        // the test cases in `self_import.isl`, so they are not expected to load on their own.
        "imports::self_import::.*_invalid_isl::should_be_a_valid_schema",
        // Error cases not fully validated yet.
        "imports::header_imports::Importing_a_type_with_the_same_name_or_alias_as_locally_defined_type_should_result_in_an_error.*",
        "imports::header_imports::Two_different_imported_types_with_the_same_name_or_alias_should_result_in_an_error.*",
        "imports::invalid_imports::header_imports_may_not_have_unexpected_fields.*",
        "imports::invalid_imports::imports_may_not_have_unexpected_annotations.*",
        "imports::invalid_imports::imports_field_may_occur_at_most_one_time_in_the_schema_header.*",
        "imports::invalid_imports::imports_field_must_be_a_non_null_list.*",
        "imports::invalid_imports::imports_may_not_have_repeated_field_names.*",
        "imports::invalid_imports::inline_imports_must_have_exactly_one_each_of__id__and__type__fields_and_nothing_else__0_",
        "imports::invalid_imports::when_imported_schema_or_type_does_not_exist__should_be_an_invalid_schema__1_",
        "imports::invalid_imports::when_imported_schema_or_type_does_not_exist__should_be_an_invalid_schema__3_",
        // Failing because of https://github.com/amazon-ion/ion-rust/issues/399
        "constraints::regex::value_should_be_invalid_for_type_regex_unescaped_newline__2_",
    )
);
