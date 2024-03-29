use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_1_0/",
    ignored(
        "constraints::occurs::invalid::occurs_for_a_field_must_be_a_positive_integer_or_a_non_empty__non_negative_integer_range__0_",
        "constraints::occurs::invalid::occurs_for_a_field_must_be_a_positive_integer_or_a_non_empty__non_negative_integer_range__1_",
        "constraints::occurs::invalid::occurs_for_a_field_must_be_a_positive_integer_or_a_non_empty__non_negative_integer_range__6_",
        "constraints::occurs::invalid::occurs_for_a_field_must_be_a_positive_integer_or_a_non_empty__non_negative_integer_range__7_",
        "constraints::occurs::invalid::occurs_must_be_a_positive_integer_or_non_empty__non_negative_integer_range__06_",
        "constraints::occurs::invalid::occurs_range_must_be_a_valid__satisfiable_range__1_",
        "constraints::scale::invalid::scale_must_be_an_integer_or_range__04_",
        "constraints::valid_values::all_types::value_should_be_invalid_for_type_valid_values_all_types__04_",
        "nullable::*",
        "schema::import::cycles::*",
        "schema::import::diamond_import::should_be_a_valid_schema",
        "schema::import::diamond_import::value_should_be_valid_for_type_diamond_import",
        "schema::import::import::value_should_be_(valid|invalid)_for_type",
        "schema::import::import_inline::.*",
        "schema::import::import_type_unknown::attempting_to_import_an_unknown_type_should_result_in_an_error",
        "schema::import::import_types::.*",
        "schema::import::invalid_duplicate_.*",
        "schema::import::invalid_transitive_import_.*",
        "schema::invalid_reuse_of_type_name::two_types_in_a_schema_cannot_have_the_same_name",
    ),
);
