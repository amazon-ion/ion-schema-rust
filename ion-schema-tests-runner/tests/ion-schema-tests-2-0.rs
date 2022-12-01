use ion_schema_tests_runner::ion_schema_tests;

ion_schema_tests!(
    root = "ion-schema-tests/ion_schema_2_0/",
    // Support for ISL 2.0 is not implemented yet, so all tests are ignored.
    ignored(".*")
);
