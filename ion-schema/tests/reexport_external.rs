#[cfg(test)]
mod reexport_tests {
    use ion_schema::authority::MapDocumentAuthority;
    use ion_schema::isl::isl_constraint::v_1_0::*;
    use ion_schema::isl::isl_type::v_1_0::*;
    use ion_schema::isl::isl_type::IslType;
    use ion_schema::isl::isl_type_reference::v_1_0::*;
    use ion_schema::result::IonSchemaResult;
    use ion_schema::system::SchemaSystem;

    /// This test shows how the ion_schema integration with ion_rs can be used
    /// through a reexport. This means that consumers of ion_schema can use this
    /// integration without having to specify the exact dependency version.
    #[test]
    fn ion_rs_is_reexported() -> IonSchemaResult<()> {
        // map with (id, ion content)
        let map_authority = [(
            "sample.isl",
            r#"
                schema_header::{}
                
                type::{
                  name: my_type,
                  type: int,
                }
                
                schema_footer::{}
            "#,
        )];
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(MapDocumentAuthority::new(map_authority))]);
        let schema = schema_system.load_isl_schema_v1_0("sample.isl")?;
        let actual_isl_type: &IslType = &schema.types()[0];

        let expected_isl_type = &named_type("my_type", [type_constraint(named_type_ref("int"))]);
        assert_eq!(actual_isl_type, expected_isl_type);
        Ok(())
    }
}
