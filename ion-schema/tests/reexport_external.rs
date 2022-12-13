#[cfg(test)]
mod reexport_tests {
    use ion_schema::external::ion_rs::value::reader::{element_reader, ElementReader};
    use ion_schema::isl::isl_constraint::IslConstraint;
    use ion_schema::isl::isl_type::{IslType, IslTypeImpl};
    use ion_schema::isl::isl_type_reference::IslTypeRef;

    /// This test shows how the ion_schema integration with ion_rs can be used
    /// through a reexport. This means that consumers of ion_schema can use this
    /// integration without having to specify the exact dependency version.
    #[test]
    fn ion_rs_is_reexported() {
        let actual_isl_type = load_anonymous_type("{type: int}");
        let expected_isl_type =
            IslType::anonymous([IslConstraint::type_constraint(IslTypeRef::named("int"))]);
        assert_eq!(actual_isl_type, expected_isl_type);
    }

    // helper function to create anonymous ISL type
    fn load_anonymous_type(text: &str) -> IslType {
        IslType::Anonymous(
            IslTypeImpl::from_owned_element(
                &element_reader()
                    .read_one(text.as_bytes())
                    .expect("parsing failed unexpectedly"),
                &mut vec![],
            )
            .unwrap(),
        )
    }
}
