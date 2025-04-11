// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::internal_traits::{LoaderContext, ReadFromIsl, WriteAsIsl, WriteContext};
use crate::ion_extension::StructExtensions;
use crate::resolver::TypeCoordinates;
use crate::result::{invalid_schema, HasIslSourceLocation, IonSchemaResult, IslSourceLocation};
use crate::IslVersion;
use ion_rs::{Element, StructWriter, Value, ValueWriter};

/// References another type, by name, in Ion Schema.
#[derive(Debug, Clone)]
pub struct TypeReference {
    schema_id: Option<String>,
    type_name: String,
    source_location: IslSourceLocation,
    // Not exposed in public API. Initially `None`, but changed to `Some(_)` when resolving the schemas
    resolved_type_coordinates: Option<TypeCoordinates>,
}
impl PartialEq for TypeReference {
    fn eq(&self, other: &Self) -> bool {
        self.schema_id == other.schema_id && self.type_name == other.type_name
    }
}

impl TypeReference {
    /// Creates a `TypeReference` for a type name that is defined in or imported to the current
    /// [`SchemaDocument`].
    fn local<S: Into<String>>(type_name: S) -> Self {
        TypeReference {
            schema_id: None,
            type_name: type_name.into(),
            source_location: IslSourceLocation::new(None),
            resolved_type_coordinates: None,
        }
    }

    /// Creates a `TypeReference` that is an inline import from another schema.
    pub fn imported<A: Into<String>, B: Into<String>>(schema_id: A, type_name: B) -> Self {
        TypeReference {
            schema_id: Some(schema_id.into()),
            type_name: type_name.into(),
            source_location: IslSourceLocation::new(None),
            resolved_type_coordinates: None,
        }
    }

    pub fn schema_id(&self) -> Option<&str> {
        self.schema_id.as_deref()
    }

    pub fn type_name(&self) -> &str {
        self.type_name.as_str()
    }

    pub(crate) fn type_coordinates(&self) -> Option<TypeCoordinates> {
        self.resolved_type_coordinates
    }

    /// Sets the [`TypeCoordinates`] of this `TypeReference`.
    pub(crate) fn set_type_coordinates(&mut self, type_coordinates: Option<TypeCoordinates>) {
        self.resolved_type_coordinates = type_coordinates
    }
}

impl<T: Into<String>> From<T> for TypeReference {
    fn from(value: T) -> Self {
        TypeReference::local(value)
    }
}

impl<V: IslVersion> WriteAsIsl<V> for TypeReference {
    fn write_as_isl<W: ValueWriter>(
        &self,
        writer: W,
        ctx: &WriteContext<V>,
    ) -> IonSchemaResult<()> {
        if let Some(schema_id) = self.schema_id() {
            let mut struct_writer = writer.struct_writer()?;
            struct_writer.field_writer("id").write(schema_id)?;
            struct_writer
                .field_writer("type")
                .write_symbol(self.type_name())?;
            struct_writer.close()?;
        } else {
            writer.write_symbol(self.type_name())?;
        }
        Ok(())
    }
}

impl<V: IslVersion> ReadFromIsl<V> for TypeReference {
    fn try_read(ion: &Element, ctx: &LoaderContext<V>) -> IonSchemaResult<Self> {
        match ion.value() {
            Value::Symbol(s) => {
                let mut tr: TypeReference = s.expect_text()?.into();
                tr.source_location = ion.isl_source_location();
                Ok(tr)
            }
            Value::Struct(s) => {
                if s.fields().count() != 2 {
                    return invalid_schema!("Unexpected extra field(s) in inline import: {ion}");
                }
                let schema_id = s.get_required("id")?.expect_text()?;
                let type_name = s.get_required("type")?.expect_text()?;
                Ok(TypeReference::imported(schema_id, type_name))
            }
            other => invalid_schema!("TypeReference must be a symbol or struct; was: {ion}"),
        }
    }
}

impl HasIslSourceLocation for TypeReference {
    fn isl_source_location(&self) -> IslSourceLocation {
        self.source_location
    }
}

#[cfg(test)]
mod tests {
    use crate::internal_traits::{LoaderContext, ReadFromIsl};
    use crate::model::type_reference::TypeReference;

    use crate::result::IslSourceLocation;
    use crate::{ISL_1_0, ISL_2_0};
    use ion_rs::Element;
    use rstest::rstest;

    #[test]
    fn from_string() {
        let from_value = "foo";
        let expected = TypeReference::local("foo");
        assert_eq!(expected, from_value.into())
    }

    #[test]
    fn new_imported_type_reference() {
        let actual = TypeReference::imported("foo", "bar");
        let expected = TypeReference {
            schema_id: Some("foo".to_string()),
            type_name: "bar".to_string(),
            source_location: IslSourceLocation::new(None),
            resolved_type_coordinates: None,
        };
        assert_eq!(expected, actual)
    }

    #[rstest]
    #[case::type_name("foo", TypeReference::local("foo"))]
    #[case::inline_import("{id:\"foo.isl\",type:bar}", TypeReference::imported("foo.isl", "bar"))]
    fn type_reference_try_read_ok(#[case] ion: &str, #[case] expected: TypeReference) {
        let expected = Ok(expected);
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_1_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert_eq!(result, expected);

        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert_eq!(result, expected)
    }

    #[rstest]
    #[case::inline_import_must_have_id("{type:foo}")]
    #[case::inline_import_must_have_type("{id:\"foo\"}")]
    #[case::inline_import_must_not_have_other_fields("{id: \"abc\",type:def,as:ghi}")]
    fn type_constraint_try_read_err(#[case] ion: &str) {
        let element = Element::read_one(ion).unwrap();
        let load_ctx = LoaderContext::<ISL_1_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert!(result.is_err());

        let load_ctx = LoaderContext::<ISL_2_0>::new();
        let result = TypeReference::try_read(&element, &load_ctx);
        assert!(result.is_err())
    }
}
