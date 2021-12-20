use crate::import::Import;
use crate::system::{TypeId, TypeStore};
use crate::types::{TypeDefinitionImpl, TypeRef};
use std::rc::Rc;

/// A Schema is a collection of zero or more [Type]s.
///
/// Each type may refer to other types within the same schema,
/// or types imported into this schema from other schemas.
/// To instantiate a Schema, see [SchemaSystem].
#[derive(Debug, Clone)]
pub struct Schema {
    id: String,
    types: Rc<TypeStore>,
}

impl Schema {
    pub(crate) fn new<A: AsRef<str>>(id: A, types: Rc<TypeStore>) -> Self {
        Self {
            id: id.as_ref().to_owned(),
            types,
        }
    }

    /// Returns the id for this Schema
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Returns an [Import] representing all the types imported from
    /// the specified schema [id].
    fn import(&self, id: String) -> Option<Import> {
        todo!()
    }

    /// Returns an iterator over the imports of this [Schema].
    fn imports(&self) -> SchemaTypeIterator {
        todo!()
    }

    /// Returns an iterator over the imported types of this [Schema].
    fn imported_types(&self) -> SchemaTypeIterator {
        SchemaTypeIterator::new(Rc::clone(&self.types), self.types.get_imports())
    }

    /// Returns the requested type, if present in this schema;
    /// otherwise returns None.
    pub fn get_type<A: AsRef<str>>(&self, name: A) -> Option<TypeRef> {
        let type_id = self.types.get_type_id_by_name(name.as_ref())?;
        Some(TypeRef::new(*type_id, Rc::clone(&self.types)))
    }

    /// Returns an iterator over the types in this schema.
    /// This includes the core types and named types defined within this schema.
    pub fn get_types(&self) -> SchemaTypeIterator {
        SchemaTypeIterator::new(Rc::clone(&self.types), self.types.get_types())
    }

    /// Returns a new [Schema] instance containing all the types of this
    /// instance plus the provided type.  Note that the added type
    /// in the returned instance will hide a type of the same name
    /// from this instance.
    fn plus_type(&self, schema_type: TypeDefinitionImpl) -> Self {
        todo!()
    }
}

/// Provides an Iterator which returns [Type]s inside a [Schema]
pub struct SchemaTypeIterator {
    type_store: Rc<TypeStore>,
    index: usize,
    types: Vec<TypeId>,
}

impl SchemaTypeIterator {
    fn new(type_store: Rc<TypeStore>, types: Vec<TypeId>) -> Self {
        Self {
            type_store,
            index: 0,
            types,
        }
    }
}

impl Iterator for SchemaTypeIterator {
    type Item = TypeRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.types.len() {
            return None;
        }
        self.index = self.index + 1;
        Some(TypeRef::new(
            self.types[self.index - 1],
            Rc::clone(&self.type_store),
        ))
    }
}

#[cfg(test)]
mod schema_tests {
    use super::*;
    use crate::system::Resolver;
    use ion_rs::value::owned::OwnedElement;
    use ion_rs::value::reader::element_reader;
    use ion_rs::value::reader::ElementReader;
    use rstest::*;

    // helper function to be used by schema tests
    fn load(text: &str) -> Vec<OwnedElement> {
        element_reader()
            .read_all(text.as_bytes())
            .expect("parsing failed unexpectedly")
    }

    // helper function to be used by validation tests
    fn load_schema_from_text(text: &str) -> Rc<Schema> {
        let owned_elements = load(text).into_iter();
        // create a type_store and resolver instance to be used for loading OwnedElements as schema
        let type_store = &mut TypeStore::new();
        let mut resolver = Resolver::new(vec![]);

        // create a isl from owned_elements and create a schema from isl
        let isl = resolver.isl_schema_from_elements(owned_elements, "my_schema.isl");
        let schema = resolver
            .schema_from_isl_schema(isl.unwrap(), "my_schema.isl", type_store, None)
            .unwrap();
        schema
    }

    #[rstest(
    owned_elements, total_types,
    case::type_constraint_with_named_type(
        load(r#" // For a schema with named type as below:
            type:: { name: my_int, type: int }
        "#).into_iter(),
        1 // this includes the named type my_int
    ),
    case::type_constraint_with_self_reference_type(
        load(r#" // For a schema with self reference type as below:
            type:: { name: my_int, type: my_int }
        "#).into_iter(),
        1 // this includes only my_int type
    ),
    case::type_constraint_with_nested_self_reference_type(
        load(r#" // For a schema with nested self reference type as below:
            type:: { name: my_int, type: { type: my_int } }
        "#).into_iter(),
        1 // this includes my_int type
    ),
    case::type_constraint_with_nested_type(
        load(r#" // For a schema with nested types as below:
            type:: { name: my_int, type: { type: int } }
        "#).into_iter(),
        1 // this includes my_int type
    ),
    case::type_constraint_with_nested_multiple_types(
        load(r#" // For a schema with nested multiple types as below:
            type:: { name: my_int, type: { type: int }, type: { type: my_int } }
        "#).into_iter(),
        1 //  this includes my_int type
    ),
    case::type_constraint_with_multiple_types(
        load(r#" // For a schema with multiple type as below:
             type:: { name: my_int, type: int }
             type:: { name: my_bool, type: bool }
        "#).into_iter(),
        2
    ),
    case::all_of_constraint(
        load(r#" // For a schema with all_of type as below:
            type:: { name: all_of_type, all_of: [{ type: int }] }
        "#).into_iter(),
        1 // this includes named type all_of_type
    ),
    case::any_of_constraint(
        load(r#" // For a schema with any_of constraint as below:
                type:: { name: any_of_type, any_of: [{ type: int }, { type: decimal }] }
            "#).into_iter(),
        1 // this includes named type any_of_type
    ),
    case::one_of_constraint(
        load(r#" // For a schema with one_of constraint as below:
                type:: { name: one_of_type, one_of: [{ type: int }, { type: decimal }] }
            "#).into_iter(),
        1 // this includes named type one_of_type
    ),
    case::not_constraint(
        load(r#" // For a schema with not constraint as below:
                type:: { name: not_type, not: { type: int } }
            "#).into_iter(),
        1 // this includes named type not_type
    ),
    case::ordred_elements_constraint(
        load(r#" // For a schema with ordered_elements constraint as below:
                type:: { name: ordred_elements_type, ordered_elements: [ symbol, { type: int, occurs: optional }, ] }
            "#).into_iter(),
        1 // this includes named type ordered_elements_type
    ),
    )]
    fn owned_elements_to_schema<'a, I: Iterator<Item = OwnedElement>>(
        owned_elements: I,
        total_types: usize,
    ) {
        // create a type_store and resolver instance to be used for loading OwnedElements as schema
        let type_store = &mut TypeStore::new();
        let mut resolver = Resolver::new(vec![]);

        // create a isl from owned_elements and verifies if the result is `ok`
        let isl = resolver.isl_schema_from_elements(owned_elements, "my_schema.isl");
        assert_eq!(isl.is_ok(), true);

        // create a schema from isl and verifies if the result is `ok`
        let schema =
            resolver.schema_from_isl_schema(isl.unwrap(), "my_schema.isl", type_store, None);
        assert_eq!(schema.is_ok(), true);

        // check if the types of the created schema matches with the actual types specified by test case
        let types: Vec<TypeRef> = schema.unwrap().get_types().collect();
        assert_eq!(types.len(), total_types);
    }

    #[rstest(
        valid_values, invalid_values, schema,
        case::type_constraint(
            load(r#"
                5
                0
                -2
            "#),
            load(r#"
                false
                "hello"
                5.4
            "#),
            load_schema_from_text(r#" // For a schema with named type as below: 
                type:: { name: my_int, type: int }
            "#),
        ),
        case::not_constraint(
            load(r#"
                true
                "hello"
                5.4
                6e10
            "#),
            load(r#"
                5
                0
                -1
            "#),
            load_schema_from_text(r#" // For a schema with not constraint as below: 
                type:: { name: not_type, not: { type: int } }
            "#),
        ),
        case::one_of_constraint(
            load(r#"
                5
                -5
                5.4
                -5.4
            "#),
            load(r#"
                false
                "hello"
                hey
            "#),
            load_schema_from_text(r#" // For a schema with one_of constraint as below: 
                type:: { name: one_of_type, one_of: [int, decimal] }
            "#),
        ),
        // TODO: add a test case for all_of constraint
        case::any_of_constraint(
            load(r#"
                5
                5.4
                true
            "#),
            load(r#"
                "hello"
                hey
                6e10
            "#),
            load_schema_from_text(r#" // For a schema with any_of constraint as below: 
                type:: { name: any_of_type, any_of: [int, decimal, bool] }
            "#),
        ),
    )]
    fn type_validation(
        valid_values: Vec<OwnedElement>,
        invalid_values: Vec<OwnedElement>,
        schema: Rc<Schema>,
    ) {
        let type_ref: TypeRef = schema.get_types().next().expect("Loaded schema was empty.");
        // check for validation without any violations
        for valid_value in valid_values.iter() {
            // there is only a single type in each schema defined above hence validate with that type
            let validation_result = type_ref.validate(valid_value);
            assert_eq!(validation_result.is_ok(), true);
        }
        // check for violations due to invalid values
        for invalid_value in invalid_values.iter() {
            // there is only a single type in each schema defined above hence validate with that type
            let validation_result = type_ref.validate(invalid_value);
            assert_eq!(validation_result.is_err(), true);
        }
    }
}
