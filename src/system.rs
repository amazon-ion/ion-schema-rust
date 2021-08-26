use crate::authority::Authority;
use crate::result::{
    invalid_schema_error, unresolvable_schema_error, IonSchemaError, IonSchemaResult,
};
use crate::schema::Schema;
use crate::types::Type;
use ion_rs::value::owned::{text_token, OwnedElement, OwnedSymbolToken};
use ion_rs::value::{Element, Sequence, Struct};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::ErrorKind;
use std::rc::Rc;

/// Provides a type cache reference which will be shared to resolve types during load_schema
pub type SharedTypeCache = Rc<RefCell<HashMap<String, Type>>>;

/// Provides functions to load [Schema] with [Type]s using authorities for [System]
pub struct Resolver {
    authorities: Vec<Box<dyn Authority>>,
    resolved_schema_cache: HashMap<String, Rc<Schema>>,
}

impl Resolver {
    pub fn new(authorities: Vec<Box<dyn Authority>>) -> Self {
        Self {
            authorities,
            resolved_schema_cache: HashMap::new(),
        }
    }

    pub fn schema_from_elements<I: Iterator<Item = OwnedElement>>(
        &mut self,
        elements: I,
        id: &str,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Rc<Schema>> {
        let mut types: HashMap<String, Type> = HashMap::new();
        let mut found_header = false;
        let mut found_footer = false;
        for value in elements {
            let annotations: Vec<&OwnedSymbolToken> = value.annotations().collect();
            // load header for schema
            if annotations.contains(&&text_token("schema_header")) {
                found_header = true;
                if let Some(imports) = try_to!(value.as_struct())
                    .get("imports")
                    .and_then(|it| it.as_sequence())
                {
                    for import in imports.iter() {
                        let import_id = try_to!(try_to!(import.as_struct()).get("id"));
                        let imported_schema =
                            self.load_schema(try_to!(import_id.as_str()), type_cache)?;
                        types.extend(imported_schema.get_types().to_owned().into_iter());
                    }
                }
            }
            // load types for schema
            else if annotations.contains(&&text_token("type")) {
                let type_def: Type =
                    Type::parse_from_ion_element(try_to!(value.as_struct()), type_cache)?;
                types.insert(type_def.name().to_owned(), type_def);
            }
            // load footer for schema
            else if annotations.contains(&&text_token("schema_footer")) {
                found_footer = true;
            } else {
                continue;
            }
        }
        if found_footer ^ found_header {
            return invalid_schema_error("For any schema while a header and footer are both optional, a footer is required if a header is present (and vice-versa).");
        }
        Ok(Rc::new(Schema::new(id, types.into_iter())))
    }

    /// Loads a [Schema] with resolved [Type]s using authorities and type_cache
    fn load_schema<A: AsRef<str>>(
        &mut self,
        id: A,
        type_cache: &SharedTypeCache,
    ) -> IonSchemaResult<Rc<Schema>> {
        let id: &str = id.as_ref();
        if let Some(schema) = self.resolved_schema_cache.get(id) {
            return Ok(Rc::clone(schema));
        }

        for authority in &self.authorities {
            return match authority.elements(id) {
                Err(error) => match error {
                    IonSchemaError::IoError { source } => match source.kind() {
                        ErrorKind::NotFound => continue,
                        _ => Err(IonSchemaError::IoError { source }),
                    },
                    _ => Err(error),
                },
                Ok(schema_content) => {
                    self.schema_from_elements(schema_content.into_iter(), id, type_cache)
                }
            };
        }
        unresolvable_schema_error("Unable to load schema: ".to_owned() + id)
    }
}

/// Provides functions for instantiating instances of [Schema].
pub struct SchemaSystem {
    resolver: Resolver,
}

// TODO: make methods public based on the requirements
impl SchemaSystem {
    pub fn new(authorities: Vec<Box<dyn Authority>>) -> Self {
        Self {
            resolver: Resolver::new(authorities),
        }
    }

    /// Requests each of the provided [Authority]s, in order, to resolve the requested schema id
    /// until one successfully resolves it.
    /// If an Authority throws an exception, resolution silently proceeds to the next Authority.
    fn load_schema<A: AsRef<str>>(&mut self, id: A) -> IonSchemaResult<Rc<Schema>> {
        self.resolver
            .load_schema(id, &Rc::new(RefCell::new(HashMap::new())))
    }

    /// Returns authorities associated with this [SchemaSystem]
    fn authorities(&mut self) -> &[Box<dyn Authority>] {
        &self.resolver.authorities
    }

    /// Adds the provided authority to the list of [Authority]s.
    fn add_authority(&mut self, authority: Box<dyn Authority>) {
        self.resolver.authorities.push(authority);
    }

    /// Replaces the list of [Authority]s with a list containing only the specified authority.
    fn with_authority(&mut self, authority: Box<dyn Authority>) {
        let authorities: Vec<Box<dyn Authority>> = vec![authority];
        self.resolver.authorities = authorities;
    }

    // TODO: Use IntoIterator here instead of a Vec
    /// Replaces the list of [Authority]s with the specified list of [Authority]s.
    fn with_authorities(&mut self, authorities: Vec<Box<dyn Authority>>) {
        self.resolver.authorities = authorities;
    }
}

#[cfg(test)]
mod schema_system_tests {
    use super::*;
    use crate::authority::FileSystemAuthority;
    use std::path::Path;

    #[test]
    fn schema_system_add_authorities_test() {
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("src")))]);
        schema_system.add_authority(Box::new(FileSystemAuthority::new(Path::new("test"))));
        let schema_system_authorities = schema_system.authorities();
        assert_eq!(2, schema_system_authorities.len());
    }

    #[test]
    fn schema_system_with_authority_test() {
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("src")))]);
        schema_system.with_authority(Box::new(FileSystemAuthority::new(Path::new("test"))));
        let schema_system_authorities = schema_system.authorities();
        assert_eq!(1, schema_system_authorities.len());
    }

    #[test]
    fn schema_system_with_authorities_test() {
        let mut schema_system =
            SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("src")))]);
        schema_system.with_authorities(vec![
            Box::new(FileSystemAuthority::new(Path::new("test"))),
            Box::new(FileSystemAuthority::new(Path::new("ion"))),
        ]);
        let schema_system_authorities = schema_system.authorities();
        assert_eq!(2, schema_system_authorities.len());
    }

    #[test]
    fn schema_system_load_schema_test() {
        let root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(
            &root_path.join(Path::new("ion-schema-tests/")),
        ))]);
        // load schema for core types
        let schema = schema_system
            .load_schema("constraints/type/validation_int.isl".to_owned())
            .unwrap();
        assert_eq!(schema.id(), "constraints/type/validation_int.isl");

        assert_eq!(schema.get_types().len(), 1);
    }
}
