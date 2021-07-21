use crate::authority::Authority;
use crate::result::{unresolvable_schema_error, IonSchemaResult};
use crate::schema::Schema;
use std::collections::HashMap;
use std::rc::Rc;

/// Provides functions for instantiating instances of [Schema].
pub struct SchemaSystem {
    authorities: Vec<Box<dyn Authority>>,
    resolved_schema_cache: HashMap<String, Rc<Schema>>,
}

impl SchemaSystem {
    pub fn new(authorities: Vec<Box<dyn Authority>>) -> Self {
        Self {
            authorities,
            resolved_schema_cache: HashMap::new(),
        }
    }

    /// Requests each of the provided [Authority]s, in order, to resolve the requested schema id
    /// until one successfully resolves it.
    /// If an Authority throws an exception, resolution silently proceeds to the next Authority.
    fn load_schema<A: AsRef<str>>(&mut self, id: A) -> IonSchemaResult<Rc<Schema>> {
        let id: &str = id.as_ref();
        for authority in &self.authorities {
            if let Some(schema) = self.resolved_schema_cache.get(id) {
                return Ok(Rc::clone(schema));
            }
            return match authority.resolve(id) {
                Err(error) => Err(error),
                Ok(schema) => {
                    // If schema is resolved then add it to the schema cache
                    let schema_rc = Rc::new(schema);
                    self.resolved_schema_cache
                        .insert(id.to_owned(), Rc::clone(&schema_rc));
                    Ok(schema_rc)
                }
            };
        }
        unresolvable_schema_error("Unable to load schema: ".to_owned() + id)
    }

    /// Returns the cache for this [SchemaSystem] which has cached loaded [Schema]s
    fn schema_cache(&self) -> &HashMap<String, Rc<Schema>> {
        &self.resolved_schema_cache
    }

    /// Returns authorities associated with this [SchemaSystem]
    fn authorities(&self) -> &Vec<Box<dyn Authority>> {
        &self.authorities
    }

    /// Adds the provided authority to the list of [Authority]s.
    fn add_authority(&mut self, authority: Box<dyn Authority>) {
        self.authorities.push(authority);
    }

    /// Replaces the list of [Authority]s with a list containing only the specified authority.
    fn with_authority(&mut self, authority: Box<dyn Authority>) {
        let mut authorities: Vec<Box<dyn Authority>> = vec![authority];
        self.authorities = authorities;
    }

    // TODO: Use IntoIterator here instead of a Vec
    /// Replaces the list of [Authority]s with the specified list of [Authority]s.
    fn with_authorities(&mut self, authorities: Vec<Box<dyn Authority>>) {
        self.authorities = authorities;
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
        let mut root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(
            &root_path.join(Path::new("ion-schema-tests/schema")),
        ))]);
        let schema = schema_system
            .load_schema("Customer.isl".to_owned())
            .unwrap();
        assert_eq!(schema.id(), &"Customer.isl".to_owned());
        assert_eq!(schema.content().len(), 5);
    }
}
