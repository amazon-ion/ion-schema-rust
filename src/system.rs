use std::rc::Rc;
use std::collections::HashMap;
use crate::authority::Authority;
use crate::schema::Schema;
use crate::result::{IonSchemaResult, unresolvable_schema_error};

/// Provides functions for instantiating instances of [Schema].
///
/// To create an instance, use [IonSchemaSystemBuilder].
pub struct SchemaSystem {
    authorities: Vec<Box<dyn Authority>>,
    resolved_schema_cache: HashMap<String, Rc<Schema>>
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
        let id = id.as_ref();
        for authority in self.authorities.iter() {
            if let Some(schema) = self.resolved_schema_cache.get(id) {
                return Ok(Rc::clone(schema));
            }
            if let Ok(schema) = authority.resolve(id.parse().unwrap()) {
                // If schema is resolved then add it to the schema cache
                let schema_rc = Rc::new(schema);
                self.resolved_schema_cache.insert(id.parse().unwrap(), Rc::clone(&schema_rc));
                return Ok(schema_rc);
            }
        }
        unresolvable_schema_error("Unable to load schema: ".to_owned() + id)
    }

    /// Returns the cache for this [SchemaSystem] which has caches loaded [Schema]s
    fn get_schema_cache(&self) -> &HashMap<String, Rc<Schema>>{
        &self.resolved_schema_cache
    }

    /// Returns authorities associated with this [SchemaSystem]
    fn get_authorities(&self) -> &Vec<Box<dyn Authority>> {
        &self.authorities
    }

    /// Adds the provided authority to the list of [Authority]s.
    fn add_authority(mut self, authority: Box<dyn Authority>) -> Self {
        self.authorities.push(authority);
        SchemaSystem::new(self.authorities)
    }

    /// Replaces the list of [Authority]s with a list containing only the specified authority.
    fn with_authority(mut self, authority: Box<dyn Authority>) -> Self {
        let mut authorities: Vec<Box<dyn Authority>> = vec![authority];
        self.authorities = authorities;
        SchemaSystem::new(self.authorities)
    }

    /// Replaces the list of [Authority]s with the specified list of [Authority]s.
    fn with_authorities(mut self, authorities: Vec<Box<dyn Authority>>) -> Self {
        let mut authorities: Vec<Box<dyn Authority>> = authorities;
        self.authorities = authorities;
        SchemaSystem::new(self.authorities)
    }
}

#[cfg(test)]
mod schema_system_tests {
    use super::*;
    use std::path::Path;
    use crate::authority::FileSystemAuthority;

    #[test]
    fn schema_system_add_authorities_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("src")))]);
        schema_system = schema_system.add_authority(Box::new(FileSystemAuthority::new(Path::new("test"))));
        let schema_system_authorities = schema_system.get_authorities();
        assert_eq!(2, schema_system_authorities.len());
        assert_eq!(Path::new("src").file_name(), schema_system_authorities.get(0).unwrap().get_base_path().file_name());
        assert_eq!(Path::new("test").file_name(), schema_system_authorities.get(1).unwrap().get_base_path().file_name());
    }

    #[test]
    fn schema_system_with_authority_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("src")))]);
        schema_system = schema_system.with_authority(Box::new(FileSystemAuthority::new(Path::new("test"))));
        let schema_system_authorities = schema_system.get_authorities();
        assert_eq!(1, schema_system_authorities.len());
        assert_eq!(Path::new("test").file_name(), schema_system_authorities.get(0).unwrap().get_base_path().file_name());
    }

    #[test]
    fn schema_system_with_authorities_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("src")))]);
        schema_system = schema_system.with_authorities(vec![Box::new(FileSystemAuthority::new(Path::new("test"))), Box::new(FileSystemAuthority::new(Path::new("ion")))]);
        let schema_system_authorities = schema_system.get_authorities();
        assert_eq!(2, schema_system_authorities.len());
        assert_eq!(Path::new("test").file_name(), schema_system_authorities.get(0).unwrap().get_base_path().file_name());
        assert_eq!(Path::new("ion").file_name(),schema_system_authorities.get(1).unwrap().get_base_path().file_name());
    }

    #[test]
    fn schema_system_load_schema_test() {
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(Path::new("ion-schema-tests/schema")))]);
        let schema = schema_system.load_schema("Customer.isl".to_owned()).unwrap();
        assert_eq!(schema.get_id(), &"Customer.isl".to_owned());
        assert_eq!(schema.get_content().len(), 5);
    }
}