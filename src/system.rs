use crate::schema::{Schema, SchemaImpl};
use std::fmt::Debug;
use crate::authority::{FileSystemAuthority, Authority};
use ion_rs::value::reader::{ElementReader, element_reader};
use std::{fs, env, io};
use std::path::Path;
use std::env::join_paths;
use std::ffi::OsString;
use std::fs::{canonicalize, File};
use std::io::Read;

/// Provides functions for instantiating instances of [Schema].
pub trait SchemaSystem: Debug + Clone {
    type Authority: Authority;
    type Schema: Schema;

    /// Requests each of the provided [Authority]s, in order, to resolve the requested schema id
    /// until one successfully resolves it.
    /// If an Authority throws an exception, resolution silently proceeds to the next Authority.
    fn load_schema(self, id: String) -> io::Result<Self::Schema>;

    /// Adds the provided authority to the list of [Authority]s.
    fn add_authority(self, authority: Self::Authority) -> Self;

    /// Replaces the list of [Authority]s with a list containing only the specified authority.
    fn with_authority(&mut self, authority: Self::Authority) -> Self;

    /// Replaces the list of [Authority]s with the specified list of [Authority]s.
    fn with_authorities(&mut self, authorities: Vec<Self::Authority>) -> Self;
}


// TODO: add constraint as a property
#[derive(Debug, Clone)]
pub struct SchemaSystemImpl<'a> {
    authorities: Vec<FileSystemAuthority<'a>>,
}

impl<'a> SchemaSystemImpl<'a> {
    fn new(authorities: Vec<FileSystemAuthority<'a>>) -> Self {
        Self {
            authorities,
        }
    }

    pub fn get_authorities(&self) -> &Vec<FileSystemAuthority<'a>> {
        &self.authorities
    }
}

impl<'a> SchemaSystem for SchemaSystemImpl<'a> {
    type Authority = FileSystemAuthority<'a>;
    type Schema = SchemaImpl<'a>;

    fn load_schema(self, id: String) -> io::Result<Self::Schema> {
        let mut loaded_schema: SchemaImpl = SchemaImpl::new(self.clone(), vec![], id.to_owned());
        // check if the schema authorities has the given schema to be loaded based on id
        for authority in self.authorities.iter() {
            let absolute_path = authority.get_base_path().join(&id);
            // if absolute_path exists for the given id then load schema with file contents
            let ion_data = fs::read_to_string(absolute_path).expect("Unable to read file!");
            let mut iterator = element_reader().iterate_over(ion_data.as_ref()).unwrap();
            let mut peekable_iterator = &mut iterator.peekable();
            if peekable_iterator.peek().is_some() {
                loaded_schema = SchemaImpl::new(self.clone(), peekable_iterator.collect(), id.to_owned());
                break;
            };
        }
        Ok(loaded_schema)
    }

    fn add_authority(mut self, authority: Self::Authority) -> Self {
        self.authorities.push(authority);
        SchemaSystemImpl::new(self.authorities)
    }

    fn with_authority(&mut self, authority: Self::Authority) -> Self {
        let mut authorities: Vec<FileSystemAuthority> = vec![authority];
        self.authorities = authorities;
        SchemaSystemImpl::new(self.authorities.clone())
    }

    fn with_authorities(&mut self, authorities: Vec<Self::Authority>) -> Self {
        let mut authorities: Vec<FileSystemAuthority> = authorities;
        self.authorities = authorities;
        SchemaSystemImpl::new(self.authorities.clone())
    }
}


#[cfg(test)]
mod schema_system_tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn schema_system_add_authorities_test() {
        let mut schema_system = SchemaSystemImpl::new(vec![FileSystemAuthority::new(Path::new("src"))]);
        schema_system = schema_system.add_authority(FileSystemAuthority::new(Path::new("test")));
        let schema_system_authorities = schema_system.get_authorities();
        assert_eq!(2, schema_system_authorities.len());
        assert_eq!(Path::new("src").file_name(), schema_system_authorities.get(0).unwrap().get_base_path().file_name());
        assert_eq!(Path::new("test").file_name(), schema_system_authorities.get(1).unwrap().get_base_path().file_name());
    }

    #[test]
    fn schema_system_with_authority_test() {
        let mut schema_system = SchemaSystemImpl::new(vec![FileSystemAuthority::new(Path::new("src"))]);
        schema_system = schema_system.with_authority(FileSystemAuthority::new(Path::new("test")));
        let schema_system_authorities = schema_system.get_authorities();
        assert_eq!(1, schema_system_authorities.len());
        assert_eq!(Path::new("test").file_name(), schema_system_authorities.get(0).unwrap().get_base_path().file_name());
    }

    #[test]
    fn schema_system_with_authorities_test() {
        let mut schema_system = SchemaSystemImpl::new(vec![FileSystemAuthority::new(Path::new("src"))]);
        schema_system = schema_system.with_authorities(vec![FileSystemAuthority::new(Path::new("test")), FileSystemAuthority::new(Path::new("ion"))]);
        let schema_system_authorities = schema_system.get_authorities();
        assert_eq!(2, schema_system_authorities.len());
        assert_eq!(Path::new("test").file_name(), schema_system_authorities.get(0).unwrap().get_base_path().file_name());
        assert_eq!(Path::new("ion").file_name(),schema_system_authorities.get(1).unwrap().get_base_path().file_name());
    }

    #[test]
    fn schema_system_load_schema_test() {
        let mut schema_system = SchemaSystemImpl::new(vec![FileSystemAuthority::new(Path::new("./ion-schema-schemas/isl"))]);
        let schema = schema_system.load_schema("schema.isl".to_owned()).unwrap();
        assert_eq!(schema.get_id(), &"schema.isl".to_owned());
        assert_eq!(schema.get_content().len(), 8);
    }
}