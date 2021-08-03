use crate::authority::Authority;
use crate::result::{
    invalid_schema_error, invalid_schema_error_raw, unresolvable_schema_error, IonSchemaResult,
};
use crate::schema::Schema;
use crate::types::TypeRef::{AliasType, AnonymousType};
use crate::types::{Type, TypeRef};
use ion_rs::value::owned::{text_token, OwnedSymbolToken};
use ion_rs::value::{Element, Sequence, Struct};
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

/// Provides functions for instantiating instances of [Schema].
pub struct SchemaSystem {
    authorities: Vec<Box<dyn Authority>>,
    resolved_schema_cache: HashMap<String, Rc<Schema>>,
}

// TODO: make methods public based on the requirements
// TODO: need for a schema resolver based on the load_schema implementation
impl SchemaSystem {
    pub fn new(authorities: Vec<Box<dyn Authority>>) -> Self {
        Self {
            authorities,
            resolved_schema_cache: HashMap::new(),
        }
    }

    /// Resolves all the deferred type references using types collected while loading a schema
    fn resolve_deferred_type_references(
        &self,
        types: HashMap<String, Type>,
        id: &str,
    ) -> IonSchemaResult<HashMap<String, Type>> {
        let mut resolved_types: HashMap<String, Type> = types.clone();
        for (type_name, type_value) in types {
            let type_references: &[TypeRef] = type_value.deferred_type_references();
            for type_reference in type_references {
                match type_reference {
                    AliasType(name) => {
                        if !resolved_types.contains_key(name) {
                            return Err(invalid_schema_error_raw(format!(
                                "type reference {:} does not exists for schema: {:}",
                                name, id
                            )));
                        }
                    }
                    AnonymousType(anonymous_type) => {
                        resolved_types
                            .insert(anonymous_type.name().to_owned(), anonymous_type.to_owned());
                    }
                    _ => {}
                }
            }
        }
        Ok(resolved_types)
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
            return match authority.elements(id) {
                Err(error) => Err(error),
                Ok(schema_content) => {
                    let mut types: HashMap<String, Type> = HashMap::new();
                    let mut found_header = false;
                    let mut found_footer = false;
                    for value in schema_content {
                        let annotations: Vec<&OwnedSymbolToken> = value.annotations().collect();
                        // load header for schema
                        if annotations.contains(&&text_token("schema_header")) {
                            found_header = true;
                            let imports = try_to!(value.as_struct())
                                .get("imports")
                                .and_then(|it| it.as_sequence());

                            if imports.is_some() {
                                for import in imports.unwrap().iter() {
                                    let import_id = try_to!(try_to!(import.as_struct()).get("id"));
                                    let imported_schema =
                                        self.load_schema(try_to!(import_id.as_str()))?;
                                    types.extend(imported_schema.types().to_owned().into_iter());
                                }
                            }
                        }
                        // load types for schema
                        else if annotations.contains(&&text_token("type")) {
                            let inline_type: Type = try_to!(value.as_struct()).try_into()?;
                            types.insert(inline_type.name().to_owned(), inline_type);
                        }
                        // load footer for schema
                        else if annotations.contains(&&text_token("schema_footer")) {
                            found_footer = true;
                        } else {
                            //TODO: this should throw an error for anything other than schema_header, type and schema_footer is written inside schema
                            continue;
                        }
                    }
                    if found_footer ^ found_header {
                        return invalid_schema_error("For any schema while a header and footer are both optional, a footer is required if a header is present (and vice-versa).");
                    }
                    types = self.resolve_deferred_type_references(types, id)?;
                    let schema = Schema::new(id, types.into_iter());
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
    fn authorities(&self) -> &[Box<dyn Authority>] {
        &self.authorities
    }

    /// Adds the provided authority to the list of [Authority]s.
    fn add_authority(&mut self, authority: Box<dyn Authority>) {
        self.authorities.push(authority);
    }

    /// Replaces the list of [Authority]s with a list containing only the specified authority.
    fn with_authority(&mut self, authority: Box<dyn Authority>) {
        let authorities: Vec<Box<dyn Authority>> = vec![authority];
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
        let root_path = Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut schema_system = SchemaSystem::new(vec![Box::new(FileSystemAuthority::new(
            &root_path.join(Path::new("ion-schema-tests/")),
        ))]);
        // load schema for core types (BaseType TypeRef)
        let schema = schema_system
            .load_schema("constraints/type/validation_int.isl".to_owned())
            .unwrap();
        assert_eq!(schema.id(), "constraints/type/validation_int.isl");
    }
}
