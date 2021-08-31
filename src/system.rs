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
pub type SharedTypeStore = Rc<RefCell<TypeStore>>;

pub type TypeId = usize;

/// Defines a cache that can be used to store resolved [Type]s of a [Schema]
#[derive(Debug, Clone)]
pub struct TypeStore {
    ids_by_name: HashMap<String, TypeId>,
    types_by_id: Vec<Type>,
}

impl TypeStore {
    pub fn new() -> Self {
        Self {
            ids_by_name: HashMap::new(),
            types_by_id: Vec::new(),
        }
    }

    /// Provides the [Type] associated with given name if it exists in the [TypeStore]  
    /// Otherwise returns None
    pub fn get_type_by_name(&self, name: &str) -> Option<&Type> {
        self.ids_by_name
            .get(name)
            .and_then(|id| self.types_by_id.get(*id))
    }

    /// Provides the [TypeId] associated with given name if it exists in the [TypeStore]  
    /// Otherwise returns None
    pub fn get_type_id_by_name(&self, name: &str) -> Option<&TypeId> {
        self.ids_by_name.get(name)
    }

    /// Provides the [Type] associated with given [TypeId] if it exists in the [TypeStore]  
    /// Otherwise returns None
    pub fn get_type_by_id(&self, id: TypeId) -> Option<&Type> {
        self.types_by_id.get(id)
    }

    /// Adds the [Type] and the associated name in the [TypeStore] and returns the [TypeId] for it
    /// If the name already exists in the [TypeStore] it returns the associated [TypeId]
    pub fn add_named_type(&mut self, name: &str, type_def: Type) -> TypeId {
        if let Some(exists) = self.ids_by_name.get(name) {
            return exists.to_owned();
        }
        let type_id = self.types_by_id.len();
        self.ids_by_name.insert(name.to_owned(), type_id);
        self.types_by_id.push(type_def);
        type_id
    }

    /// Adds the [Type] in the [TypeStore] and returns the [TypeId] for it
    pub fn add_anonymous_type(&mut self, type_def: Type) -> TypeId {
        let type_id = self.types_by_id.len();
        self.types_by_id.push(type_def);
        type_id
    }
}

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
        type_store: &SharedTypeStore,
    ) -> IonSchemaResult<Rc<Schema>> {
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
                            self.load_schema(try_to!(import_id.as_str()), type_store)?;
                    }
                }
            }
            // load types for schema
            else if annotations.contains(&&text_token("type")) {
                let type_def: Type =
                    Type::parse_from_ion_element(try_to!(value.as_struct()), type_store)?;
                // add the resolved type_def into type_store
                type_store
                    .borrow_mut()
                    .add_named_type(type_def.to_owned().name(), type_def);
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

        Ok(Rc::new(Schema::new(
            id,
            Rc::new(type_store.borrow().to_owned()),
        )))
    }

    /// Loads a [Schema] with resolved [Type]s using authorities and type_store
    fn load_schema<A: AsRef<str>>(
        &mut self,
        id: A,
        type_store: &SharedTypeStore,
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
                    self.schema_from_elements(schema_content.into_iter(), id, type_store)
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
            .load_schema(id, &Rc::new(RefCell::new(TypeStore::new())))
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
    }
}
