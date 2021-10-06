#[macro_use]
extern crate clap;
use clap::App;
use ion_schema_rust::authority::{DocumentAuthority, FileSystemDocumentAuthority};
use ion_schema_rust::result::IonSchemaResult;
use ion_schema_rust::system::SchemaSystem;
use std::path::Path;

fn main() -> IonSchemaResult<()> {
    // load the YAML file which has all the CLI commands information stored in it
    let yaml = load_yaml!("cli.yaml");
    let matches = App::from_yaml(yaml).get_matches();

    // Extract the user provided authorities
    let authorities: Vec<_> = matches.values_of("authorities").unwrap().collect();

    // Extract schema file provided by user
    let schema_id = matches.value_of("input").unwrap();

    // Set up authorities vector
    let mut document_authorities: Vec<Box<dyn DocumentAuthority>> = vec![];

    for authority in authorities {
        document_authorities.push(Box::new(FileSystemDocumentAuthority::new(Path::new(
            authority,
        ))))
    }

    // Create a new schema system from given document authorities
    let mut schema_system = SchemaSystem::new(document_authorities);

    // load schema
    let schema = schema_system.load_schema(schema_id);

    if schema.is_ok() {
        eprintln!("Schema: {:?} was successfully loaded", schema.unwrap().id());
    } else {
        eprintln!("{:?}", schema.unwrap_err());
    }

    Ok(())
}
