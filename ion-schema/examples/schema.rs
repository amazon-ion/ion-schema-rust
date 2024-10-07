#[macro_use]
extern crate clap;
use clap::{App, ArgMatches};
use ion_rs::WriteConfig;
use ion_rs::{Element, SequenceWriter, StructWriter, TextFormat, ValueWriter, Writer};
use ion_schema::authority::{DocumentAuthority, FileSystemDocumentAuthority};
use ion_schema::result::IonSchemaResult;
use ion_schema::system::SchemaSystem;
use std::fs;
use std::path::Path;
use std::str::from_utf8;

fn main() -> IonSchemaResult<()> {
    // load the YAML file which has all the CLI commands information stored in it
    let yaml = load_yaml!("cli.yaml");
    let matches = App::from_yaml(yaml).get_matches();
    let (command_name, command_args) = matches.subcommand();
    let command_args = command_args.unwrap();

    match command_name {
        "load" => load(command_args)?,
        "validate" => validate(command_args)?,
        _ => eprintln!("command name: {} not found", command_name),
    }

    Ok(())
}

fn load(command_args: &ArgMatches) -> IonSchemaResult<()> {
    // Extract the user provided authorities
    let authorities: Vec<_> = command_args.values_of("directories").unwrap().collect();

    // Extract schema file provided by user
    let schema_id = command_args.value_of("schema").unwrap();

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
    println!("Schema: {:#?}", schema_system.load_schema(schema_id)?);

    Ok(())
}

fn validate(command_args: &ArgMatches) -> IonSchemaResult<()> {
    // Extract the user provided authorities
    let authorities: Vec<_> = command_args.values_of("directories").unwrap().collect();

    // Extract schema file provided by user
    let schema_id = command_args.value_of("schema").unwrap();

    // Extract the schema type provided by user
    let schema_type = command_args.value_of("type").unwrap();

    // Extract Ion value provided by user
    let input_file = command_args.value_of("input").unwrap();
    let value = fs::read(input_file).expect("Can not load given ion file");
    let owned_elements = Element::read_all(value).expect("parsing failed unexpectedly");

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

    // get the type provided by user from the schema file
    let type_ref = schema.unwrap().get_type(schema_type).unwrap();

    // create a text writer to make the output
    let output = vec![];
    let write_config = WriteConfig::<ion_rs::v1_0::Text>::new(TextFormat::Pretty);
    let mut writer = Writer::new(write_config, output)?;

    // validate owned_elements according to type_ref
    for owned_element in owned_elements {
        // create a validation report with validation result, value, schema and/or violation
        let mut struct_writer = writer.struct_writer()?;
        let validation_result = type_ref.validate(&owned_element);
        match validation_result {
            Ok(_) => {
                struct_writer.field_writer("result").write_string("Valid")?;
                struct_writer
                    .field_writer("value")
                    .write_string(format!("{owned_element}"))?;
                struct_writer
                    .field_writer("schema")
                    .write_string(schema_id)?;
            }
            Err(_) => {
                struct_writer
                    .field_writer("result")
                    .write_string("Invalid")?;
                struct_writer
                    .field_writer("violation")
                    .write_string(format!("{:#?}", validation_result.unwrap_err()))?;
            }
        }
        struct_writer.close()?;
    }
    let output = writer.close()?;
    println!("Validation report:");
    println!("{}", from_utf8(&output).unwrap());
    Ok(())
}
