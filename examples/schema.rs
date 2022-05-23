#[macro_use]
extern crate clap;
use clap::{App, ArgMatches};
use ion_rs::value::reader::element_reader;
use ion_rs::value::reader::ElementReader;
use ion_rs::value::writer::{ElementWriter, Format, TextKind};
use ion_rs::IonType;
use ion_rs::{RawTextWriter, Writer};
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
    let owned_elements = element_reader()
        .read_all(&value)
        .expect("parsing failed unexpectedly");

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
    let mut output = vec![];
    let mut writer = RawTextWriter::new(&mut output);

    // validate owned_elements according to type_ref
    for owned_element in owned_elements {
        // create a validation report with validation result, value, schema and/or violation
        writer.step_in(IonType::Struct)?;
        let validation_result = type_ref.validate(&owned_element);
        writer.set_field_name("result");
        match validation_result {
            Ok(_) => {
                writer.write_string("Valid")?;
                writer.set_field_name("value");
                const TEST_BUF_LEN: usize = 4 * 1024 * 1024;
                let mut buf = vec![0u8; TEST_BUF_LEN];
                let mut element_writer =
                    Format::Text(TextKind::Pretty).element_writer_for_slice(&mut buf)?;
                element_writer.write(&owned_element)?;
                let slice = element_writer.finish()?;
                let slice = from_utf8(slice).unwrap_or("<INVALID UTF-8>");
                writer.write_string(slice)?;
                writer.set_field_name("schema");
                writer.write_string(schema_id)?;
            }
            Err(_) => {
                writer.write_string("Invalid")?;
                writer.set_field_name("violation");
                writer.write_string(format!("{:#?}", validation_result.unwrap_err()))?;
            }
        }
        writer.step_out()?;
    }
    drop(writer);
    println!("Validation report:");
    println!("{}", from_utf8(&output).unwrap());
    Ok(())
}
