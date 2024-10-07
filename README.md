# Amazon Ion Schema Rust
[![Crate](https://img.shields.io/crates/v/ion-schema.svg)](https://crates.io/crates/ion-schema)
[![Docs](https://docs.rs/ion-schema/badge.svg)](https://docs.rs/ion-schema/latest/ion_schema/)
[![License](https://img.shields.io/hexpm/l/plug.svg)](https://github.com/amazon-ion/ion-schema-rust/blob/main/LICENSE)
[![Rust](https://github.com/amazon-ion/ion-schema-rust/workflows/CI%20Build/badge.svg)](https://github.com/amazon-ion/ion-schema-rust/actions?query=workflow%3A%22CI+Build%22)

An implementation of [Amazon Ion Schema](https://amazon-ion.github.io/ion-schema) in Rust.

If you want to try out a web tool to validate your Ion values using `ion-schema-rust`, please visit: https://amazon-ion.github.io/ion-schema/sandbox

**This package is considered experimental, under active/early development, and the API is subject to change.**

## Getting Started

The following rust code sample is a simple example of how to use this API.

### Example schema `my_schema.isl`

This file (`my_schema.isl`) defines a new type (`my_int_type`) based on Ion's `int` type.
```
schema_header::{
  imports: [],
}

type::{
  name: my_int_type,
  type: int,
}

schema_footer::{
}
```

### Loading a schema and validating an Ion value
```rust
use ion_rs::Element;
use ion_schema::authority::{DocumentAuthority, FileSystemDocumentAuthority};
use ion_schema::result::{IonSchemaResult, ValidationResult};
use ion_schema::schema::Schema;
use ion_schema::system::SchemaSystem;
use ion_schema::types::TypeDefinition;
use ion_schema::IonSchemaElement;
use std::fmt::Debug;
use std::path::Path;
use std::sync::Arc;

fn main() -> IonSchemaResult<()> {
    // Create authorities vector containing all the authorities that will be used to load a schema based on schema id
    let document_authorities: Vec<Box<dyn DocumentAuthority>> = vec![Box::new(
        FileSystemDocumentAuthority::new(Path::new("schemas")), // provide a path to the authority base folder containing schemas
    )];

    // Create a new schema system from given document authorities
    let mut schema_system = SchemaSystem::new(document_authorities);

    // Provide schema id for the schema you want to load (schema_id is the schema file name here)
    let schema_id = "my_schema.isl";

    // Load schema
    let schema: Arc<Schema> = schema_system.load_schema(schema_id)?;

    // Retrieve a particular type from this schema
    let type_ref: TypeDefinition = schema.get_type("my_int_type").unwrap();

    let valid_element: Element = 5.into();
    let invalid_element: Element = 5e3.into();
    let invalid_document_element: Vec<Element> = vec![5.into(), true.into(), 6e3.into()];

    // Validate data based on the type: 'my_int_type'
    check_value(&valid_element, &type_ref); // this validation passes as the value satisfies integer type constraint
    check_value(&invalid_element, &type_ref); // this returns violation as 'my_int_type' expects an integer value
    check_value(&invalid_document_element, &type_ref); // this returns violation as 'my_int_type' expects an integer value

    Ok(())
}

// Verify if the given value is valid and print violation for invalid value
fn check_value<I: Into<IonSchemaElement> + Debug + Clone>(value: I, type_ref: &TypeDefinition) {
    let validation_result: ValidationResult = type_ref.validate(value.to_owned());
    if let Err(violation) = validation_result {
        println!("{}", value.into());
        println!("{:#?}", violation);
    }
}
```

### Output
When run, the code above produces the following output:
```
5e3
Violation {
    constraint: "my_int_type",
    code: TypeConstraintsUnsatisfied,
    message: "value didn't satisfy type constraint(s)",
    ion_path: (),
    violations: [
        Violation {
            constraint: "type_constraint",
            code: TypeMismatched,
            message: "expected type Int, found Float",
            ion_path: (),
            violations: [],
        },
    ],
}
/* Ion document */ 5 true 6e3 /* end */
Violation {
    constraint: "my_int_type",
    code: TypeConstraintsUnsatisfied,
    message: "value didn't satisfy type constraint(s)",
    ion_path: (),
    violations: [
        Violation {
            constraint: "type_constraint",
            code: TypeMismatched,
            message: "expected type Int, found document",
            ion_path: (),
            violations: [],
        },
    ],
}
```

For more getting started examples, please visit: https://amazon-ion.github.io/ion-schema/docs/cookbook/ion-schema-rust-getting-started

## Development

This repository contains [git submodules](https://git-scm.com/docs/git-submodule)
called `ion-schema-schemas` and `ion-schema-tests`, which holds test data used by
this library's unit tests.

The easiest way to clone the `ion-schema-rust` repository and initialize its submodules
is to run the following command:

```bash
$ git clone --recursive https://github.com/amazon-ion/ion-schema-rust.git ion-schema-rust
```

Alternatively, the submodule may be initialized independently from the clone
by running the following commands:

```bash
$ git submodule init
$ git submodule update
```

Building the project,
```bash
$ cargo build --workspace --all-targets
```

Running all tests for `ion-schema-rust`,
```bash
$ cargo test --workspace
```

## Examples

The repository contains an `examples/` folder which is a CLI tool to load and validate schema.

To load a schema using the examples CLI:
```bash
$ cargo run --package ion-schema --example schema load --directory <DIRECTORY> --schema <SCHEMA_FILE> 
```

To validate an ion value against a schema type using the examples CLI:
```bash
$ cargo run --package ion-schema --example schema validate --directory <DIRECTORY> --schema <SCHEMA_FILE> --input <INPUT_FILE> --type <TYPE>
```

For more information on how to use the examples CLI, run the following command:
```bash
$ cargo run --package ion-schema --example schema help  
```

## License

This library is licensed under the Apache-2.0 License.

