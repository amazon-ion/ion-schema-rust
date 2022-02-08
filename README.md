# Amazon Ion Schema Rust
[![Crate](https://img.shields.io/crates/v/ion-schema.svg)](https://crates.io/crates/ion-schema)
[![Docs](https://docs.rs/ion-schema/badge.svg)](https://docs.rs/ion-schema/latest/ion_schema/)
[![License](https://img.shields.io/hexpm/l/plug.svg)](https://github.com/amzn/ion-schema-rust/blob/main/LICENSE)
[![Rust](https://github.com/amzn/ion-schema-rust/workflows/CI%20Build/badge.svg)](https://github.com/amzn/ion-schema-rust/actions?query=workflow%3A%22CI+Build%22)

An implementation of [Amazon Ion Schema](http://amzn.github.io/ion-schema) in Rust.

**This package is considered experimental, under active/early development, and the API is subject to change.**

## Development

This repository contains [git submodules](https://git-scm.com/docs/git-submodule)
called `ion-schema-schemas` and `ion-schema-tests`, which holds test data used by
this library's unit tests.

The easiest way to clone the `ion-schema-rust` repository and initialize its submodules
is to run the following command:

```bash
$ git clone --recursive https://github.com/amzn/ion-schema-rust.git ion-schema-rust
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
$ cargo run --package ion-schema --example schema validate --directory <DIRECTORY> --schema <SCHEMA_FILE> --input <INPUT_FILE>
```

For more information on how to use the examples CLI, run the following command:
```bash
$ cargo run --package ion-schema --example schema help  
```

## License

This library is licensed under the Apache-2.0 License.

