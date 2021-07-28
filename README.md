# Amazon Ion Schema Rust

An implementation of [Amazon Ion Schema](http://amzn.github.io/ion-schema) in Rust.

## Getting Started

Example usage TBD

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

## License

This library is licensed under the Apache-2.0 License.

