# Ion schema sandbox (Experimental package)

Ion schema sandbox is a tool to validate an Ion value against a [schema](https://amazon-ion.github.io/ion-schema/docs/spec/isl-1-0-spec.html) provided by user.

_**Please note, at this stage the code within this package is considered experimental and should not be used for production.**_

## Local Usage
For local usage follow the below steps.

1. Ensure `wasm-pack` is installed on your machine by running the following command; if not, install it from [here](https://rustwasm.github.io/wasm-pack/installer/):
```bash
wasm-pack --version
```
2. Ensure `npm` is installed on your machine; see [here](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) for more details:
```bash
npm --version
```
3. The easiest way to clone the `wasm-schema-sandbox` repository is to run the following command:
```bash
git clone --recursive https://github.com/amazon-ion/ion-schema-rust.git
```
4. Enter the `wasm-schema-sandbox` root directory:
```bash
cd ion-schema-rust/wasm-schema-sandbox
```
5. Run `wasm-pack build`:
```
wasm-pack build
```
6. Start the node server from `wasm-schema-sandbox` package's `www` directory:
```bash
cd www
npm install
npm run start
```
Note: In this step run `npm audit fix` if there are any issues with installing dependencies.
7. In your browser go to `http://localhost:8000/`

## Development
`Ion schema sandbox` uses [WebAssembly (Wasm)](https://webassembly.org/) for integrating the front-end with ion-schema-rust back-end.
Considering this, please install the `wasm-pack` by following the instructions [here](https://github.com/rustwasm/wasm-pack#-prerequisities).

Upon any changes to the package's Rust dependencies (E.g. `ion-schema-rust`) or the wasm code under `./src/lib` of this package, you need to rebuild the Wasm package using the following command from the root of this package:
```bash
wasm-pack build --target web
```

_**Please note, as the package is experimental at this stage, all HTML code and assets reside in this package, but this doesn't necessarily mean that it'll be the case in the future.**_

## Dependencies
| Package                                                                | License                                                                                         |
|------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------|
| [ace Editor](https://ace.c9.io/)                                       | [BSD License](https://github.com/ajaxorg/ace/blob/master/LICENSE)                               |
| [node](https://nodejs.org/en/)                                         | [MIT License](https://github.com/nodejs/node/blob/main/LICENSE)                                 |
| [wasm-bindgen](https://github.com/rustwasm/wasm-bindgen)               | [Apache License Version 2.0](https://github.com/rustwasm/wasm-bindgen/blob/main/LICENSE-APACHE) | 
| [wasm-pack](https://github.com/rustwasm/wasm-pack)                     | [Apache License Version 2.0](https://github.com/rustwasm/wasm-pack/blob/master/LICENSE-APACHE)  |