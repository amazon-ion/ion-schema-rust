fn main() {
    // Ensure that the macro-generated tests get regenerated if ion-schema-tests changes.
    println!("cargo:rerun-if-changed=../ion-schema-tests/");
}
