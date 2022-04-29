// TODO: remove the following line once we have a basic implementation ready
#![allow(dead_code, unused_variables)]

/// A [`try`]-like macro to workaround the [`Option`]/[`Result`] nested APIs.
/// These API require checking the type and then calling the appropriate getter function
/// (which returns a None if you got it wrong). This macro turns the `None` into
/// an `IonSchemaError` which cannot be currently done with `?`.
macro_rules! try_to {
    ($getter:expr) => {
        match $getter {
            Some(value) => value,
            None => invalid_schema_error(format!("Missing a value: {}", stringify!($getter)))?,
        }
    };
}

// TODO: consider changing some of these modules to public if required
pub mod authority;
mod constraint;
mod import;
pub mod isl;
pub mod result;
pub mod schema;
pub mod system;
pub mod types;
mod violation;

/// Re-export of the ion-rs dependency that is part of our public API.
pub mod external {
    pub use ion_rs;
}
