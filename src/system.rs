use crate::schema::{Authority, Schema};
use std::fmt::Debug;

/// Provides functions for instantiating instances of [Schema].
///
/// To create an instance, use [IonSchemaSystemBuilder].

pub trait SchemaSystem: Debug + Clone {
    type Authority: Authority;
    type Schema: Schema;

    /// Requests each of the provided [Authority]s, in order, to resolve the requested schema id
    /// until one successfully resolves it.
    /// If an Authority throws an exception, resolution silently proceeds to the next Authority.
    fn load_schema(&self, id: String) -> Self::Schema;

    /// Adds the provided authority to the list of [Authority]s.
    fn add_authority(self, authority: Self::Authority) -> Self;

    /// Replaces the list of [Authority]s with a list containing only the specified authority.
    fn with_authority(mut self, authority: Self::Authority) -> Self;

    /// Replaces the list of [Authority]s with the specified list of [Authority]s.
    fn with_authorities(authorities: Vec<Self::Authority>) -> Self;
}

// TODO: Fill the trait
pub trait ConstraintFactory {}