//! Represents the [`IonSchemaResult`] type for error handling.
//!
//! [`IonSchemaResult<T, E>`][`IonSchemaResult`]  is the type used for returning and propagating errors.
//! It is an enum with the variants, Ok(T), representing success and containing a value,and Err(E), representing an [`IonSchemaError`].

mod invalid_schema_error;
mod isl_source_location;

pub use invalid_schema_error::InvalidSchemaError;
pub(crate) use isl_source_location::*;

use crate::violation::Violation;
use ion_rs::IonError;
use std::convert::Infallible;
use std::fmt::{Debug, Write};
use std::io;
use std::io::Error;
use thiserror::Error;

/// A unified Result type representing the outcome of method calls that may fail.
pub type IonSchemaResult<T> = Result<T, IonSchemaError>;

// This trait implementation allows us to use `IonSchemaResult` in things like `map_err` because it
// provides a conversion from IonSchemaResult to IonSchemaError.
impl From<Result<Infallible, IonSchemaError>> for IonSchemaError {
    fn from(value: Result<Infallible, IonSchemaError>) -> Self {
        value.unwrap_err()
    }
}

impl From<Result<Infallible, InvalidSchemaError>> for IonSchemaError {
    fn from(value: Result<Infallible, InvalidSchemaError>) -> Self {
        IonSchemaError::InvalidSchemaError(value.unwrap_err())
    }
}

impl TryFrom<Result<Infallible, IonSchemaError>> for InvalidSchemaError {
    type Error = IonSchemaError;

    fn try_from(value: Result<Infallible, IonSchemaError>) -> Result<Self, Self::Error> {
        match value.unwrap_err() {
            IonSchemaError::InvalidSchemaError(error) => Ok(error),
            other => Err(other),
        }
    }
}

/// A Result type representing the outcome of validation that may result in violations.
pub type ValidationResult = Result<(), Violation>;

/// Represents the different types of high-level failures that might occur when reading Ion Schema.
#[derive(Debug, Error, PartialEq)]
#[non_exhaustive]
pub enum IonSchemaError {
    /// Indicates that an io error occurred while loading a schema
    #[error(transparent)]
    IoError(#[from] IoError),

    /// Indicates failure for schema which has unresolvable imports/types
    #[error("{description}")]
    UnresolvableSchemaError { description: String },

    /// Indicates failure due to an invalid schema document
    #[error(transparent)]
    InvalidSchemaError(#[from] InvalidSchemaError),

    /// Indicates failure due to ion-rust error defined by IonError
    #[error("{0:?}")]
    IonError(#[from] IonError),
}

/// Wrapper for [`io::Error`] that implements [`PartialEq`].
#[derive(Debug, Error)]
#[error(transparent)]
pub struct IoError {
    source: io::Error,
}
impl AsRef<io::Error> for IoError {
    fn as_ref(&self) -> &Error {
        &self.source
    }
}
impl PartialEq for IoError {
    fn eq(&self, other: &Self) -> bool {
        // We can compare the io::Errors' ErrorKinds, offering a weak definition of equality.
        self.source.kind() == other.source.kind()
    }
}
impl From<io::Error> for IonSchemaError {
    fn from(value: Error) -> Self {
        Self::IoError(IoError { source: value })
    }
}

/// A convenience method for creating an IonSchemaResult containing an IonSchemaError::UnresolvableSchemaError
/// with the provided description text.
pub fn unresolvable_schema_error<T, S: AsRef<str>>(description: S) -> IonSchemaResult<T> {
    Err(IonSchemaError::UnresolvableSchemaError {
        description: description.as_ref().to_string(),
    })
}

/// A convenience method for creating an  IonSchemaError::InvalidSchemaError with the provided operation
/// text.
pub fn unresolvable_schema_error_raw<S: AsRef<str>>(description: S) -> IonSchemaError {
    IonSchemaError::UnresolvableSchemaError {
        description: description.as_ref().to_string(),
    }
}

/// Creates an [`IonSchemaError::InvalidSchemaError`]
macro_rules! invalid_schema {
    ($($schema_id:ident, $($loc:ident,)?)? $fmt_string:literal $($args:tt)*) => {
        Result::Err(
            crate::result::IonSchemaError::InvalidSchemaError(crate::result::InvalidSchemaError::builder(format!($fmt_string $($args)*))
                $(.with_schema_id($schema_id)
                $(.with_isl_source_location($loc))?)?
                .build()
        ))
    };
}
pub(crate) use invalid_schema;

/// A macro that checks some condition required to be valid ISL.
///
/// If invalid, returns an InvalidSchemaErr with the given error message.
macro_rules! isl_require {
    ($expression:expr => $fmt_string:literal $($tt:tt)*) => {
        if ($expression) {
            Ok(())
        } else {
            Err($crate::result::IonSchemaError::from(crate::result::InvalidSchemaError::builder(format!($fmt_string $($tt)*)).build()))
        }
    };
}
pub(crate) use isl_require;
