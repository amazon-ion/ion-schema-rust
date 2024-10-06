//! Represents the [`IonSchemaResult`] type for error handling.
//!
//! [`IonSchemaResult<T, E>`][`IonSchemaResult`]  is the type used for returning and propagating errors.
//! It is an enum with the variants, Ok(T), representing success and containing a value,and Err(E), representing an [`IonSchemaError`].

use crate::violation::Violation;
use ion_rs::IonError;
use std::io;
use thiserror::Error;

/// A unified Result type representing the outcome of method calls that may fail.
pub type IonSchemaResult<T> = Result<T, IonSchemaError>;

/// A Result type representing the outcome of validation that may result in violations.
pub type ValidationResult = Result<(), Violation>;

/// Represents the different types of high-level failures that might occur when reading Ion Schema.
#[derive(Debug, Error)]
pub enum IonSchemaError {
    /// Indicates that an io error occurred while loading a schema
    #[error("{source:?}")]
    IoError {
        #[from]
        source: io::Error,
    },

    /// Indicates failure for schema which has unresolvable imports/types
    #[error("{description}")]
    UnresolvableSchemaError { description: String },

    /// Indicates failure due to invalid schema syntax
    #[error("{description}")]
    InvalidSchemaError { description: String },

    /// Indicates failure due to ion-rust error defined by IonError
    #[error("{source:?}")]
    IonError {
        #[from]
        source: IonError,
    },
}

// io::Error does not implement PartialEq, which precludes us from simply deriving an implementation.
impl PartialEq for IonSchemaError {
    fn eq(&self, other: &Self) -> bool {
        use IonSchemaError::*;
        match (self, other) {
            // We can compare the io::Errors' ErrorKinds, offering a weak definition of equality.
            (IoError { source: s1 }, IoError { source: s2 }) => s1.kind() == s2.kind(),
            (
                UnresolvableSchemaError { description: s1 },
                UnresolvableSchemaError { description: s2 },
            ) => s1 == s2,
            (InvalidSchemaError { description: s1 }, InvalidSchemaError { description: s2 }) => {
                s1 == s2
            }
            (IonError { source: s1 }, IonError { source: s2 }) => s1 == s2,
            _ => false,
        }
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
pub fn invalid_schema_error_raw<S: AsRef<str>>(description: S) -> IonSchemaError {
    IonSchemaError::InvalidSchemaError {
        description: description.as_ref().to_string(),
    }
}

/// A convenience method for creating an IonSchemaResult containing an IonSchemaError::InvalidSchemaError
/// with the provided description text.
pub fn invalid_schema_error<T, S: AsRef<str>>(description: S) -> IonSchemaResult<T> {
    Err(IonSchemaError::InvalidSchemaError {
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

/// A macro that checks some condition required to be valid ISL.
///
/// If invalid, returns an InvalidSchemaErr with the given error message.
#[macro_export]
macro_rules! isl_require {
    ($expression:expr => $fmt_string:literal $(, $($tt:tt)*)?) => {
        if ($expression) {
            Ok(())
        } else {
            Err($crate::result::IonSchemaError::InvalidSchemaError {
                description: format!($fmt_string),
            })
        }
    };
}
