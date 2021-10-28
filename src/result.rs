use ion_rs::result::IonError;
use std::io;
use thiserror::Error;

/// A unified Result type representing the outcome of method calls that may fail.
pub type IonSchemaResult<T> = Result<T, IonSchemaError>;

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
