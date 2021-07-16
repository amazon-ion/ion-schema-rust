use thiserror::Error;
use std::io;
use ion_rs::result::IonError;

pub type IonSchemaResult<T> = Result<T, IonSchemaError>;

#[derive(Debug, Error)]
pub enum IonSchemaError {
    /// Indicates that an io error occurred while loading a schema
    #[error("{source:?}")]
    IoError {
        #[from]
        source: io::Error,
    },

    #[error("{description}")]
    UnresolvableSchemaError { description: String },

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

/// A convenience method for creating an  IonSchemaError::UnresolvableSchemaError with the provided operation
/// text.
pub fn unresolvable_schema_error_raw<S: AsRef<str>>(description: S) -> IonSchemaError {
    IonSchemaError::UnresolvableSchemaError {
        description: description.as_ref().to_string(),
    }
}