use thiserror::Error;
use std::io;

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
        source: ion_rs::result::IonError,
    },
}

impl Clone for IonSchemaError {
    fn clone(&self) -> Self {
        use IonSchemaError::*;
        match self {
            IoError { source } => IoError {
                // io::Error implements From<ErrorKind>, and ErrorKind is cloneable.
                source: io::Error::from(source.kind().clone()),
            },
            UnresolvableSchemaError { description } => UnresolvableSchemaError {
                description: description.clone(),
            },
            IonError { source } => IonError {
                source: source.clone(),
            },
        }
    }
}

pub fn unresolvable_schema_error<T, S: AsRef<str>>(description: S) -> IonSchemaResult<T> {
    Err(IonSchemaError::UnresolvableSchemaError {
        description: description.as_ref().to_string(),
    })
}
