use crate::result::{HasIslSourceLocation, IonSchemaError, IonSchemaResult, IslSourceLocation};
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter, Write};
use thiserror::Error;

/// Collects multiple errors to report them together.
///
/// ```ignore
/// # use ion_schema::IonSchemaResult;
/// #
/// use ion_schema::result::ErrorCollector;
///
/// fn error_prone_function(i: i8) -> IonSchemaResult<()> {
///     todo!()
/// }
/// fn some_fun() -> IonSchemaResult<()> {
///     // Create the ErrorCollector
///     let mut errors = ErrorCollector::default();
///     for i in 1..10 {
///         let Some(ok_value) = errors.ok_or_push_err(error_prone_function(i)) else {
///             continue;
///         }
///         // Do something with `ok_value`
///     }
///     // Transform the error collector into a result.
///     errors.into_result_with(())
/// }
/// ```
#[derive(Debug, Default)]
pub(crate) struct InvalidSchemaErrorCollector {
    errors: BTreeSet<InvalidSchemaDetails>,
}
impl InvalidSchemaErrorCollector {
    /// Consumes `self` and `ok_value`. If this `ErrorCollector` contains no errors, returns `Ok(ok_value)`.
    /// Otherwise, returns `Err`.
    ///
    /// Use this method to turn the `ErrorCollector` into a usable `IonSchemaResult`.
    pub(crate) fn into_result_with<T>(self, ok_value: T) -> Result<T, InvalidSchemaError> {
        if self.errors.is_empty() {
            Ok(ok_value)
        } else {
            Err(InvalidSchemaError(InvalidSchemaErrorQuantity::Many(
                self.errors,
            )))
        }
    }

    /// Push an `IonSchemaError` to this `ErrorCollector`.
    pub(crate) fn push_err(&mut self, e: impl Into<InvalidSchemaError>) {
        let e = e.into();
        match e.0 {
            InvalidSchemaErrorQuantity::Many(errors) => self.errors.extend(errors),
            InvalidSchemaErrorQuantity::One(error) => {
                self.errors.insert(error);
            }
        }
    }

    /// Converts from [`IonSchemaResult<T>`] to [`Option<T>`].
    ///
    /// Converts `result` into an [`Option<T>`], consuming `result`, and pushing the error, if any,
    /// to this `ErrorCollector`.
    /// This is the multi-error equivalent to [`Result::ok`].
    pub(crate) fn ok_or_push_err<T>(
        &mut self,
        result: impl Into<Result<T, InvalidSchemaError>>,
    ) -> Option<T> {
        match result.into() {
            Ok(value) => Some(value),
            Err(e) => {
                self.push_err(e);
                None
            }
        }
    }
}

#[derive(Debug, Error, PartialEq)]
#[error(transparent)]
pub struct InvalidSchemaError(InvalidSchemaErrorQuantity);

impl InvalidSchemaError {
    pub(crate) fn builder(description: String) -> InvalidSchemaDetailsBuilder {
        InvalidSchemaDetailsBuilder::new(description)
    }
}

pub struct InvalidSchemaDetailsBuilder {
    schema_id: String,
    row_column: IslSourceLocation,
    description: String,
}

impl InvalidSchemaDetailsBuilder {
    fn new(description: String) -> Self {
        InvalidSchemaDetailsBuilder {
            description,
            row_column: IslSourceLocation::default(),
            schema_id: "".to_string(),
        }
    }
    pub(crate) fn with_schema_id(mut self, id: impl Into<String>) -> Self {
        self.schema_id = id.into();
        self
    }
    pub(crate) fn with_isl_source_location<T: HasIslSourceLocation>(mut self, item: &T) -> Self {
        self.row_column = item.isl_source_location();
        self
    }
    pub(crate) fn build(self) -> InvalidSchemaError {
        InvalidSchemaError(InvalidSchemaErrorQuantity::One(InvalidSchemaDetails {
            schema_id: self.schema_id,
            row_column: self.row_column,
            description: self.description,
        }))
    }
}
impl From<InvalidSchemaDetailsBuilder> for InvalidSchemaError {
    fn from(value: InvalidSchemaDetailsBuilder) -> Self {
        value.build()
    }
}
impl<T> From<InvalidSchemaDetailsBuilder> for Result<T, InvalidSchemaError> {
    fn from(value: InvalidSchemaDetailsBuilder) -> Self {
        Err(value.build())
    }
}
impl From<InvalidSchemaDetailsBuilder> for IonSchemaError {
    fn from(value: InvalidSchemaDetailsBuilder) -> Self {
        IonSchemaError::InvalidSchemaError(value.build())
    }
}
impl<T> From<InvalidSchemaDetailsBuilder> for IonSchemaResult<T> {
    fn from(value: InvalidSchemaDetailsBuilder) -> Self {
        Err(IonSchemaError::InvalidSchemaError(value.build()))
    }
}

#[derive(Debug, Error, PartialEq)]
enum InvalidSchemaErrorQuantity {
    One(InvalidSchemaDetails),
    Many(BTreeSet<InvalidSchemaDetails>),
}
impl Display for InvalidSchemaErrorQuantity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidSchemaErrorQuantity::One(one) => Display::fmt(one, f),
            InvalidSchemaErrorQuantity::Many(many) => {
                for detail in many {
                    Display::fmt(detail, f)?;
                    f.write_char('\n')?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Error, PartialEq, PartialOrd, Eq, Ord)]
#[error("{schema_id}:{row_column} > {description}")]
struct InvalidSchemaDetails {
    schema_id: String,
    row_column: IslSourceLocation,
    description: String,
}
