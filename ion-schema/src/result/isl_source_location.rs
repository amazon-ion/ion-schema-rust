use std::fmt::{Display, Formatter};

/// Represents a location where an InvalidSchemaError occurred (or could potentially occur).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default, Eq, Ord)]
pub(crate) struct IslSourceLocation(usize, usize);
impl IslSourceLocation {
    pub fn new(from: Option<(usize, usize)>) -> Self {
        match from {
            None => IslSourceLocation(0, 0),
            Some(rc) => {
                let (r, c) = rc;
                IslSourceLocation(r, c)
            }
        }
    }
}
impl Display for IslSourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Self(r, c) = *self;
        if r != 0 || c != 0 {
            f.write_fmt(format_args!("{r}:{c}"))?;
        }
        Ok(())
    }
}

/// Should be implemented by any type that needs to provide a location to enable friendly error
/// reporting.
pub(crate) trait HasIslSourceLocation {
    fn isl_source_location(&self) -> IslSourceLocation;
}
impl HasIslSourceLocation for ion_rs::Element {
    fn isl_source_location(&self) -> IslSourceLocation {
        let location = self.location();
        IslSourceLocation(
            location.row().unwrap_or_default(),
            location.column().unwrap_or_default(),
        )
    }
}
