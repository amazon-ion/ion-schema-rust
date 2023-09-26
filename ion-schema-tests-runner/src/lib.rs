extern crate proc_macro;

use crate::generator::generate_ion_schema_tests;
use darling::FromMeta;
use proc_macro::TokenStream as _TokenStream;
use regex::{Regex, RegexSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use syn::parse_macro_input;
use syn::{AttributeArgs, Lit, NestedMeta};

mod generator;
mod model;

/// Thin wrapper around [PathBuf] so that we can implement the [FromMeta] trait and get better
/// compile-time error messages if the path is not valid.
#[derive(Debug)]
struct WrappedPathBuf(PathBuf);
impl FromMeta for WrappedPathBuf {
    fn from_string(value: &str) -> darling::Result<Self> {
        Path::new(value)
            .canonicalize()
            .map(WrappedPathBuf)
            .map_err(darling::Error::custom)
    }
}
impl From<WrappedPathBuf> for PathBuf {
    fn from(value: WrappedPathBuf) -> Self {
        value.0
    }
}

/// Wrapper around a vec of strings so that we can implement the [FromMeta] trait and get better
/// compile-time error messages if one of the strings is not a valid regex string.
#[derive(Debug, Clone, Default)]
struct FilterStrings(Vec<String>);
impl FromMeta for FilterStrings {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
        let mut the_vec: Vec<String> = vec![];
        let mut errors: Vec<darling::Error> = vec![];
        for item in items {
            match item {
                NestedMeta::Lit(Lit::Str(s)) => {
                    if let Err(e) = Regex::new(&s.value()) {
                        errors.push(darling::Error::custom(e).with_span(s));
                    } else {
                        the_vec.push(s.value());
                    }
                }
                _ => {
                    errors.push(darling::Error::unexpected_type("not a string").with_span(item));
                }
            }
        }
        if !errors.is_empty() {
            return Err(darling::Error::multiple(errors));
        }
        Ok(FilterStrings(the_vec))
    }
}

#[derive(Debug, FromMeta)]
struct MacroArgs {
    root: WrappedPathBuf,
    ignored: Option<FilterStrings>,
}

/// Macro for generating test cases from an Ion Schema Tests directory.
///
/// Example usage:
/// ```
/// use ion_schema_tests_runner::ion_schema_tests;
///
/// ion_schema_tests!(
///     // The root directory of a test suite.
///     root = "ion-schema-tests/ion_schema_1_0/",
///     // Optional, a list of tests to mark as `#[ignore]`.
///     // Strings can be test names or regexes that match test names.
///     ignored(
///         // Ignore all tests for `ordered_elements` constraint
///         "constraints::ordered_elements",
///         // Ignore all tests for invalid imports
///         "invalid.*import",
///     )
/// );
/// ```
#[proc_macro]
pub fn ion_schema_tests(item: _TokenStream) -> _TokenStream {
    let attr_args = parse_macro_input!(item as AttributeArgs);

    let args: MacroArgs = match MacroArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => {
            return e.write_errors().into();
        }
    };

    let root_dir = PathBuf::from(args.root);

    let regex_set = match args.ignored {
        None => RegexSet::empty(),
        Some(strings) => RegexSet::new(strings.0).unwrap(),
    };
    let exclude_filter = Rc::new(move |s: String| regex_set.is_match(&s));

    generate_ion_schema_tests(root_dir.as_path(), exclude_filter).into()
}
