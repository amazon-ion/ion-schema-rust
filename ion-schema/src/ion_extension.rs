// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::result::{invalid_schema, IonSchemaResult};
use ion_rs::{Decimal, IonResult, Struct, Symbol};
use ion_rs::{Element, Value};
use num_traits::ToPrimitive;

/// Trait for adding extensions to [`Element`] that are useful for implementing Ion Schema.
pub(crate) trait ElementExtensions {
    /// Returns some `usize` if this `Element` is an Ion Int _and_ it can be represented as (fits in) a `usize`.
    /// Returns `None` if `self` is not an Ion Int, or self is null.int, or self is out of bounds for `usize`.
    fn as_usize(&self) -> Option<usize>;
    /// Returns some `u64` if this `Element` is an Ion Int _and_ it can be represented as (fits in) a `u64`.
    /// Returns `None` if `self` is not an Ion Int, or self is null.int, or self is out of bounds for `u64`.
    fn as_u64(&self) -> Option<u64>;
    /// Returns some [`Decimal`] if this `Element` is any Ion number type (`int`, `decimal`, or `float`)
    /// _and_ it can be represented as (fits in) a `Decimal`. Returns `None` if `self` is not one
    /// of the Ion number types or not a finite value.
    fn any_number_as_decimal(&self) -> Option<Decimal>;
    /// Get up to one annotation from this [`Element`]. If this [`Element`] has more than one
    /// annotation, or if the only annotation has unknown text, returns [`Err`].
    fn one_optional_annotation(&self) -> IonSchemaResult<Option<&str>>;
    /// If this [`Element`] is an Ion symbol with known text, returns [`Some`] with that text.
    /// Otherwise, returns [`None`].
    fn as_symbol_text(&self) -> Option<&str>;
}
impl ElementExtensions for Element {
    fn as_usize(&self) -> Option<usize> {
        match self.value() {
            Value::Int(i) => i.as_usize(),
            _ => None,
        }
    }
    fn as_u64(&self) -> Option<u64> {
        match self.value() {
            Value::Int(i) => i.as_i128()?.to_u64(),
            _ => None,
        }
    }
    fn any_number_as_decimal(&self) -> Option<Decimal> {
        match self.value() {
            Value::Int(i) => Some((*i).into()),
            Value::Float(f) => (*f).try_into().ok(),
            Value::Decimal(d) => Some(*d),
            _ => None,
        }
    }
    fn one_optional_annotation(&self) -> IonSchemaResult<Option<&str>> {
        let mut annotations = self.annotations().iter();
        let Some(symbol) = annotations.next() else {
            return Ok(None);
        };
        let text = symbol.expect_text()?;
        match annotations.next() {
            None => Ok(Some(text)),
            Some(_) => invalid_schema!("Unexpected annotations: {self}"),
        }
    }

    fn as_symbol_text(&self) -> Option<&str> {
        self.as_symbol().and_then(|s| s.text())
    }
}

/// Trait for adding extensions to [`Symbol`] that are useful for implementing Ion Schema.
pub(crate) trait SymbolExtensions: Sized {
    /// Returns Err if the symbol has unknown text, otherwise returns Ok(self).
    fn expect_known_symbol(self) -> IonResult<Self>;
}
impl SymbolExtensions for Symbol {
    fn expect_known_symbol(self) -> IonResult<Self> {
        let result = self.expect_text();
        match result {
            Ok(_) => Ok(self),
            Err(e) => Err(e),
        }
    }
}

/// Trait for adding extensions to [`Struct`] that are useful for implementing Ion Schema.
pub(crate) trait StructExtensions {
    /// Gets the value for the given field name, returning Err if there is more than one value with
    /// that field name.
    fn get_optional(&self, field_name: &str) -> IonSchemaResult<Option<&Element>>;
    /// Gets the value for the given field name, returning Err if there are no values with that
    /// field name or if there is more than one value with that field name.
    fn get_required(&self, field_name: &str) -> IonSchemaResult<&Element>;
}
impl StructExtensions for Struct {
    fn get_optional(&self, field_name: &str) -> IonSchemaResult<Option<&Element>> {
        let mut iter = self.get_all(field_name);
        let first = iter.next();
        let second = iter.next();
        if second.is_some() {
            invalid_schema!("Illegal repeated field '{field_name}' in: {self}")
        } else {
            Ok(first)
        }
    }

    fn get_required(&self, field_name: &str) -> IonSchemaResult<&Element> {
        if let Some(element) = self.get_optional(field_name)? {
            Ok(element)
        } else {
            invalid_schema!("Missing required field '{field_name}' in: {self}")
        }
    }
}
