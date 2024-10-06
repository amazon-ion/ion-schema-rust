use ion_rs::Decimal;
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
}
impl ElementExtensions for Element {
    fn as_usize(&self) -> Option<usize> {
        match self.value() {
            Value::Int(i) => i.as_i128()?.to_usize(),
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
}
