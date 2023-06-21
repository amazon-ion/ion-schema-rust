use ion_rs::element::{Element, Value};
use ion_rs::external::bigdecimal::BigDecimal;
use ion_rs::{Decimal, Int};
use num_traits::ToPrimitive;
use std::str::FromStr;

/// Trait for adding extensions to [`Element`] that are useful for implementing Ion Schema.
pub(crate) trait ElementExtensions {
    /// Returns the value as an `usize` if it is an Ion Int that can be represented as such.
    fn as_usize(&self) -> Option<usize>;
    /// Returns the value as an `u64` if it is an Ion Int that can be represented as such.
    fn as_u64(&self) -> Option<u64>;
    /// Returns the value as a [`Decimal`] if it is any numeric Ion value that can be represented as a [`Decimal`].
    fn any_number_as_decimal(&self) -> Option<Decimal>;
}
impl ElementExtensions for Element {
    fn as_usize(&self) -> Option<usize> {
        match self.value() {
            Value::Int(Int::I64(i)) => i.to_usize(),
            Value::Int(Int::BigInt(i)) => i.to_usize(),
            _ => None,
        }
    }
    fn as_u64(&self) -> Option<u64> {
        match self.value() {
            Value::Int(Int::I64(i)) => i.to_u64(),
            Value::Int(Int::BigInt(i)) => i.to_u64(),
            _ => None,
        }
    }
    fn any_number_as_decimal(&self) -> Option<Decimal> {
        match self.value() {
            Value::Int(Int::I64(i)) => Some(Decimal::from(*i)),
            Value::Int(Int::BigInt(i)) => Some(Decimal::from(BigDecimal::from(i.clone()))),
            Value::Float(f) => BigDecimal::from_str(&format!(
                "{f:.PRECISION$e}",
                PRECISION = f64::MANTISSA_DIGITS as usize
            ))
            .ok()
            .map(|it| it.into()),
            Value::Decimal(d) => Some(d.clone()),
            _ => None,
        }
    }
}
