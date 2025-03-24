// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod private {
    /// Used to prevent a public trait from being implemented outside of this crate.
    /// See https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/
    pub trait IslVersion {}
}

/// Trait for ISL version marker types.
pub trait IslVersion: private::IslVersion + Clone {
    const MAJOR_MINOR: (u8, u8);
}

/// Ion Schema Language 1.0
#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum ISL_1_0 {}

impl private::IslVersion for ISL_1_0 {}

impl IslVersion for ISL_1_0 {
    const MAJOR_MINOR: (u8, u8) = (1, 0);
}

/// Ion Schema Language 2.0
#[derive(Copy, Clone, Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum ISL_2_0 {}

impl private::IslVersion for ISL_2_0 {}

impl IslVersion for ISL_2_0 {
    const MAJOR_MINOR: (u8, u8) = (2, 0);
}

/*
When a new version is added, we can add pub(crate) traits to represent ranges of versions.

/// Internal-only marker trait to make it simple to implement a trait once
/// for things that are supported the same way in Isl 2.0 and 2.1.
pub(crate) trait SinceISL_2_0: IslVersion { }

impl SinceISL_2_0 for ISL_2_0 {}

 */

use std::marker::PhantomData;

/// Wrapper for data that needs to be passed around with an ISL version—for example, builder methods
/// for some constraints can accept `Versioned<TypeArgument>`s.
///
/// Technically, this is a smart pointer that (rather than managing the ownership or memory of the
/// wrapped value) encodes Ion Schema version information along with the wrapped value.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Versioned<T, V: IslVersion>(T, PhantomData<V>);
impl<T, V: IslVersion> Versioned<T, V> {
    /// Creates a new [`Versioned`] instance, wrapping the given value.
    pub(crate) fn new(value: T) -> Self {
        Self(value, PhantomData)
    }

    /// Maps the value contained by this [`Versioned`] instance.
    pub(crate) fn map<U, F: FnOnce(T) -> U>(
        this: Versioned<T, V>,
        transform: F,
    ) -> Versioned<U, V> {
        Versioned(transform(this.0), this.1)
    }

    /// Consumes this [`Versioned`] instance, return the value contained within.
    pub(crate) fn into_inner(self) -> T {
        self.0
    }
}

// For testing-convenience only, we make this wrapper non-opaque.
#[cfg(test)]
use std::ops::Deref;
#[cfg(test)]
impl<T, V: IslVersion> Deref for Versioned<T, V> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
