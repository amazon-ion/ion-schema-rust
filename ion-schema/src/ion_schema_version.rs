// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

mod private {
    /// Used to prevent a public trait from being implemented outside of this crate.
    /// See https://predr.ag/blog/definitive-guide-to-sealed-traits-in-rust/
    pub trait IslVersion {}
}

/// Trait for ISL version marker types.
pub trait IslVersion: private::IslVersion {
    const MAJOR_MINOR: (u8, u8);
}

/// Ion Schema Language 1.0
#[allow(non_camel_case_types)]
pub enum ISL_1_0 {}

impl private::IslVersion for ISL_1_0 {}

impl IslVersion for ISL_1_0 {
    const MAJOR_MINOR: (u8, u8) = (1, 0);
}

/// Ion Schema Language 2.0
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
