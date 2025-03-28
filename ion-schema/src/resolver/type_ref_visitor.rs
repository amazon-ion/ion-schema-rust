// Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
// SPDX-License-Identifier: Apache-2.0

use crate::model::Bag;
use crate::model::TypeReference;
use std::collections::HashMap;

/// A visitor for [`TypeReference`]s in a [`SchemaDocument`], used for resolving schema references.
pub(crate) trait TypeRefVisitor {
    /// Visits a single [`TypeReference`].
    fn visit(&mut self, type_ref: &TypeReference);
}

impl<F: FnMut(&TypeReference)> TypeRefVisitor for F {
    fn visit(&mut self, type_ref: &TypeReference) {
        self(type_ref)
    }
}

/// A trait indicating that a type can walk a [`TypeRefVisitor`], possibly to [`TypeReferences`].
pub(crate) trait TypeRefWalker {
    fn walk<V: TypeRefVisitor>(&self, visitor: &mut V);
}

/// A macro that implements [`TypeRefWalker`] for container types that have an `iter()` method.
macro_rules! has_n_type_refs {
    ($t:ident) => {
        impl<T: TypeRefWalker> TypeRefWalker for $t<T> {
            fn walk<V: TypeRefVisitor>(&self, visitor: &mut V) {
                self.iter().for_each(|item| item.walk(visitor))
            }
        }
    };
}
has_n_type_refs!(Vec);
has_n_type_refs!(Bag);
has_n_type_refs!(Option);

impl<T: TypeRefWalker> TypeRefWalker for &T {
    fn walk<V: TypeRefVisitor>(&self, visitor: &mut V) {
        <T as TypeRefWalker>::walk(self, visitor)
    }
}

impl<K, T: TypeRefWalker> TypeRefWalker for HashMap<K, T> {
    fn walk<V: TypeRefVisitor>(&self, visitor: &mut V) {
        self.iter().for_each(|(_, item)| item.walk(visitor))
    }
}

/// A macro that implements [`TypeRefWalker`]. The first argument is the type for which to implement
/// the trait. The second argument is an expression (relative to `self`) that accesses a single
/// field that _indirectly_ contains type references.
macro_rules! impl_type_ref_walker {
    ($t:ty) => {
        impl crate::resolver::TypeRefWalker for $t {
            fn walk<V: crate::resolver::TypeRefVisitor>(&self, visitor: &mut V) {
            }
        }
    };
    ($t:ty, $($accessor:tt)+) => {
        impl crate::resolver::TypeRefWalker for $t {
            fn walk<V: crate::resolver::TypeRefVisitor>(&self, visitor: &mut V) {
                self.$($accessor)+.walk(visitor);
            }
        }
    };
}
pub(crate) use impl_type_ref_walker;
