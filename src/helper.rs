//! This modules provides utilities to the crate

use crate::prelude::*;


/// Iterator helper
pub trait IteratorHelper where Self: Sized {
    #[inline(always)]
    /// Convert index into variant
    fn variants<E: Enumeration>(self) -> impl Iterator<Item = Result<E, E::Error>> where Self: IntoIterator<Item = E::Index> {
        self.into_iter().map(E::variant)
    }

    #[inline(always)]
    /// Convert index into variant, panic upon error
    fn variants_unwrap<E: Enumeration>(self) -> impl Iterator<Item = E> where Self: IntoIterator<Item = E::Index> {
        self.variants().map(Result::unwrap)
    }

    #[inline(always)]
    /// Convert index into variant without checking
    unsafe fn variants_unchecked<E: Enumeration>(self) -> impl Iterator<Item = E> where Self: IntoIterator<Item = E::Index> {
        self.into_iter().map(|i| unsafe { E::variant_unchecked(i) })
    }

    #[inline(always)]
    /// Convert index into variant without checking
    fn values<E: Enumeration>(self) -> impl Iterator<Item = (E, &'static E::AssociatedValueType)> where Self: IntoIterator<Item = E> {
        self.into_iter().map(|e| (e, e.value()))
    }

    #[inline(always)]
    /// Convert index into variant without checking
    fn index_values<E: Enumeration>(self) -> impl Iterator<Item = (E::Index, &'static E::AssociatedValueType)> where Self: IntoIterator<Item = E> {
        self.into_iter().map(|e| (e.to_index(), e.value()))
    }

    #[inline(always)]
    /// Convert index into variant without checking
    fn usize_values<E: Enumeration>(self) -> impl Iterator<Item = (usize, &'static E::AssociatedValueType)> where Self: IntoIterator<Item = E>, E::Index: Into<usize> {
        self.into_iter().map(|e| (e.to_index().into(), e.value()))
    }
}

impl<T: Iterator> IteratorHelper for T {}

