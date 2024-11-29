//! This module provide helper traits and macro for bit related enumeration

use std::{borrow::Borrow, iter::Sum, ops::*};

pub use crate::bitenum;
use crate::prelude::Enumeration;

pub trait BitOperational:
    Clone
    + Eq
    + for<'a> BitAnd<&'a Self, Output = Self>
    + for<'a> BitOr<&'a Self, Output = Self>
    + Not<Output = Self>
    + Default
{
}

impl<
        T: Clone
            + Eq
            + for<'a> BitAnd<&'a Self, Output = Self>
            + for<'a> BitOr<&'a Self, Output = Self>
            + Not<Output = Self>
            + Default,
    > BitOperational for T
{
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Bits<T>(pub T);

impl<T: BitOperational> Bits<T> {
    #[inline(always)]
    pub fn has(self, t: impl Borrow<T>) -> bool {
        &(self.0 & (&t.borrow())) == t.borrow()
    }

    #[inline(always)]
    pub fn has_all<U: Borrow<T>, I: IntoIterator<Item = U>>(self, i: I) -> bool {
        self.has(i.into_iter().sum::<Self>())
    }

    #[inline(always)]
    pub fn zero() -> Self {
        Default::default()
    }
}

impl<T: BitOperational> Borrow<T> for Bits<T> {
    #[inline(always)]
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<'a, T: BitOperational> Borrow<T> for &'a Bits<T> {
    #[inline(always)]
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: BitOperational, U: Borrow<T>> Add<U> for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn add(self, rhs: U) -> Self {
        Bits(self.0 | rhs.borrow())
    }
}

impl<T: BitOperational, U: Borrow<T>> Sub<U> for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn sub(self, rhs: U) -> Self {
        Bits(self.0 & (&!rhs.borrow().clone()))
    }
}

impl<T: BitOperational, U: Borrow<T>> Sum<U> for Bits<T> {
    #[inline(always)]
    fn sum<I: Iterator<Item = U>>(iter: I) -> Self {
        Bits(iter.fold(T::default(), |t, u| t | u.borrow()))
    }
}

impl<T: BitOperational, U: Borrow<T>> FromIterator<U> for Bits<T> {
    #[inline(always)]
    fn from_iter<I: IntoIterator<Item = U>>(iter: I) -> Self {
        iter.into_iter().sum()
    }
}

pub trait BitEnumeration<T: BitOperational + Copy>: Enumeration<AssociatedValueType = Bits<T>> + Borrow<T> {
    #[inline(always)]
    fn bit(&self) -> Bits<T> {
        self.value_copy()
    }

    #[inline(always)]
    fn zero_bit() -> Bits<T> {
        Bits::zero()
    }

    #[inline(always)]
    fn all_bits() -> Bits<T> where Self::Index: Into<usize> + TryFrom<usize> {
        Self::try_iter().sum()
    }
}

#[macro_export]
macro_rules! bitenum {
    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident; $associated_value_type:ty) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::bitenum!($(#[$enum_attr])* $visibility $name ($t; $associated_value_type) $($(#[$attr])* $variant)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident; $associated_value_type:ty) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; $crate::bitmask::Bits<$associated_value_type>) $($(#[$attr])* $variant = $crate::bitmask::Bits(1u8 as $associated_value_type << unsafe { ::std::mem::transmute::<$name, $t>($name::$variant) }))*);

        impl $crate::bitmask::BitEnumeration<$associated_value_type> for $name {}

        impl ::std::borrow::Borrow<$associated_value_type> for $name {
            #[inline(always)]
            fn borrow(&self) -> &$associated_value_type {
                use $crate::enumeration::Enumeration;
                &self.value().0
            }
        }

        impl<'a> ::std::borrow::Borrow<$associated_value_type> for &'a $name {
            #[inline(always)]
            fn borrow(&self) -> &$associated_value_type {
                use $crate::enumeration::Enumeration;
                &self.value().0
            }
        }

        impl ::std::ops::Add for $name {
            type Output = $crate::bitmask::Bits<$associated_value_type>;

            #[inline(always)]
            fn add(self, o: Self) -> Self::Output {
                use $crate::enumeration::Enumeration;
                self.value_clone() + o.value()
            }
        }

        impl ::std::ops::Add<$crate::bitmask::Bits<$associated_value_type>> for $name {
            type Output = $crate::bitmask::Bits<$associated_value_type>;

            #[inline(always)]
            fn add(self, o: Self::Output) -> Self::Output {
                use $crate::enumeration::Enumeration;
                o + self.value()
            }
        }
    };
}
