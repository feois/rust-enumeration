//! This module provide helper traits and macro for bit related enumeration
//! 
//! # [`BitOperational`]
//! 
//! This trait is the core trait of the module, representing all types that can be potentially used for [`crate::bit_enum`].
//! This trait is a supertrait that is automatically implemented for types that also implements all the required traits.
//! 
//! # [`BitEnumeration`]
//! 
//! This trait is automatically implemented to [`Enumeration`] that satisfies some trait bounds. All [`Enumeration`] generated with [`crate::bit_enum`] is guaranteed to implement this trait.
//! 
//! # [`Bits`]
//! 
//! This struct is the core struct of the module, a wrapper struct that is meant to be used for bit-masking related operations.
//! 
//! ## Wrapped type
//! 
//! A generic type must be provided. While the struct does not have any trait bounds, most features are only available when the wrapped type implements [`BitOperational`] similarly to [`std::collections::HashMap`].
//! 
//! ## Binary Operations
//! 
//! All operations are defined for all types that implements [`Borrow<T>`], including but not limited to [`Bits<T>`], `&Bits<T>`, `T`, `&T`, [`BitEnumeration<T>`], `&BitEnumeration<T>`.
//! 
//! - `lhs + rhs` = `lhs & rhs`, that is, turns on bits in lhs that is turned on in rhs, or in other words, merge two bit masks
//! - `lhs - rhs` = `lhs & -rhs`, that is, turns off bits in lhs that is turned on in rhs, or in other words, remove all lhs' flags that are present in rhs
//! - `&` ([`BitAnd`]), `|` ([`BitOr`]) are defined to be same as the bit operations
//! - `^` ([`BitXor`]) is implemented if `T` implements [`BitXor`]
//! - `!` ([Not]) and `-` ([Neg]) are both defined to be bit flipping
//! - `+=` ([`AddAssign`]), `-=` ([`SubAssign`]), `&=` ([`BitAndAssign`]), `|=` ([`BitOrAssign`]) and `^=` ([`BitXorAssign`]) are all implemented automatically
//! 
//! ## Examples
//! 
//! ```
//! # use enumeration::{prelude::*, bitmask::*};
//! bit_enum! {
//!     BitMask(u16; u8)
//!         Foo
//!         Bar
//!         Baz
//! }
//! 
//! let foo_bit: Bits<u8> = BitMask::Foo.bit();
//! 
//! assert_eq!(foo_bit, Bits(1 << 0));
//! assert_eq!(BitMask::Bar.bit(), Bits(1 << 1));
//! assert_eq!(BitMask::Baz.bit(), Bits(1 << 2));
//! 
//! let foobar_bit: Bits<u8> = foo_bit + (1 << 1); // Bits<T> + T
//! 
//! assert_eq!(foobar_bit, Bits((1 << 0) + (1 << 1)));
//! 
//! let foobar_bit2: Bits<u8> = foo_bit + BitMask::Bar; // Bits<T> + BitEnumeration<T>
//! let foobar_bit3: Bits<u8> = foo_bit + BitMask::Bar.bit(); // Bits<T> + Bits<T>
//! let foobar_bit4: Bits<u8> = BitMask::Foo + BitMask::Bar; // BitEnumeration<T> + BitEnumeration<T>
//! let not_bit: u8 = (1 << 0) + (1 << 1); // Cautious! The left expression is not Bits<T> and adding here does not equal to Bits<T> adding
//! 
//! assert_eq!(foobar_bit, foobar_bit2);
//! assert_eq!(foobar_bit, foobar_bit3);
//! 
//! let foobar_bit5: Bits<u8> = foo_bit + &(1 << 1); // Bits<T> + &T
//! let foobar_bit6: Bits<u8> = foo_bit + &BitMask::Bar; // Bits<T> + &BitEnumeration<T>
//! let foobar_bit7: Bits<u8> = foo_bit + &BitMask::Bar.bit(); // Bits<T> + &Bits<T>
//!
//! let foobar_bit8: Bits<u8> = foobar_bit + BitMask::Foo;
//! 
//! assert_eq!(foobar_bit, foobar_bit8); // adding bits that are turned on will not do anything
//! 
//! let foo_bit2 = foobar_bit - BitMask::Bar;
//! 
//! assert_eq!(foo_bit, foo_bit2);
//! 
//! let foo_bit3 = foo_bit - BitMask::Bar;
//! 
//! assert_eq!(foo_bit, foo_bit3); // subtracting bits that are turned off will not do anything
//! assert_eq!(foo_bit, foobar_bit - (BitMask::Bar + BitMask::Baz));
//! ```
//! 
//! ## Iterator functions
//! 
//! [`Iterator::sum`] and [`Iterator::collect`] are both available for types that implement [`Borrow<T>`]
//! 
//! ```
//! # use enumeration::{prelude::*, bitmask::*};
//! bit_enum! {
//!     BitMask(u16; u8)
//!         Foo
//!         Bar
//!         Baz
//! }
//! 
//! let bits = BitMask::Foo + BitMask::Bar + BitMask::Baz;
//! 
//! assert_eq!(bits, BitMask::try_iter().sum::<Bits<u8>>());
//! assert_eq!(bits, BitMask::try_iter().collect::<Bits<u8>>());
//! assert_eq!(bits, [1 << 0, 1 << 1, 1 << 2].iter().sum::<Bits<u8>>());
//! assert_eq!(bits, [1 << 0, 1 << 1, 1 << 2].into_iter().sum::<Bits<u8>>());
//! assert_eq!(bits, [1 << 0, 1 << 1, 1 << 2].iter().collect::<Bits<u8>>());
//! assert_eq!(bits, [1 << 0, 1 << 1, 1 << 2].into_iter().collect::<Bits<u8>>());
//! ```
//! 
//! ## Check if this [`Bits<T>`] contains all bits in another [`Bits<T>`]
//! 
//! ```
//! # use enumeration::{prelude::*, bitmask::*};
//! bit_enum! {
//!     BitMask(u16; u8)
//!         Foo
//!         Bar
//!         Baz
//! }
//! 
//! let foobar = BitMask::Foo + BitMask::Bar;
//! 
//! assert!(foobar.has(BitMask::Foo));
//! assert!(foobar.has(&BitMask::Foo));
//! assert!(foobar.has(BitMask::Foo.bit()));
//! assert!(foobar.has(&BitMask::Foo.bit()));
//! assert!(foobar.has(1 << 0));
//! assert!(foobar.has(&(1 << 0)));
//! 
//! assert!(foobar.has(BitMask::Foo + BitMask::Bar));
//! assert!(foobar.has(BitMask::Bar));
//! assert!(!foobar.has(BitMask::Bar + BitMask::Baz));
//! 
//! assert!(foobar.has_all([BitMask::Foo, BitMask::Bar]));
//! assert!(foobar.has_all(&[BitMask::Foo, BitMask::Bar]));
//! assert!(foobar.has_all([BitMask::Foo, BitMask::Bar].iter()));
//! assert!(foobar.has_all([BitMask::Foo, BitMask::Bar].into_iter()));
//! assert!(!foobar.has_all([BitMask::Bar, BitMask::Baz]));
//! assert!(!foobar.has_all(BitMask::try_iter()));
//! ```

use std::{borrow::Borrow, iter::Sum, ops::*};

pub use crate::bit_enum;
use crate::prelude::Enumeration;

/// This trait is a marker trait automatically implemented for types that can be used as a bit mask.
/// See the [module documentation](`crate::bitmask`) for more information.
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
#[repr(transparent)]
/// A wrapper for the [`BitOperational`] type that redefines [`Add`] and [`Sub`]
///
/// # Some examples
///
/// ```
/// # use enumeration::{prelude::*, bitmask::*};
///
/// bit_enum!(Bitmask(u8; u8)
///     Red
///     Green
///     Blue
///     Alpha
/// );
///
/// assert_eq!(Bitmask::Red.bit(), Bits(1 << 0));
/// assert_eq!(Bitmask::Green.bit(), Bits(1 << 1));
/// assert_eq!(Bitmask::Blue.bit(), Bits(1 << 2));
/// assert_eq!(Bitmask::Alpha.bit(), Bits(1 << 3));
///
/// assert_eq!(Bitmask::Red + Bitmask::Blue, Bits((1 << 0) + (1 << 2)));
/// assert_eq!(Bitmask::all_bits() - Bitmask::Red, Bitmask::Green + Bitmask::Blue + Bitmask::Alpha);
/// assert_eq!(Bitmask::zero_bit() - Bitmask::Red, Bitmask::zero_bit());
///
/// // you can also do this
/// let _ = Bitmask::Red + (1 << 1);
/// // or this
/// let _ = Bits(0) + (1 << 0);
/// // or
/// let _ = Bits::zero() + (1 << 0);
/// ```
/// 
/// See the [module documentation](`crate::bitmask`) for more information.
pub struct Bits<T>(pub T);

impl<T: BitOperational> Bits<T> {
    #[inline(always)]
    /// Check if this [`Bits`] has all bits
    /// or in other words, `self + bits == self`
    pub fn has(self, bits: impl Borrow<T>) -> bool {
        &(self & bits.borrow()).0 == bits.borrow()
    }

    #[inline(always)]
    /// Check if this [`Bits`] has all bits of every item combined
    /// See also [`Bits::has`]
    pub fn has_all<U: Borrow<T>, I: IntoIterator<Item = U>>(self, bits: I) -> bool {
        self.has(bits.into_iter().sum::<Self>())
    }

    #[inline(always)]
    /// Returns [`Bits`] with no bit turned on
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
        self | rhs
    }
}

impl<T: BitOperational, U: Borrow<T>> AddAssign<U> for Bits<T> {
    #[inline(always)]
    fn add_assign(&mut self, rhs: U) {
        *self = self.clone() + rhs;
    }
}

impl<T: BitOperational, U: Borrow<T>> Sub<U> for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn sub(self, rhs: U) -> Self {
        self & -Bits(rhs.borrow().clone())
    }
}

impl<T: BitOperational, U: Borrow<T>> SubAssign<U> for Bits<T> {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: U) {
        *self = self.clone() - rhs;
    }
}

impl<T: BitOperational> Not for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn not(self) -> Self::Output {
        Bits(!self.0)
    }
}

impl<T: BitOperational> Neg for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn neg(self) -> Self::Output {
        !self
    }
}

impl<T: BitOperational, U: Borrow<T>> BitAnd<U> for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn bitand(self, rhs: U) -> Self::Output {
        Bits(self.0 & rhs.borrow())
    }
}

impl<T: BitOperational, U: Borrow<T>> BitAndAssign<U> for Bits<T> {
    #[inline(always)]
    fn bitand_assign(&mut self, rhs: U) {
        *self = self.clone() & rhs;
    }
}

impl<T: BitOperational, U: Borrow<T>> BitOr<U> for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn bitor(self, rhs: U) -> Self::Output {
        Bits(self.0 | rhs.borrow())
    }
}

impl<T: BitOperational, U: Borrow<T>> BitOrAssign<U> for Bits<T> {
    #[inline(always)]
    fn bitor_assign(&mut self, rhs: U) {
        *self = self.clone() | rhs;
    }
}

impl<T: BitOperational + for<'a> BitXor<&'a T, Output = T>, U: Borrow<T>> BitXor<U> for Bits<T> {
    type Output = Self;

    #[inline(always)]
    fn bitxor(self, rhs: U) -> Self::Output {
        Bits(self.0 ^ rhs.borrow())
    }
}

impl<T: BitOperational + for<'a> BitXor<&'a T, Output = T>, U: Borrow<T>> BitXorAssign<U>
    for Bits<T>
{
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: U) {
        *self = self.clone() ^ rhs;
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

/// Helper trait for [`Enumeration`] created with [`crate::bit_enum`].
/// See the [module documentation](`crate::bitmask`) for more information.
pub trait BitEnumeration<T: BitOperational + Copy>:
    Enumeration<AssociatedValueType = Bits<T>> + Borrow<T>
{
    #[inline(always)]
    /// returns [`Bits`]
    fn bit(&self) -> Bits<T> {
        self.value_copy()
    }

    /// returns [`Bits::zero`]
    #[inline(always)]
    fn zero_bit() -> Bits<T> {
        Bits::zero()
    }

    /// returns [`Bits`] with every bit turned on
    #[inline(always)]
    fn all_bits() -> Bits<T>
    where
        Self::Index: Into<usize> + TryFrom<usize>,
    {
        Self::try_iter().sum()
    }
}

impl<T: BitOperational + Copy, E: Enumeration<AssociatedValueType = Bits<T>> + Borrow<T>> BitEnumeration<T> for E {}

#[macro_export]
/// A specialized version of [`crate::enumerate`], it requires a non-optional associated value type (currently only integers are supported) that are automatically defined for each variant
///
/// # Syntax
/// 
/// The syntax is similar to [`crate::enumerate`].
/// 
/// `<code>` means optional
/// 
/// ```ignore
/// bit_enum! {
///     <#[attribute] ... #[attribute]>
///     <pub> <enum> EnumName(EnumType; BitMaskType)
///         <#[attribute] ... #[attribute]>
///         Variant1
///         <#[attribute] ... #[attribute]>
///         Variant2
///         ...
///         <#[attribute] ... #[attribute]>
///         VariantN
/// }
/// ```
/// 
/// Arguments (in order)
/// - attributes of the enum (optional)
/// - a visibility qualifier (optional)
/// - a name for the enum
/// - an integer type as the underlying type
/// - an integer type as the type used for bit operations (currently only integers are supported)
/// - at least a variant
///     - attributes (before variant) (optional)
///
/// # Variant count
/// 
/// The number of variants shall not exceed the bit width of the bit type
/// 
/// ```compile_fail
/// # use enumeration::{prelude::*, bitmask::*};
/// bit_enum! {
///     Foo(u16; u8)
///         Bar1
///         Bar2
///         Bar3
///         Bar4
///         Bar5
///         Bar6
///         Bar7
///         Bar8
///         Bar9
/// }
/// ```
/// 
/// # Features
/// 
/// See the [module documentation](`crate::bitmask`)
macro_rules! bit_enum {
    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident; $associated_value_type:ty) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::bit_enum!($(#[$enum_attr])* $visibility $name ($t; $associated_value_type) $($(#[$attr])* $variant)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident; $associated_value_type:ty) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; $crate::bitmask::Bits<$associated_value_type>) $($(#[$attr])* $variant = $crate::bitmask::Bits(1u8 as $associated_value_type << unsafe { ::std::mem::transmute::<$name, $t>($name::$variant) }))*);
        
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

        impl<T: ::std::borrow::Borrow<$associated_value_type>> ::std::ops::Add<T> for $name {
            type Output = $crate::bitmask::Bits<$associated_value_type>;

            #[inline(always)]
            fn add(self, o: T) -> Self::Output {
                use $crate::enumeration::Enumeration;
                self.value_clone() + o
            }
        }
    };
}
