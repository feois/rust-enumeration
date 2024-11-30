//! This module provides runtime representation of enumeration

use std::any::{type_name, TypeId};
use std::fmt::{Debug, Display};
use std::marker::PhantomData;

use crate::prelude::*;

/// Error type when attempting to cast [Variant] to [Enumeration]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CastError<T: Enumeration>(PhantomData<T>);

impl<T: Enumeration> Display for CastError<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Unable to cast to enumeration type {}",
            type_name::<T>()
        )
    }
}

impl<T: Enumeration> std::error::Error for CastError<T> {}

/// Provides runtime specialized representation of [Enumeration].
///
/// To avoid using `dyn` for mixing [Enumeration], you can use [Variant] with a trade-off for having to try casting before using it.
///
/// # Examples
/// ```
/// # use enumeration::{prelude::*, variant::*};
/// enumerate!(Foo(u8)
///     Bar
///     Baz
/// );
///
/// enumerate!(Color(u8)
///     Red
///     Green
///     Blue
/// );
///
/// let mut vec: Vec<Variant<u8>> = vec![Foo::Bar.into(), Color::Green.into(), Foo::Baz.into()]; // Variant::new(Foo::Bar) or Variant::from(Foo::Bar) works too
///
/// assert_eq!(vec[0].cast::<Foo>(), Ok(Foo::Bar));
/// assert!(vec[1].cast::<Foo>().is_err());
/// ```
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Variant<T: Debug> {
    type_id: TypeId,
    index: T,
}

impl<T: Enumeration> From<T> for Variant<T::Index> {
    #[inline(always)]
    fn from(e: T) -> Self {
        Self {
            type_id: TypeId::of::<T>(),
            index: e.to_index(),
        }
    }
}

impl<T: Debug> Variant<T> {
    #[inline(always)]
    /// Construct [Variant] with [Enumeration]
    pub fn new<E: Enumeration>(e: E) -> Variant<E::Index> {
        Variant::from(e)
    }

    #[inline(always)]
    /// Returns the type id of the enumeration.
    pub fn type_id(self) -> TypeId {
        self.type_id
    }

    #[inline(always)]
    /// Returns the index of the enumeration.
    pub fn index(self) -> T {
        self.index
    }

    #[inline(always)]
    /// Try casting to the given generic parameter
    pub fn cast<E: Enumeration<Index = T>>(self) -> Result<E, CastError<E>> {
        if TypeId::of::<E>() == self.type_id {
            Ok(E::variant(self.index).unwrap())
        }
        else {
            Err(CastError(PhantomData))
        }
    }
}

/// Provides runtime specialized representation of [Enumeration] with specified [Enumeration::AssociatedValueType].
/// More details in [Variant].
///
/// It's basically [Variant] but with specified on what it's [Enumeration::AssociatedValueType] must be.
///
/// # Examples
/// ```compile_fail
/// # use enumerate::prelude::*;
/// enumerate!(Foo(u8; i32)
///     Bar = 111
///     Baz = 333
/// );
///
/// enumerate!(Color(u8; &'static str)
///     Red = "#FF0000"
///     Blue = "#0000FF"
///     Yellow = "#FFFF00"
///     Cyan = "#00FFFF"
/// );
///
/// let mut vec = vec![Color::Red.into(), Color::Blue.into()];
///
/// assert_eq!(vec[0].value(), "#FF0000");
///
/// vec.push(Foo::Bar.into()); // compile error
/// ```
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariantWith<T: Debug, U: 'static> {
    type_id: TypeId,
    index: T,
    value: &'static U,
}

impl<T: Enumeration> From<T> for VariantWith<T::Index, T::AssociatedValueType> {
    #[inline(always)]
    fn from(e: T) -> Self {
        Self {
            type_id: TypeId::of::<T>(),
            index: e.to_index(),
            value: e.value(),
        }
    }
}

impl<T: Debug, U: 'static> VariantWith<T, U> {
    #[inline(always)]
    /// Constructs [VariantWith] with [Enumeration]
    pub fn new<E: Enumeration>(e: E) -> VariantWith<E::Index, E::AssociatedValueType> {
        VariantWith::from(e)
    }

    #[inline(always)]
    /// Returns the type id.
    pub fn type_id(self) -> TypeId {
        self.type_id
    }

    #[inline(always)]
    /// Returns the index.
    pub fn index(self) -> T {
        self.index
    }

    #[inline(always)]
    /// Returns the associated constant value without casting.
    pub fn value(self) -> &'static U {
        self.value
    }

    #[inline(always)]
    /// Try casting to the given generic parameter
    pub fn cast<E: Enumeration<Index = T, AssociatedValueType = U>>(
        self,
    ) -> Result<E, CastError<E>> {
        if TypeId::of::<E>() == self.type_id {
            Ok(E::variant(self.index).unwrap())
        }
        else {
            Err(CastError(PhantomData))
        }
    }
}
