//! This module provides the core trait `Enumeration`

use std::fmt::{Debug, Display};
use std::hash::Hash;


/// Error type when casting from [Enumeration::Index] to [Enumeration].
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OutOfRangeError<T: Enumeration>(pub T::Index);

impl<T: Enumeration> Display for OutOfRangeError<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Out of enumeration index range (0 to {}): {}",
            T::VARIANT_COUNT,
            self.0
        )
    }
}

impl<T: Enumeration + Debug> std::error::Error for OutOfRangeError<T> {}

/// Error type when converting from [Enumeration::AssociatedValueType] to [Enumeration].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnknownAssociatedValueError<'a, T: Enumeration>(pub &'a T::AssociatedValueType);

impl<'a, T: Enumeration> Display for UnknownAssociatedValueError<'a, T> where T::AssociatedValueType: Display {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Unknown associated value: {}",
            self.0,
        )
    }
}

impl<'a, T: Enumeration> std::error::Error for UnknownAssociatedValueError<'a, T> where T::AssociatedValueType: Display {}

/// A trait to extend enum.
///
/// You should not implement this directly, instead use [crate::enumerate].
/// The rest of the library assumes the constants are correct.
///
/// This trait provides support for
/// - implementation for common traits ([Clone], [Copy], [Hash], etc.)
/// - getting number of variants through constant [Self::VARIANT_COUNT]
/// - casting between index (of type [Self::Index]) and enumeration
/// - attaching a constant value to each of the variants
pub trait Enumeration:
    std::convert::TryFrom<Self::Index, Error = OutOfRangeError<Self>>
    + Into<Self::Index>
    + Clone
    + Copy
    + Debug
    + Hash
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + 'static
where
    Self::AssociatedValueType: 'static + Debug,
    Self::Index: Debug + Display,
{
    /// The type of index this enumeration can cast to.
    type Index;

    /// The type of associated constant value of the variants of this enumeration.
    type AssociatedValueType;

    /// The number of variants of this enumeration.
    const VARIANT_COUNT: Self::Index;

    #[inline(always)]
    /// Same as [Enumeration::VARIANT_COUNT]
    fn count() -> Self::Index { Self::VARIANT_COUNT }

    #[inline(always)]
    /// [Enumeration::VARIANT_COUNT] as usize
    fn len() -> usize where Self::Index: Into<usize> { Self::count().into() }

    /// Get the reference to the static associated constant value of the variant or the default constant value [DefaultAssociatedValue::DEFAULT_ASSOCIATED_VALUE].
    fn value(self) -> &'static Self::AssociatedValueType;

    #[inline(always)]
    /// Get the associated constant value by copying
    fn value_copy(self) -> Self::AssociatedValueType where Self::AssociatedValueType: Copy {
        *self.value()
    }

    #[inline(always)]
    /// Get the associated constant value by cloning
    fn value_clone(self) -> Self::AssociatedValueType where Self::AssociatedValueType: Clone {
        self.value().clone()
    }

    #[inline(always)]
    /// Cast index to the respective enumeration.
    ///
    /// # Errors
    ///
    /// If the index is out of range (zero to [Self::VARIANT_COUNT] (exclusive)), this function will return [OutOfRangeError]
    fn variant(index: Self::Index) -> Result<Self, Self::Error> {
        Self::try_from(index)
    }

    /// Cast index to the respective enumeration without checking
    unsafe fn variant_unchecked(index: Self::Index) -> Self;

    #[inline(always)]
    /// Cast this variant to respective index.
    fn to_index(self) -> Self::Index {
        self.into()
    }

    #[inline(always)]
    /// Cast this variant to usize
    fn to_usize(self) -> usize where Self::Index: Into<usize> {
        self.to_index().into()
    }

    #[inline(always)]
    /// Iterate through all variants
    fn iter() -> impl Iterator<Item = Self> where Self::Index: Into<usize> + From<usize> {
        (0..Self::len()).map(|i| unsafe { Self::variant_unchecked(i.into()) })
    }

    #[inline(always)]
    /// iter() but when `Index` only implements `TryFrom<usize>` as opposed to `From<usize>`
    fn try_iter() -> impl Iterator<Item = Self> where Self::Index: Into<usize> + TryFrom<usize> {
        (0..Self::len()).map(|i| unsafe { Self::variant_unchecked(i.try_into().unwrap_or_else(|_| unreachable!()) ) })
    }
}

/// Returns the first variant (in the declaration order) associated with the value
pub trait FromValue: Enumeration {
    /// Returns the first variant (in the declaration order) associated with the value
    /// Returns error when no variant is associated with the value
    /// 
    /// See also [Enumeration::from_value_unchecked]
    fn from_value<'a>(value: &'a Self::AssociatedValueType) -> Result<Self, UnknownAssociatedValueError<'a, Self>>;
    /// Returns the first variant (in the declaration order) associated with the value
    /// Panics when no variant is associated with the value
    /// 
    /// See also [Enumeration::from_value]
    fn from_value_unchecked(value: &Self::AssociatedValueType) -> Self;
}

/// When the a default associated value is provided in the [crate::enumerate] macro, this trait will be implemented automatically
pub trait DefaultAssociatedValue: Enumeration {
    /// The default associated value of an [Enumeration] variant
    const DEFAULT_ASSOCIATED_VALUE: Self::AssociatedValueType;
}

/// This macro helps to create enum with trait [Enumeration].
///
/// You have to pass in
/// - attributes (optional)
/// - a visibility qualifier (optional)
/// - a name
/// - an integer type as the underlying type
/// - a type for associated constant values (optional)
///     - a default value for associated constant values (optional)
/// - at least a variant
///     - attributes (before variant) (optional)
///     - with associated constant value (only if the type is given) (optional only if default is given)
///
/// Note that commas after each variant isn't a must (`;` works too).
/// You can also type enum before the name, i.e. `pub enum Foo(u8)` instead of `pub Foo(u8)`
///
/// # Simple usage
/// ```
/// # use enumeration::enumerate;
/// enumerate!(pub Foo(u8)
///     Bar
///     Baz
/// );
/// ```
///
/// produces
///
/// ```
/// # #[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// pub enum Foo {
///     Bar,
///     Baz,
/// }
/// ```
///
/// with [Enumeration] implementation.
///
/// # Syntax
/// 
/// Commas are entirely optional after each variant, you can also use semicolon instead.
///
/// ```
/// # use enumeration::enumerate;
/// enumerate!(pub enum Foo(u8) // enum is optional but suggested by official Rust syntax guidelines (https://rust-lang.github.io/api-guidelines/macros.html#input-syntax-is-evocative-of-the-output-c-evocative)
///     Bar, // comma is optional
///     Baz; // semicolon works too!
/// );
/// ```
///
/// # Casting between index and enumeration
/// ```
/// # use enumeration::enumerate;
/// enumerate!(Foo(u8)
///     Bar
///     Baz
/// );
///
/// # #[test]
/// # fn test() -> Result<Foo, enumeration::OutOfRangeError> {
/// assert_eq!(Foo::variant(0)?, Foo::Bar);
/// assert_eq!(Foo::variant(1)?, Foo::Baz);
/// assert_eq!(Foo::Bar.index(), 0);
/// assert_eq!(Foo::Baz.index(), 1);
/// assert!(Foo::variant(2).is_err());
/// # }
/// ```
///
/// # Associated constant values
/// ```
/// # use enumeration::enumerate;
/// enumerate!(Foo(u8; i32)
///     Bar = 10
///     Baz = 20
/// );
/// ```
///
/// produces
///
/// ```ignore
/// enum Foo {
///     Bar,
///     Baz,
/// }
///
/// # enumeration::impl_try_from_into!(u8, Foo);
/// #
/// impl Enumeration for Foo {
///     fn value(self) -> &'static i32 {
///         const Bar: &'static i32 = &10;
///         const Baz: &'static i32 = &20;
///         
///         match self {
///             Foo::Bar => Bar,
///             Foo::Baz => Baz,
///         }
///     }
/// }
/// 
/// assert_eq!(Foo::Bar.value(), &10);
/// assert_eq!(Foo::Baz.value(), &20);
/// ```
///
/// # Default constant value
/// 
/// ```
/// # use enumeration::enumerate;
/// enumerate!(Foo(u8; i32 = 20)
///     Bar
///     Baz = 10
/// );
/// ```
///
/// produces
///
/// ```ignore
/// enum Foo {
///     Bar,
///     Baz,
/// }
/// 
/// impl DefaultAssociatedValue for Foo {
///     const DEFAULT_ASSOCIATED_VALUE: i32 = 20;
/// }
///
/// impl Enumeration for Foo {
///     fn value(self) -> &'static i32 {
///         const Bar: &'static i32 = &Self::DEFAULT_ASSOCIATED_VALUE;
///         const Baz: &'static i32 = &10;
///         
///         match self {
///             Foo::Bar => Bar,
///             Foo::Baz => Baz,
///         }
///     }
/// }
/// 
/// assert_eq!(Foo::Bar.value(), &20);
/// assert_eq!(Foo::Baz.value(), &10);
/// ```
///
/// The macro will emit error if neither associated constant value nor default constant value is provided.
///
/// ```compile_fail
/// # use enumeration::prelude::*;
/// enumerate!(Foo(u8; i32)
///     Bar
///     Baz = 10
/// );
/// ```
/// 
/// Note that any constant expression can be used as an associated constant.
///
/// # Visibility
/// 
/// ```
/// # use enumeration::prelude::*;
/// enumerate!(pub Foo(u8)
///     Bar
/// );
///
/// enumerate!(Baz(u8)
///     FooBar
/// );
/// ```
///
/// # Attributes
/// Attributes can be attached to enumeration itself, default constant value and each of the variants.
/// It's most useful for documentation.
///
/// ```
/// # use enumeration::prelude::*;
/// enumerate!(#[doc="An enumeration named Foo"] pub Foo(u8)
///     #[doc="Bar"] Bar
///     #[doc="Baz"] Baz
/// );
/// ```
///
/// # Examples
/// ```
/// # use enumeration::enumerate;
/// enumerate!(pub Color(u8; &'static str)
///     Red = "#FF0000"
///     Blue = "#0000FF"
///     Yellow = "#FFFF00"
///     Cyan = "#00FFFF"
/// );
///
/// enumerate!(State(u8)
///     None
///     Stationary
///     Moving
/// );
/// ```
#[macro_export]
macro_rules! enumerate {
    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; () = ()) $($(#[$attr])* $variant = ())*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; () = ()) $($(#[$attr])* $variant = ())*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident; $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; $(#[$default_attr])* $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way enum $name:ident ($t:ident) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; () = ()) $($(#[$attr])* $variant = ())*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way $name:ident ($t:ident) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; () = ()) $($(#[$attr])* $variant = ())*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way enum $name:ident ($t:ident; $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $(#[$default_attr])* $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way $name:ident ($t:ident; $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $(#[$enum_attr])*
        #[repr($t)]
        #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        $visibility enum $name {
            $($(#[$attr])* $variant,)*
        }

        $crate::impl_default!($name $associated_value_type $(= $default_value)?);

        #[allow(non_upper_case_globals)]
        impl $crate::enumeration::Enumeration for $name {
            type Index = $t;
            type AssociatedValueType = $associated_value_type;
            const VARIANT_COUNT: $t = $crate::count!($($variant)*);
            
            #[inline(always)]
            fn value(self) -> &'static Self::AssociatedValueType {
                $crate::validate!($name $associated_value_type, $($default_value)?, $(($($attr)* : $variant : $($associated_value)?))*);

                match self {
                    $(Self::$variant => $variant,)*
                }
            }

            #[inline(always)]
            unsafe fn variant_unchecked(index: Self::Index) -> Self {
                debug_assert!(index >= 0 && index < Self::VARIANT_COUNT);

                std::mem::transmute(index)
            }
        }

        $crate::impl_try_from_into!($t, $name);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident; $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);

        #[allow(non_upper_case_globals)]
        impl $crate::enumeration::FromValue for $name {
            #[inline(always)]
            fn from_value<'a>(value: &'a $associated_value_type) -> ::std::result::Result<Self, $crate::enumeration::UnknownAssociatedValueError<'a, Self>> {
                $crate::validate!($name $associated_value_type, $($default_value)?, $(($($attr)* : $variant : $($associated_value)?))*);

                match value {
                    $($variant => Ok(Self::$variant),)*
                    _ => Err($crate::enumeration::UnknownAssociatedValueError(value)),
                }
            }

            #[inline(always)]
            fn from_value_unchecked(value: &$associated_value_type) -> Self {
                $crate::validate!($name $associated_value_type, $($default_value)?, $(($($attr)* : $variant : $($associated_value)?))*);

                match value {
                    $($variant => Self::$variant,)*
                    _ => unreachable!(),
                }
            }
        }
    };
}
