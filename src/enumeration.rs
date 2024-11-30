//! This module provides the core trait [`Enumeration`]

use std::fmt::{Debug, Display};
use std::hash::Hash;

/// Error type when casting from [`Enumeration::Index`] to [`Enumeration`].
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

/// Error type when converting from [`Enumeration::AssociatedValueType`] to [`Enumeration`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnknownAssociatedValueError<'a, T: Enumeration>(pub &'a T::AssociatedValueType);

impl<'a, T: Enumeration> Display for UnknownAssociatedValueError<'a, T>
where
    T::AssociatedValueType: Display,
{
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Unknown associated value: {}", self.0,)
    }
}

impl<'a, T: Enumeration> std::error::Error for UnknownAssociatedValueError<'a, T> where
    T::AssociatedValueType: Display
{
}

/// A trait to extend enum.
///
/// You should not implement this directly, instead use [`crate::enumerate`].
/// The rest of the library assumes the constants are correct.
///
/// # Associated types
///
/// - [`Enumeration`] is the enum type of variants, e.g. `Enum::Foo` or `Enum::Bar`
/// - [`Enumeration::Index`] is the underlying type of the enum, i.e. enum internally uses this type as the integer representation, hence `size_of::<Enumeration>() == size_of::<Enumeration::Index>()`
/// - [`Enumeration::AssociatedValueType`] is the type of the associated value
///
/// # Variant to Index
///
/// [`Enumeration::to_index`] will convert the variant ([`Enumeration`]) to index ([`Enumeration::Index`])
///
/// # Index to Variant
///
/// - [`Enumeration::variant`] will check if index is in range before converting
/// - [`Enumeration::variant_unchecked`] will not check but may result in undefined behaviour if index is out of range
///
/// # Variant count
///
/// - [`Enumeration::VARIANT_COUNT`] is a constant of type [`Enumeration::Index`] set to the number of variants
/// - [`Enumeration::count`] simply returns [`Enumeration::VARIANT_COUNT`]
/// - [`Enumeration::len`] returns [`Enumeration::VARIANT_COUNT`] but converts it to [`usize`] (does not work if [`Enumeration::Index`] is larger than [`usize`], e.g. [`u128`])
///
/// # Iterating
///
/// - not supported for types larger than [`usize`], e.g. [`u128`] on 64-bit architectures
/// - [`Enumeration::iter`] will return an [`Iterator`]
/// - use [`Enumeration::try_iter`] if [`Enumeration::Index`] does not implement [`From<usize>`] (e.g. types smaller than [`usize`] like [`u8`] or [`u16`])
///     - The reason is that it internally uses [`usize`] to index through variants as opposed to [`Enumeration::Index`] because currently [`std::ops::Range`] does not support generics until [`std::iter::Step`] is stabilized
///     - For types smaller than [`usize`], [`TryFrom<usize>`] and [`Result::unwrap`] is used instead, while it may sound problematic, `enum` itself guarantees that the variant count will not exceed the maximum of [`Enumeration::Index`]. However it may be slower than [`Enumeration::iter`] depending on compiler optimization.
///
/// # Two-way conversion
///
/// You can convert [`Enumeration::AssociatedValueType`] back into [`Enumeration`] variant if [`FromValue`] is [implemented](crate::enumerate#two-way-conversion)
///
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     Foo(u8; char)
///         Bar = 'a'
///         Baz = 'b'
/// }
/// 
/// assert_eq!(Foo::from_value(&'a'), Ok(Foo::Bar));
/// assert_eq!(Foo::from_value(&'b'), Ok(Foo::Baz));
/// assert!(Foo::from_value(&'c').is_err());
/// ```
/// 
/// You can skip the safety checks but it will panic on unknown value
/// 
/// ```should_panic
/// # use enumeration::prelude::*;
/// # enumerate! {
/// #     Foo(u8; char)
/// #         Bar = 'a'
/// #         Baz = 'b'
/// # }
/// assert_eq!(Foo::from_value_unchecked(&'a'), Foo::Bar);
/// assert_eq!(Foo::from_value_unchecked(&'b'), Foo::Baz);
/// let _ = Foo::from_value_unchecked(&'c'); // panic
/// ```
/// 
/// If two or more variants are associated with the same value, the first variant declared will be returned
/// 
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     Foo(u8; char)
///         Bar = 'a'
///         Baz = 'a'
///         FooBar = 'b'
///         FooBaz = 'b'
///         BarBar = 'a'
///         BarBaz = 'b'
/// }
/// 
/// assert_eq!(Foo::from_value(&'a'), Ok(Foo::Bar));
/// assert_eq!(Foo::from_value(&'b'), Ok(Foo::FooBar));
/// ```
/// 
/// Same logic applies to default values, if two or more variants have no associated value provided (i.e. they are associated with default values), the first variant will be returned
/// 
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     Foo(u8; char = 'c')
///         Bar = 'a'
///         Baz = 'b'
///         FooBar
///         FooBaz
///         BarBar = 'c'
///         BarBaz = 'd'
/// }
/// 
/// assert_eq!(Foo::from_value(&'a'), Ok(Foo::Bar));
/// assert_eq!(Foo::from_value(&'b'), Ok(Foo::Baz));
/// assert_eq!(Foo::from_value(&'c'), Ok(Foo::FooBar));
/// assert_eq!(Foo::from_value(&'d'), Ok(Foo::BarBaz));
/// assert_eq!(Foo::from_value(&Foo::DEFAULT_ASSOCIATED_VALUE), Ok(Foo::FooBar));
/// assert_eq!(Foo::from_value(Foo::BarBar.value()), Ok(Foo::FooBar)); // because BarBar has the same value as default value, FooBar is returned instead
/// ```
pub trait Enumeration:
    TryFrom<Self::Index, Error = OutOfRangeError<Self>>
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
{
    /// The type of index this enumeration can cast to.
    type Index: Debug + Display;

    /// The type of associated constant value of the variants of this enumeration.
    type AssociatedValueType: 'static + Debug;

    /// The number of variants of this enumeration.
    const VARIANT_COUNT: Self::Index;

    #[inline(always)]
    /// Same as [`Enumeration::VARIANT_COUNT`]
    fn count() -> Self::Index {
        Self::VARIANT_COUNT
    }

    #[inline(always)]
    /// [`Enumeration::VARIANT_COUNT`] as usize.
    /// Not implemented if [`Enumeration::Index`] is larger than [`usize`] (e.g. [`u128`]).
    fn len() -> usize
    where
        Self::Index: Into<usize>,
    {
        Self::count().into()
    }

    /// Get the reference to the static associated constant value of the variant or the [default constant value](DefaultAssociatedValue::DEFAULT_ASSOCIATED_VALUE).
    /// Implementation details is in [crate::enumerate#associated-constant-values]
    fn value(self) -> &'static Self::AssociatedValueType;

    #[inline(always)]
    /// Get the associated constant value by copying
    fn value_copy(self) -> Self::AssociatedValueType
    where
        Self::AssociatedValueType: Copy,
    {
        *self.value()
    }

    #[inline(always)]
    /// Get the associated constant value by cloning
    fn value_clone(self) -> Self::AssociatedValueType
    where
        Self::AssociatedValueType: Clone,
    {
        self.value().clone()
    }

    #[inline(always)]
    /// Cast index to the respective enumeration.
    ///
    /// # Errors
    ///
    /// If the index is out of range (< zero or >= [`Enumeration::VARIANT_COUNT`]), this function will return [`OutOfRangeError`]
    fn variant(index: Self::Index) -> Result<Self, Self::Error> {
        Self::try_from(index)
    }

    /// Cast index to the respective enumeration without checking.
    ///
    /// May result in undefined behaviour if index is out of the enum's range
    unsafe fn variant_unchecked(index: Self::Index) -> Self;

    #[inline(always)]
    /// Cast this variant to respective index.
    fn to_index(self) -> Self::Index {
        self.into()
    }

    #[inline(always)]
    /// Cast this variant to usize.
    fn to_usize(self) -> usize
    where
        Self::Index: Into<usize>,
    {
        self.to_index().into()
    }

    #[inline(always)]
    /// See the [Iterating](Enumeration#iterating) section
    fn iter() -> impl Iterator<Item = Self>
    where
        Self::Index: Into<usize> + From<usize>,
    {
        (0..Self::len()).map(|i| unsafe { Self::variant_unchecked(i.into()) })
    }

    #[inline(always)]
    /// See the [Iterating](Enumeration#iterating) section
    fn try_iter() -> impl Iterator<Item = Self>
    where
        Self::Index: Into<usize> + TryFrom<usize>,
    {
        (0..Self::len()).map(|i| unsafe {
            Self::variant_unchecked(i.try_into().unwrap_or_else(|_| unreachable!()))
        })
    }
}

/// Returns the first variant (in the declaration order) associated with the value.
/// More info at [Enumeration#two-way-conversion].
pub trait FromValue: Enumeration {
    /// Returns the first variant (in the declaration order) associated with the value.
    /// Returns error when no variant is associated with the value.
    /// More info at [Enumeration#two-way-conversion].
    ///
    /// See also [`FromValue::from_value_unchecked`]
    fn from_value<'a>(
        value: &'a Self::AssociatedValueType,
    ) -> Result<Self, UnknownAssociatedValueError<'a, Self>>;

    /// Returns the first variant (in the declaration order) associated with the value.
    /// Panics when no variant is associated with the value.
    /// More info at [Enumeration#two-way-conversion].
    ///
    /// See also [`FromValue::from_value`]
    fn from_value_unchecked(value: &Self::AssociatedValueType) -> Self;
}

/// When the a default associated value is provided in the [`crate::enumerate`] macro, this trait will be implemented automatically
pub trait DefaultAssociatedValue: Enumeration {
    /// The default associated value of an [`Enumeration`] variant
    const DEFAULT_ASSOCIATED_VALUE: Self::AssociatedValueType;
}

/// This macro helps to create enum with trait [`Enumeration`].
///
/// # Syntax
///
/// `<code>` means optional
///
/// ```ignore
/// enumerate! {
///     <#[attribute] ... #[attribute]>
///     <pub> <one-way> <enum> EnumName(EnumType <; <AssociatedValueAlias> AssociatedType <= DefaultValue>>)
///         <#[attribute] ... #[attribute]>
///         Variant1 <= Value> <,> <;>
///         <#[attribute] ... #[attribute]>
///         Variant2 <= Value> <,> <;>
///         ...
///         <#[attribute] ... #[attribute]>
///         VariantN <= Value> <,> <;>
/// }
/// ```
///
/// Arguments (in order)
/// - attributes of the enum (optional)
/// - a visibility qualifier (optional)
/// - one-way keyword to disable implementation of [`FromValue`] (optional)
/// - a name for the enum
/// - an integer type as the underlying type
/// - a type for associated constant values (optional)
///     - an alias for the name of the association (before the type) (optional)
///     - a default value for associated constant values (optional)
/// - at least a variant
///     - attributes (before variant) (optional)
///     - with associated constant value (only if the associated type is given) (optional only if default is given)
///
/// Notes:
/// - Commas after each variant is not required but accepted by the macro (semicolons are also accepted).
/// - Keyword `enum` is optional but suggested by [`official Rust syntax guidelines`](https://rust-lang.github.io/api-guidelines/macros.html#input-syntax-is-evocative-of-the-output-c-evocative)
///
/// ```
/// # use enumeration::enumerate;
/// enumerate! {
///     pub enum Foo(u8)
///         Bar, // comma is optional
///         Baz; // semicolon works too!
/// }
///
/// enumerate! {
///     FooBar(u8; char)
///         BarBar = 'a'
///         BazBar = 'b'
/// }
///
/// enumerate! {
///     #[doc="foobaz"]
///     pub FooBaz(u8; &'static str = "default string")
///         #[doc="barbaz"]
///         BarBaz = "hello world"
///         BazBaz // value not provided will be default value
/// }
/// ```
///
/// # Expanded macro
///
/// ```
/// # use enumeration::enumerate;
/// enumerate! {
///     pub Foo(u8)
///         Bar
///         Baz
/// }
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
/// with implementation of [`Enumeration`] and some other useful traits.
///
/// # Casting between index and enumeration
///
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     Foo(u8)
///         Bar
///         Baz
/// }
///
/// assert_eq!(Foo::variant(0), Ok(Foo::Bar));
/// assert_eq!(Foo::variant(1), Ok(Foo::Baz));
/// assert_eq!(Foo::Bar.to_index(), 0);
/// assert_eq!(Foo::Baz.to_index(), 1);
/// assert!(Foo::variant(2).is_err());
/// ```
///
/// See also [`Enumeration::variant`], [`Enumeration::variant_unchecked`], [`Enumeration::to_index`]
///
/// # Associated constant values
///
/// ```
/// # use enumeration::enumerate;
/// enumerate! {
///     Foo(u8; i32)
///         Bar = 10
///         Baz = 20
/// }
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
/// See also [`Enumeration::value`]
///
/// # Default constant value
///
/// ```
/// # use enumeration::enumerate;
/// enumerate! {
///     Foo(u8; i32 = 20)
///         Bar
///         Baz = 10
/// }
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
/// enumerate! {
///     Foo(u8; i32)
///         Bar
///         Baz = 10
/// }
/// ```
///
/// Note that any constant expression can be used as an associated constant.
/// See also [`DefaultAssociatedValue::DEFAULT_ASSOCIATED_VALUE`]
///
/// # Visibility
///
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     pub Foo(u8)
///         Bar
/// }
///
/// enumerate! {
///     Baz(u8)
///         FooBar
/// }
/// ```
///
/// # Attributes
/// Attributes can be attached to enumeration itself, default constant value and each of the variants.
/// It's mostly useful for documentation.
///
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     #[doc="An enumeration named Foo"]
///     pub Foo(u8)
///         #[doc="Bar"]
///         Bar
///
///         #[doc="Baz"]
///         Baz
/// }
/// ```
///
/// # Examples
///
/// ```
/// # use enumeration::enumerate;
/// enumerate! {
///     pub Color(u8; &'static str)
///         Red = "#FF0000"
///         Blue = "#0000FF"
///         Yellow = "#FFFF00"
///         Cyan = "#00FFFF"
/// }
///
/// enumerate! {
///     State(u8)
///         None
///         Stationary
///         Moving
/// }
/// ```
///
/// # Two-way conversion
///
/// This macro will implement [`FromValue`] that allows converting [`Enumeration::AssociatedValueType`] back into [`Enumeration`].
///
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     # one-way
///     Enum(u8; char)
///         Foo = 'a'
///         Bar = 'b'
///         Baz = 'c'
/// }
///
/// // will also generate
///
/// impl FromValue for Enum {
///     fn from_value<'a>(value: &'a Self::AssociatedValueType) -> Result<Self, UnknownAssociatedValueError<'a, Self>> {
///         match value {
///             &'a' => Ok(Enum::Foo),
///             &'b' => Ok(Enum::Bar),
///             &'c' => Ok(Enum::Baz),
///             _ => Err(UnknownAssociatedValueError(value)),
///         }
///     }
///     fn from_value_unchecked(value: &Self::AssociatedValueType) -> Self {
///         match value {
///             &'a' => Enum::Foo,
///             &'b' => Enum::Bar,
///             &'c' => Enum::Baz,
///             _ => unreachable!(),
///         }
///     }
/// }
/// ```
///
/// ### Opting out
///
/// You may not desire to have a two-way conversions in some cases like [function pointers](https://github.com/rust-lang/rust/issues/70861)
///
/// ```compile_fail
/// # use enumeration::prelude::*;
/// fn f() {}
///
/// enumerate! {
///     Foo(u8; fn())
///         Bar = (f as fn())
/// }
/// ```
///
/// You can opt out of the [`FromValue`] implementation by adding the `one-way` keyword
///
/// ```
/// # use enumeration::prelude::*;
/// fn f() {}
///
/// enumerate! {
///     one-way Foo(u8; fn())
///         Bar = (f as fn())
/// }
/// ```
///
/// More info at [Enumeration#two-way-conversion].
/// 
/// # Alias
/// 
/// You can give the associated value an alias
/// 
/// ```
/// # use enumeration::prelude::*;
/// enumerate! {
///     Foo(u8; some_alias: char)
///         Bar = 'a'
///         Baz = 'b'
/// }
/// 
/// // the left code and the right code is completely same
/// assert_eq!(Foo::Bar.value(), Foo::Bar.some_alias());
/// assert_eq!(Foo::Baz.value(), Foo::Baz.some_alias());
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
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way enum $name:ident ($t:ident; $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident; $value_alias:ident : $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; $value_alias : $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way enum $name:ident ($t:ident; $value_alias:ident : $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $value_alias : $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis one-way $name:ident ($t:ident; $value_alias:ident : $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
        
        impl $name where {
            #[inline(always)]
            fn $value_alias(self) -> &'static <Self as $crate::enumeration::Enumeration>::AssociatedValueType {
                self.value()
            }
        }
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
                debug_assert!({ #[allow(unused_comparisons)] let b = index >= 0 && index < Self::VARIANT_COUNT; b });

                ::std::mem::transmute(index)
            }
        }

        impl ::std::borrow::Borrow<$associated_value_type> for $name {
            #[inline(always)]
            fn borrow(&self) -> &$associated_value_type {
                use $crate::enumeration::Enumeration;
                self.value()
            }
        }

        $crate::impl_try_from_into!($t, $name);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident; $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
        $crate::impl_from_value!($name: $associated_value_type $(= $default_value)?; $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident; $value_alias:ident : $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility one-way $name ($t; $value_alias : $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
        $crate::impl_from_value!($name: $associated_value_type $(= $default_value)?; $($(#[$attr])* $variant $(= $associated_value)?)*);
    };
}
