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

/// A trait to extend enum.
///
/// You should not implement this directly, instead use [enumerate!].
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

    /// Default for associated constant value.
    const DEFAULT_VARIANT_ASSOCIATED_VALUE: Option<Self::AssociatedValueType>;

    #[inline(always)]
    /// Same as VARIANT_COUNT
    fn count() -> Self::Index { Self::VARIANT_COUNT }

    #[inline(always)]
    /// VARIANT_COUNT as usize
    fn len() -> usize where Self::Index: Into<usize> { Self::count().into() }

    /// Get the reference to the static associated constant value of the variant or the default constant value [Self::DEFAULT_VARIANT_ASSOCIATED_VALUE].
    fn value(self) -> &'static Self::AssociatedValueType;

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
    /// Cast this enumeration to respective index.
    fn to_index(self) -> Self::Index {
        self.into()
    }

    #[inline(always)]
    /// Cast this enumeration to usize
    fn to_usize(self) -> usize where Self::Index: Into<usize> {
        self.to_index().into()
    }
}

/// This macro helps to create enum with trait [Enumeration].
///
/// You have to pass in
/// - attributes (optional)
/// - a visibility qualifier (optional)
/// - a name
/// - a type this enumeration can cast to
/// - a type for associated constant values (optional)
///     - a default value for associated constant values (optional)
/// - at least a variant
///     - attributes (before variant) (optional)
///     - with associated constant value (only if the type is given) (optional only if default is given)
///
/// Note that commas after each variant isn't a must (`;` works too).
/// You can also specify enum after visibility
///
/// All patterns that start with @ is for internal macro implementation only.
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
/// You might notice that there is no comma after each variant.
/// The macro offers multiple syntax branch that will improve readability and comply to [official Rust syntax guidelines](https://rust-lang.github.io/api-guidelines/about.html).
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
/// ```
/// # use enumeration::Enumeration;
/// # #[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// enum Foo {
///     Bar,
///     Baz,
/// }
///
/// # enumeration::impl_try_from_into!(u8, Foo);
/// #
/// impl Enumeration for Foo {
/// #     type Index = u8; type AssociatedValueType = i32; const VARIANT_COUNT: u8 = 2; const DEFAULT_VARIANT_ASSOCIATED_VALUE: Option<i32> = None;
/// #
///     fn value(self) -> &'static i32 {
/// #         #[allow(non_upper_case_globals)]
///         const Bar: i32 = 10;
/// #         #[allow(non_upper_case_globals)]
///         const Baz: i32 = 20;
///         
///         match self {
///             Foo::Bar => &Bar,
///             Foo::Baz => &Baz,
///         }
///     }
///     
///     unsafe fn variant_unchecked(index: Self::Index) -> Self {
///         std::mem::transmute(index)    
///     }
/// }
///
/// fn example() {
///     println!("{}", Foo::Bar.value()); // prints 10
///     println!("{}", Foo::Baz.value()); // prints 20
/// }
/// ```
///
/// # Default constant value
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
/// ```
/// # use enumeration::Enumeration;
/// # #[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// enum Foo {
///     Bar,
///     Baz,
/// }
///
/// # enumeration::impl_try_from_into!(u8, Foo);
/// #
/// impl Enumeration for Foo {
/// #     type Index = u8; type AssociatedValueType = i32; const VARIANT_COUNT: u8 = 2;
///     const DEFAULT_VARIANT_ASSOCIATED_VALUE: Option<i32> = Some(20);
///
///     fn value(self) -> &'static i32 {
/// #         #[allow(non_upper_case_globals)]
///         const Bar: Option<i32> = None;
/// #         #[allow(non_upper_case_globals)]
///         const Baz: Option<i32> = Some(10);
///         
///         match self {
///             Foo::Bar => Bar.as_ref().or(Self::DEFAULT_VARIANT_ASSOCIATED_VALUE.as_ref()).unwrap(),
///             Foo::Baz => Baz.as_ref().or(Self::DEFAULT_VARIANT_ASSOCIATED_VALUE.as_ref()).unwrap(),
///         }
///     }
///     
///     unsafe fn variant_unchecked(index: Self::Index) -> Self {
///         std::mem::transmute(index)    
///     }
/// }
///
/// fn example() {
///     println!("{}", Foo::Bar.value()); // prints 20
///     println!("{}", Foo::Baz.value()); // prints 10
/// }
/// ```
///
/// Note that default constant value is only created once, so the reference to it will always be same.
///
/// The macro will emit error if neither associated constant value nor default constant value is provided.
///
/// ```compile_fail
/// # use enumeration::enumerate;
/// enumerate!(Foo(u8; i32)
///     Bar
///     Baz = 10
/// );
/// ```
///
/// # Visibility
/// ```
/// # use enumeration::enumerate;
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
/// # use enumeration::enumerate;
/// enumerate!(#[doc="An enumeration named Foo"] pub Foo(u8; #[doc="Overwrite default constant value's documentation"] i32 = 0)
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
    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; () = ()) $($(#[$attr])* $variant = ())*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident) $($(#[$attr:meta])* $variant:ident $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; () = ()) $($(#[$attr])* $variant = ())*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis enum $name:ident ($t:ident; $(#[$default_attr:meta])* $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $crate::enumerate!($(#[$enum_attr])* $visibility $name ($t; $(#[$default_attr])* $associated_value_type $(= $default_value)?) $($(#[$attr])* $variant $(= $associated_value)?)*);
    };

    ($(#[$enum_attr:meta])* $visibility:vis $name:ident ($t:ident; $(#[$default_attr:meta])* $associated_value_type:ty $(= $default_value:expr)?) $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)? $(,)? $(;)?)*) => {
        $(#[$enum_attr])*
        #[repr($t)]
        #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        $visibility enum $name {
            $($(#[$attr])* $variant,)*
        }

        impl $crate::enumeration::Enumeration for $name {
            type Index = $t;
            type AssociatedValueType = $associated_value_type;
            const VARIANT_COUNT: $t = $crate::count!($($variant)*);
            $(#[$default_attr])*
            const DEFAULT_VARIANT_ASSOCIATED_VALUE: Option<Self::AssociatedValueType> = $crate::option!($($default_value)?);
            
            #[inline(always)]
            fn value(self) -> &'static Self::AssociatedValueType {
                $crate::validate!($associated_value_type, $($default_value)?, $(($($attr)* : $variant : $($associated_value)?))*);

                match self {
                    $(Self::$variant => $variant.as_ref().or(Self::DEFAULT_VARIANT_ASSOCIATED_VALUE.as_ref()).unwrap(),)*
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
}
