#![warn(missing_docs)]

//! Provides extension to rust enum
//!
//! This crate provides `Enumeration` trait for rust `enum` with the following features
//! - implementation for common traits (Clone, Copy, Hash, etc.)
//! - getting number of variants through constant `Enumeration::VARIANT_COUNT`
//! - casting between index (of type `Enumeration::Index`) and enumeration
//! - attaching a constant value to each of the variants
//! - runtime representation of enumeration

/// Convenience re-export of common members
pub mod prelude {
    pub use super::{enumerate, Enumeration, Variant, VariantWith, IteratorHelper, OutOfRangeError, CastError};
}

use std::any::{TypeId, type_name};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;

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

/// Error type when attempting to cast [Variant] to [Enumeration]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CastError<T: Enumeration>(PhantomData<T>);

impl<T: Enumeration> Display for CastError<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Unable to cast from enumeration type from {}", type_name::<T>())
    }
}

impl<T: Enumeration> std::error::Error for CastError<T> {}

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
}

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
}

impl<T: Iterator> IteratorHelper for T {}

/// Provides runtime specialized representation of [Enumeration].
/// 
/// To avoid using `dyn` for mixing [Enumeration], you can use [Variant] with a trade-off for having to try casting before using it.
/// 
/// # Examples
/// ```
/// # use enumeration::prelude::*;
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
        Self { type_id: TypeId::of::<T>(), index: e.to_index() }
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
        Self { type_id: TypeId::of::<T>(), index: e.to_index(), value: e.value() }
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
    pub fn cast<E: Enumeration<Index = T, AssociatedValueType = U>>(self) -> Result<E, CastError<E>> {
        if TypeId::of::<E>() == self.type_id {
            Ok(E::variant(self.index).unwrap())
        }
        else {
            Err(CastError(PhantomData))
        }
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

        impl $crate::Enumeration for $name {
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

#[doc(hidden)]
#[macro_export]
macro_rules! impl_try_from_into {
    ($t:ty, $name:ident) => {
        impl std::convert::TryFrom<$t> for $name {
            type Error = $crate::OutOfRangeError<$name>;

            #[inline(always)]
            fn try_from(value: $t) -> Result<Self, $crate::OutOfRangeError<$name>> {
                #[allow(unused_comparisons)]
                if value >= 0 && value < <Self as $crate::Enumeration>::VARIANT_COUNT {
                    Ok(unsafe { std::mem::transmute(value) })
                } else {
                    Err($crate::OutOfRangeError(value))
                }
            }
        }

        impl Into<$t> for $name {
            #[inline(always)]
            fn into(self) -> $t {
                unsafe { std::mem::transmute(self) }
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! count {
    () => {
        0
    };

    ($head:tt $($rest:tt)*) => {
        1 + $crate::count!($($rest)*)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! validate {
    ($t:ty, , ($($attr:meta)* : $variant:ident :)) => {
        compile_error!("Neither associated constant value nor default constant value provided")
    };

    ($t:ty, $default:expr, ($($attr:meta)* : $variant:ident :)) => {
        #[allow(non_upper_case_globals)]
        const $variant: Option<$t> = None;
    };

    ($t:ty, $($default:expr)?, ($($attr:meta)* : $variant:ident : $associated_value:expr)) => {
        #[allow(non_upper_case_globals)]
        const $variant: Option<$t> = Some($associated_value);
    };

    ($t:ty, $($default:expr)?, ($($attr:meta)* : $variant:ident : $($associated_value:expr)?) $(($($at:meta)* : $v:ident : $($a:expr)?))+) => {
        $crate::validate!($t, $($default)?, ($($attr)* : $variant : $($associated_value)?));
        $crate::validate!($t, $($default)?, $(($($at)* : $v : $($a)?))+);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! option {
    () => {
        None
    };

    ($value:expr) => {
        Some($value)
    };
}
