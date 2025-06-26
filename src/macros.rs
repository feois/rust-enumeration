#[macro_export]
macro_rules! impl_try_from_into {
    ($t:ty, $name:ident) => {
        impl std::convert::TryFrom<$t> for $name {
            type Error = $crate::enumeration::OutOfRangeError<$name>;

            #[allow(clippy::manual_range_contains)]
            #[inline(always)]
            fn try_from(value: $t) -> Result<Self, $crate::enumeration::OutOfRangeError<$name>> {
                #[allow(unused_comparisons)]
                if value >= 0 && value < <Self as $crate::enumeration::Enumeration>::VARIANT_COUNT {
                    Ok(unsafe { std::mem::transmute(value) })
                }
                else {
                    Err($crate::enumeration::OutOfRangeError(value))
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

#[macro_export]
macro_rules! count {
    () => {
        0
    };

    ($head:tt $($rest:tt)*) => {
        1 + $crate::count!($($rest)*)
    };
}

#[macro_export]
macro_rules! validate {
    ($name:ident $t:ty, , ($($attr:meta)* : $variant:ident :)) => {
        compile_error!("Neither associated constant value nor default constant value provided")
    };

    ($name:ident $t:ty, $default:expr, ($($attr:meta)* : $variant:ident :)) => {
        $(#[$attr])*
        #[allow(non_upper_case_globals)]
        const $variant: &'static $t = &<$name as $crate::enumeration::DefaultAssociatedValue>::DEFAULT_ASSOCIATED_VALUE;
    };

    ($name:ident $t:ty, $($default:expr)?, ($($attr:meta)* : $variant:ident : $associated_value:expr)) => {
        $(#[$attr])*
        #[allow(non_upper_case_globals)]
        const $variant: &'static $t = &$associated_value;
    };

    ($name:ident $t:ty, $($default:expr)?, ($($attr:meta)* : $variant:ident : $($associated_value:expr)?) $(($($at:meta)* : $v:ident : $($a:expr)?))+) => {
        $crate::validate!($name $t, $($default)?, ($($attr)* : $variant : $($associated_value)?));
        $crate::validate!($name $t, $($default)?, $(($($at)* : $v : $($a)?))+);
    };
}

#[macro_export]
macro_rules! impl_default {
    ($name:ident $t:ty) => {};

    ($name:ident $t:ty = $default:expr) => {
        impl $crate::enumeration::DefaultAssociatedValue for $name {
            const DEFAULT_ASSOCIATED_VALUE: $t = $default;
        }
    };
}

#[macro_export]
macro_rules! impl_from_value {
    ($name:ident : $associated_value_type:ty $(= $default_value:expr)?; $($(#[$attr:meta])* $variant:ident $(= $associated_value:expr)?)*) => {
        #[allow(non_upper_case_globals)]
        impl $crate::enumeration::FromValue for $name {
            #[inline(always)]
            fn from_value<'a>(value: &'a $associated_value_type) -> ::std::result::Result<Self, $crate::enumeration::UnknownAssociatedValueError<'a, Self>> {
                $crate::validate!($name $associated_value_type, $($default_value)?, $(($($attr)* : $variant : $($associated_value)?))*);

                match value {
                    $($variant => Ok(Self::$variant),)*
                    #[allow(unreachable_patterns)]
                    _ => Err($crate::enumeration::UnknownAssociatedValueError(value)),
                }
            }

            #[inline(always)]
            fn from_value_unchecked(value: &$associated_value_type) -> Self {
                $crate::validate!($name $associated_value_type, $($default_value)?, $(($($attr)* : $variant : $($associated_value)?))*);

                match value {
                    $($variant => Self::$variant,)*
                    #[allow(unreachable_patterns)]
                    _ => unreachable!(),
                }
            }
        }
    };
}
