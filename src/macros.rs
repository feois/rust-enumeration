
#[doc(hidden)]
#[macro_export]
macro_rules! impl_try_from_into {
    ($t:ty, $name:ident) => {
        impl std::convert::TryFrom<$t> for $name {
            type Error = $crate::enumeration::OutOfRangeError<$name>;

            #[inline(always)]
            fn try_from(value: $t) -> Result<Self, $crate::enumeration::OutOfRangeError<$name>> {
                #[allow(unused_comparisons)]
                if value >= 0 && value < <Self as $crate::enumeration::Enumeration>::VARIANT_COUNT {
                    Ok(unsafe { std::mem::transmute(value) })
                } else {
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

#[doc(hidden)]
#[macro_export]
macro_rules! impl_default {
    ($name:ident $t:ty) => {

    };

    ($name:ident $t:ty = $default:expr) => {
        impl $crate::enumeration::DefaultAssociatedValue for $name {
            const DEFAULT_ASSOCIATED_VALUE: $t = $default;
        }
    };
}
