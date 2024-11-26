
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
