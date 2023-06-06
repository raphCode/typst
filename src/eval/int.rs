use std::fmt::{self, Debug, Display, Formatter};
use std::num::{NonZeroI64, NonZeroIsize, NonZeroU64, NonZeroUsize};
use std::str::FromStr;

use ecow::{eco_format, EcoString};

use super::{cast, func, ty, Float, Str, Value};
use crate::diag::StrResult;

/// A whole number.
///
/// The number can be negative, zero, or positive. As Typst uses 64 bits to
/// store integers, integers cannot be smaller than `{-9223372036854775808}` or
/// larger than `{9223372036854775807}`.
///
/// The number can also be specified as hexadecimal, octal, or binary by
/// starting it with a zero followed by either `x`, `o`, or `b`.
///
/// ## Example { #example }
/// ```example
/// #(1 + 2) \
/// #(2 - 5) \
/// #(3 + 4 < 8)
///
/// #0xff \
/// #0o10 \
/// #0b1001
/// ```
///
/// Display: Integer
/// Category: foundations
#[ty("int")]
#[constructor(Int::convert_func())]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Int(i64);

impl Int {
    /// Convert a value to an integer.
    ///
    /// - Booleans are converted to `0` or `1`.
    /// - Floats are floored to the next 64-bit integer.
    /// - Strings are parsed in base 10.
    ///
    /// ## Example { #example }
    /// ```example
    /// #int(false) \
    /// #int(true) \
    /// #int(2.7) \
    /// #{ int("27") + int("4") }
    /// ```
    ///
    /// Display: Construct
    /// Category: foundations
    #[func(Int)]
    pub fn convert(
        /// The value that should be converted to an integer.
        value: ToInt,
    ) -> Int {
        Self(value.0)
    }

    /// Extract the primitive integer.
    pub fn as_i64(self) -> i64 {
        self.0
    }

    /// Convert the integer to a float.
    pub fn as_float(self) -> Float {
        (self.0 as f64).into()
    }

    /// Checked negation of an integer.
    pub fn checked_neg(self) -> StrResult<Self> {
        Ok(Self(self.0.checked_neg().ok_or_else(too_large)?))
    }

    /// Checked adddition of two integers.
    pub fn checked_add(self, rhs: Self) -> StrResult<Self> {
        Ok(Self(self.0.checked_add(rhs.0).ok_or_else(too_large)?))
    }

    /// Checked subtraction of two integers.
    pub fn checked_sub(self, rhs: Self) -> StrResult<Self> {
        Ok(Self(self.0.checked_sub(rhs.0).ok_or_else(too_large)?))
    }

    /// Checked multiplication of two integers.
    pub fn checked_mul(self, rhs: Self) -> StrResult<Self> {
        Ok(Self(self.0.checked_mul(rhs.0).ok_or_else(too_large)?))
    }
}

/// A value that can be cast to an integer.
pub struct ToInt(i64);

cast! {
    ToInt,
    v: bool => Self(v as i64),
    v: i64 => Self(v),
    v: f64 => Self(v as i64),
    v: Str => Self(v.parse().map_err(|_| eco_format!("invalid integer: {}", v))?),
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Debug for Int {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl FromStr for Int {
    type Err = std::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(Self)
    }
}

macro_rules! to_int {
    ($($ty:ty)*) => {
        $(impl From<$ty> for Int {
            fn from(value: $ty) -> Self {
                Self(value as _)
            }
        }

        cast! {
            $ty,
            self => Value::Int(self.into()),
        })*
    }
}

macro_rules! non_zero_to_int {
    ($($ty:ty)*) => {
        $(impl From<$ty> for Int {
            fn from(value: $ty) -> Self {
                Self(value.get() as _)
            }
        }

        cast! {
            $ty,
            self => Value::Int(self.into()),
        })*
    }
}

to_int! {
    u8 u16 u32 u64 usize i8 i16 i32 i64 isize
}

non_zero_to_int! {
    NonZeroU64 NonZeroI64 NonZeroUsize NonZeroIsize
}

// todo!() make it for all number types and handle bounds.
cast! {
    u8,
    v: Int => v.as_i64() as u8,
}

cast! {
    i32,
    v: Int => v.as_i64() as i32,
}

cast! {
    i64,
    v: Int => v.as_i64(),
}

cast! {
    u32,
    v: i64 => v.try_into().map_err(|_| {
        if v < 0 {
            "number must be at least zero"
        } else {
            "number too large"
        }
    })?,
}

cast! {
    u64,
    v: i64 => v.try_into().map_err(|_| {
        if v < 0 {
            "number must be at least zero"
        } else {
            "number too large"
        }
    })?,
}

cast! {
    usize,
    v: i64 => v.try_into().map_err(|_| {
        if v < 0 {
            "number must be at least zero"
        } else {
            "number too large"
        }
    })?,
}

cast! {
    NonZeroI64,
    v: i64 => v.try_into()
        .map_err(|_| if v == 0 {
            "number must not be zero"
        } else {
            "number too large"
        })?,
}

cast! {
    NonZeroU64,
    v: i64 => v
        .try_into()
        .and_then(|v: u64| v.try_into())
        .map_err(|_| if v <= 0 {
            "number must be positive"
        } else {
            "number too large"
        })?,
}

cast! {
    NonZeroUsize,
    v: i64 => v
        .try_into()
        .and_then(|v: usize| v.try_into())
        .map_err(|_| if v <= 0 {
            "number must be positive"
        } else {
            "number too large"
        })?,
}

/// The error message if the operation would overflow.
#[cold]
fn too_large() -> EcoString {
    "value too large".into()
}
