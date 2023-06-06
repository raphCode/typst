use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::*;
use std::str::FromStr;

use ecow::eco_format;

use super::{cast, func, ty, Int, Str, Value};
use crate::geom::Ratio;

/// A floating-pointer number.
///
/// A limited-precision representation of a real number. Typst uses 64 bits to
/// store floats. Wherever a float is expected, you can also pass an
/// [integer]($type/integer).
///
/// ## Example
/// ```example
/// #3.14 \
/// #1e4 \
/// #(10 / 4)
/// ```
///
/// Display: Float
/// Category: foundations
#[ty("float")]
#[constructor(Float::convert_func())]
#[derive(Copy, Clone)]
pub struct Float(f64);

impl Float {
    /// Convert a value to a float.
    ///
    /// - Booleans are converted to `0.0` or `1.0`.
    /// - Integers are converted to the closest 64-bit float.
    /// - Ratios are divided by 100%.
    /// - Strings are parsed in base 10 to the closest 64-bit float.
    ///   Exponential notation is supported.
    ///
    /// ## Example { #example }
    /// ```example
    /// #float(false) \
    /// #float(true) \
    /// #float(4) \
    /// #float(40%) \
    /// #float("2.7") \
    /// #float("1e5")
    /// ```
    ///
    /// Display: Construct
    /// Category: foundations
    #[func(Float)]
    pub fn convert(
        /// The value that should be converted to a float.
        value: ToFloat,
    ) -> Float {
        Self(value.0)
    }

    /// Extract the primitive float.
    pub fn as_f64(self) -> f64 {
        self.0
    }

    /// Convert to an integer.
    pub fn as_int(self) -> Int {
        (self.0 as i64).into()
    }
}

/// A value that can be cast to a float.
pub struct ToFloat(f64);

cast! {
    ToFloat,
    v: bool => Self(v as i64 as f64),
    v: i64 => Self(v as f64),
    v: f64 => Self(v),
    v: Ratio => Self(v.get()),
    v: Str => Self(v.parse().map_err(|_| eco_format!("invalid float: {}", v))?),
}

impl Display for Float {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Debug for Float {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Eq for Float {}

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl PartialEq<f64> for Float {
    fn eq(&self, other: &f64) -> bool {
        self == &Self(*other)
    }
}

impl PartialOrd for Float {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }

    fn lt(&self, other: &Self) -> bool {
        self.0 < other.0
    }

    fn le(&self, other: &Self) -> bool {
        self.0 <= other.0
    }

    fn gt(&self, other: &Self) -> bool {
        self.0 > other.0
    }

    fn ge(&self, other: &Self) -> bool {
        self.0 >= other.0
    }
}

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl FromStr for Float {
    type Err = std::num::ParseFloatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(Self)
    }
}

impl Neg for Float {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl Add for Float {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub for Float {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Mul for Float {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl Div for Float {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0 / rhs.0)
    }
}

impl From<f64> for Float {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

cast! {
    f64,
    self => Value::Float(self.into()),
    v: Float => v.as_f64(),
}
