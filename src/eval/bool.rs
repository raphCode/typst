use std::fmt::{self, Debug, Formatter};
use std::ops::{BitAnd, BitOr, Not};

use super::{cast, ty, Int, Value};

/// A type with two states.
///
/// The boolean type has two values: `{true}` and `{false}`. It denotes whether
/// something is active or enabled.
///
/// ## Example { #example }
/// ```example
/// #false \
/// #true \
/// #(1 < 2)
/// ```
///
/// Display: Boolean
/// Category: foundations
#[ty("bool")]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Bool(bool);

impl Bool {
    /// Extract the primitive boolean.
    pub fn as_bool(self) -> bool {
        self.0
    }

    /// Convert to an integer.
    pub fn as_int(self) -> Int {
        (self.0 as i64).into()
    }
}

impl Debug for Bool {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            true => f.pad("true"),
            false => f.pad("false"),
        }
    }
}

impl From<bool> for Bool {
    fn from(value: bool) -> Self {
        Self(value)
    }
}

impl Not for Bool {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl BitAnd for Bool {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitOr for Bool {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

cast! {
    bool,
    self => Value::Bool(self.into()),
    v: Bool => v.as_bool(),
}
