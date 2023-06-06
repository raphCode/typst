use super::*;

/// A size or distance, possibly expressed with contextual units.
///
/// Typst supports the following length units:
///
/// - Points: `{72pt}`
/// - Millimeters: `{254mm}`
/// - Centimeters: `{2.54cm}`
/// - Inches: `{1in}`
/// - Relative to font size: `{2.5em}`
///
/// ## Example { #example }
/// ```example
/// #rect(width: 20pt)
/// #rect(width: 2em)
/// #rect(width: 1in)
/// ```
///
/// Display: Length
/// Category: layout
#[ty("length")]
#[derive(Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Length {
    /// The absolute part.
    pub abs: Abs,
    /// The font-relative part.
    pub em: Em,
}

impl Length {
    /// The zero length.
    pub const fn zero() -> Self {
        Self { abs: Abs::zero(), em: Em::zero() }
    }

    /// Try to compute the absolute value of the length.
    pub fn try_abs(self) -> Option<Self> {
        (self.abs.is_zero() || self.em.is_zero())
            .then(|| Self { abs: self.abs.abs(), em: self.em.abs() })
    }

    /// Try to divide two lengths.
    pub fn try_div(self, other: Self) -> Option<f64> {
        if self.abs.is_zero() && other.abs.is_zero() {
            Some(self.em / other.em)
        } else if self.em.is_zero() && other.em.is_zero() {
            Some(self.abs / other.abs)
        } else {
            None
        }
    }
}

impl Debug for Length {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match (self.abs.is_zero(), self.em.is_zero()) {
            (false, false) => write!(f, "{:?} + {:?}", self.abs, self.em),
            (true, false) => self.em.fmt(f),
            (_, true) => self.abs.fmt(f),
        }
    }
}

impl Numeric for Length {
    fn zero() -> Self {
        Self::zero()
    }

    fn is_finite(self) -> bool {
        self.abs.is_finite() && self.em.is_finite()
    }
}

impl PartialOrd for Length {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.em.is_zero() && other.em.is_zero() {
            self.abs.partial_cmp(&other.abs)
        } else if self.abs.is_zero() && other.abs.is_zero() {
            self.em.partial_cmp(&other.em)
        } else {
            None
        }
    }
}

impl From<Abs> for Length {
    fn from(abs: Abs) -> Self {
        Self { abs, em: Em::zero() }
    }
}

impl From<Em> for Length {
    fn from(em: Em) -> Self {
        Self { abs: Abs::zero(), em }
    }
}

impl Neg for Length {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self { abs: -self.abs, em: -self.em }
    }
}

impl Add for Length {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self { abs: self.abs + rhs.abs, em: self.em + rhs.em }
    }
}

sub_impl!(Length - Length -> Length);

impl Mul<f64> for Length {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        Self { abs: self.abs * rhs, em: self.em * rhs }
    }
}

impl Mul<Length> for f64 {
    type Output = Length;

    fn mul(self, rhs: Length) -> Self::Output {
        rhs * self
    }
}

impl Mul<Float> for Length {
    type Output = Self;

    fn mul(self, rhs: Float) -> Self::Output {
        self * rhs.as_f64()
    }
}

impl Mul<Length> for Float {
    type Output = Length;

    fn mul(self, rhs: Length) -> Self::Output {
        rhs * self.as_f64()
    }
}

impl Div<f64> for Length {
    type Output = Self;

    fn div(self, rhs: f64) -> Self::Output {
        Self { abs: self.abs / rhs, em: self.em / rhs }
    }
}

impl Div<Float> for Length {
    type Output = Self;

    fn div(self, rhs: Float) -> Self::Output {
        self / rhs.as_f64()
    }
}

assign_impl!(Length += Length);
assign_impl!(Length -= Length);
assign_impl!(Length *= f64);
assign_impl!(Length /= f64);

impl Resolve for Length {
    type Output = Abs;

    fn resolve(self, styles: StyleChain) -> Self::Output {
        self.abs + self.em.resolve(styles)
    }
}
