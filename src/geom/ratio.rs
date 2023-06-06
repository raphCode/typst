use super::*;

/// A ratio of a whole.
///
/// Written as a number, followed by a percent sign.
///
/// ## Example { #example }
/// ```example
/// #set align(center)
/// #scale(x: 150%)[
///   Scaled apart.
/// ]
/// ```
///
/// Display: Ratio
/// Category: layout
#[ty("ratio")]
#[derive(Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Ratio(Scalar);

impl Ratio {
    /// A ratio of `0%` represented as `0.0`.
    pub const fn zero() -> Self {
        Self(Scalar(0.0))
    }

    /// A ratio of `100%` represented as `1.0`.
    pub const fn one() -> Self {
        Self(Scalar(1.0))
    }

    /// Create a new ratio from a value, where `1.0` means `100%`.
    pub const fn new(ratio: f64) -> Self {
        Self(Scalar(ratio))
    }

    /// Get the underlying ratio.
    pub const fn get(self) -> f64 {
        (self.0).0
    }

    /// Whether the ratio is zero.
    pub fn is_zero(self) -> bool {
        self.0 == 0.0
    }

    /// Whether the ratio is one.
    pub fn is_one(self) -> bool {
        self.0 == 1.0
    }

    /// The absolute value of this ratio.
    pub fn abs(self) -> Self {
        Self::new(self.get().abs())
    }

    /// Return the ratio of the given `whole`.
    pub fn of<T: Numeric>(self, whole: T) -> T {
        let resolved = whole * self.get();
        if resolved.is_finite() {
            resolved
        } else {
            T::zero()
        }
    }
}

impl Debug for Ratio {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}%", round_2(100.0 * self.get()))
    }
}

impl Neg for Ratio {
    type Output = Self;

    fn neg(self) -> Self {
        Self(-self.0)
    }
}

impl Add for Ratio {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

sub_impl!(Ratio - Ratio -> Ratio);

impl Mul for Ratio {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self(self.0 * other.0)
    }
}

impl Mul<f64> for Ratio {
    type Output = Self;

    fn mul(self, other: f64) -> Self {
        Self(self.0 * other)
    }
}

impl Mul<Float> for Ratio {
    type Output = Self;

    fn mul(self, rhs: Float) -> Self::Output {
        self * rhs.as_f64()
    }
}

impl Mul<Ratio> for f64 {
    type Output = Ratio;

    fn mul(self, other: Ratio) -> Ratio {
        other * self
    }
}

impl Mul<Ratio> for Float {
    type Output = Ratio;

    fn mul(self, other: Ratio) -> Ratio {
        other * self.as_f64()
    }
}

impl Div for Ratio {
    type Output = f64;

    fn div(self, other: Self) -> f64 {
        self.get() / other.get()
    }
}

impl Div<f64> for Ratio {
    type Output = Self;

    fn div(self, other: f64) -> Self {
        Self(self.0 / other)
    }
}

impl Div<Ratio> for f64 {
    type Output = Self;

    fn div(self, other: Ratio) -> Self {
        self / other.get()
    }
}

impl Div<Float> for Ratio {
    type Output = Self;

    fn div(self, rhs: Float) -> Self::Output {
        self / rhs.as_f64()
    }
}

impl Div<Ratio> for Float {
    type Output = Self;

    fn div(self, other: Ratio) -> Self {
        (self.as_f64() / other.get()).into()
    }
}

assign_impl!(Ratio += Ratio);
assign_impl!(Ratio -= Ratio);
assign_impl!(Ratio *= Ratio);
assign_impl!(Ratio *= f64);
assign_impl!(Ratio /= f64);
