use std::str::FromStr;

use super::*;
use crate::diag::{At, SourceResult};
use crate::eval::{Args, Str};
use crate::syntax::Spanned;

/// A color in a specific color space.
///
/// Typst supports:
/// - sRGB through the [`rgb` function]($func/rgb)
/// - Device CMYK through [`cmyk` function]($func/cmyk)
/// - D65 Gray through the [`luma` function]($func/luma)
///
/// Display: Color
/// Category: visualize
#[ty("color")]
#[scope({
    scope.define("luma", Color::luma_func());
    scope.define("rgb", Color::rgb_func());
    scope.define("cmyk", Color::cmyk_func());
    scope.define("lighten", Color::lighten_func());
    scope.define("darken", Color::darken_func());
    scope.define("negate", Color::negate_func());
    scope.define("black", Color::BLACK);
    scope.define("gray", Color::GRAY);
    scope.define("silver", Color::SILVER);
    scope.define("white", Color::WHITE);
    scope.define("navy", Color::NAVY);
    scope.define("blue", Color::BLUE);
    scope.define("aqua", Color::AQUA);
    scope.define("teal", Color::TEAL);
    scope.define("eastern", Color::EASTERN);
    scope.define("purple", Color::PURPLE);
    scope.define("fuchsia", Color::FUCHSIA);
    scope.define("maroon", Color::MAROON);
    scope.define("red", Color::RED);
    scope.define("orange", Color::ORANGE);
    scope.define("yellow", Color::YELLOW);
    scope.define("olive", Color::OLIVE);
    scope.define("green", Color::GREEN);
    scope.define("lime", Color::LIME);
    scope
})]
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Color {
    /// An 8-bit luma color.
    Luma(LumaColor),
    /// An 8-bit RGBA color.
    Rgba(RgbaColor),
    /// An 8-bit CMYK color.
    Cmyk(CmykColor),
}

impl Color {
    pub const BLACK: Self = Self::Rgba(RgbaColor::new(0x00, 0x00, 0x00, 0xFF));
    pub const GRAY: Self = Self::Rgba(RgbaColor::new(0xAA, 0xAA, 0xAA, 0xFF));
    pub const SILVER: Self = Self::Rgba(RgbaColor::new(0xDD, 0xDD, 0xDD, 0xFF));
    pub const WHITE: Self = Self::Rgba(RgbaColor::new(0xFF, 0xFF, 0xFF, 0xFF));
    pub const NAVY: Self = Self::Rgba(RgbaColor::new(0x00, 0x1f, 0x3f, 0xFF));
    pub const BLUE: Self = Self::Rgba(RgbaColor::new(0x00, 0x74, 0xD9, 0xFF));
    pub const AQUA: Self = Self::Rgba(RgbaColor::new(0x7F, 0xDB, 0xFF, 0xFF));
    pub const TEAL: Self = Self::Rgba(RgbaColor::new(0x39, 0xCC, 0xCC, 0xFF));
    pub const EASTERN: Self = Self::Rgba(RgbaColor::new(0x23, 0x9D, 0xAD, 0xFF));
    pub const PURPLE: Self = Self::Rgba(RgbaColor::new(0xB1, 0x0D, 0xC9, 0xFF));
    pub const FUCHSIA: Self = Self::Rgba(RgbaColor::new(0xF0, 0x12, 0xBE, 0xFF));
    pub const MAROON: Self = Self::Rgba(RgbaColor::new(0x85, 0x14, 0x4b, 0xFF));
    pub const RED: Self = Self::Rgba(RgbaColor::new(0xFF, 0x41, 0x36, 0xFF));
    pub const ORANGE: Self = Self::Rgba(RgbaColor::new(0xFF, 0x85, 0x1B, 0xFF));
    pub const YELLOW: Self = Self::Rgba(RgbaColor::new(0xFF, 0xDC, 0x00, 0xFF));
    pub const OLIVE: Self = Self::Rgba(RgbaColor::new(0x3D, 0x99, 0x70, 0xFF));
    pub const GREEN: Self = Self::Rgba(RgbaColor::new(0x2E, 0xCC, 0x40, 0xFF));
    pub const LIME: Self = Self::Rgba(RgbaColor::new(0x01, 0xFF, 0x70, 0xFF));

    /// Create a grayscale color.
    ///
    /// ## Example { #example }
    /// ```example
    /// #for x in range(250, step: 50) {
    ///   box(square(fill: luma(x)))
    /// }
    /// ```
    ///
    /// Display: Luma
    /// Category: visualize
    #[func(Color)]
    pub fn luma(
        /// The gray component.
        gray: Component,
    ) -> Color {
        LumaColor::new(gray.0).into()
    }

    /// Create an RGB(A) color.
    ///
    /// The color is specified in the sRGB color space.
    ///
    /// _Note:_ While you can specify transparent colors and Typst's preview will
    /// render them correctly, the PDF export does not handle them properly at the
    /// moment. This will be fixed in the future.
    ///
    /// ## Example { #example }
    /// ```example
    /// #square(fill: rgb("#b1f2eb"))
    /// #square(fill: rgb(87, 127, 230))
    /// #square(fill: rgb(25%, 13%, 65%))
    /// ```
    ///
    /// Display: RGB
    /// Category: visualize
    #[func(Color)]
    pub fn rgb(
        /// The color in hexadecimal notation.
        ///
        /// Accepts three, four, six or eight hexadecimal digits and optionally
        /// a leading hashtag.
        ///
        /// If this string is given, the individual components should not be given.
        ///
        /// ```example
        /// #text(16pt, rgb("#239dad"))[
        ///   *Typst*
        /// ]
        /// ```
        #[external]
        hex: Str,
        /// The red component.
        #[external]
        red: Component,
        /// The green component.
        #[external]
        green: Component,
        /// The blue component.
        #[external]
        blue: Component,
        /// The alpha component.
        #[external]
        alpha: Component,
        /// The real arguments (the other arguments are just for the docs, this
        /// function is a bit involved, so we parse the arguments manually).
        args: Args,
    ) -> SourceResult<Color> {
        let mut args = args;
        Ok(if let Some(string) = args.find::<Spanned<Str>>()? {
            RgbaColor::from_str(&string.v).at(string.span)?.into()
        } else {
            let Component(r) = args.expect("red component")?;
            let Component(g) = args.expect("green component")?;
            let Component(b) = args.expect("blue component")?;
            let Component(a) = args.eat()?.unwrap_or(Component(255));
            RgbaColor::new(r, g, b, a).into()
        })
    }

    /// Create a CMYK color.
    ///
    /// This is useful if you want to target a specific printer. The conversion
    /// to RGB for display preview might differ from how your printer reproduces
    /// the color.
    ///
    /// ## Example { #example }
    /// ```example
    /// #square(
    ///   fill: cmyk(27%, 0%, 3%, 5%)
    /// )
    /// ````
    ///
    /// Display: CMYK
    /// Category: visualize
    #[func(Color)]
    pub fn cmyk(
        /// The cyan component.
        cyan: RatioComponent,
        /// The magenta component.
        magenta: RatioComponent,
        /// The yellow component.
        yellow: RatioComponent,
        /// The key component.
        key: RatioComponent,
    ) -> Color {
        CmykColor::new(cyan.0, magenta.0, yellow.0, key.0).into()
    }

    /// Lightens a color by a given factor.
    ///
    /// Display: Lighten
    /// Category: visualize
    #[func(Color)]
    pub fn lighten(
        self,
        /// The factor to lighten the color by.
        factor: Ratio,
    ) -> Color {
        match self {
            Self::Luma(luma) => Self::Luma(luma.lighten(factor)),
            Self::Rgba(rgba) => Self::Rgba(rgba.lighten(factor)),
            Self::Cmyk(cmyk) => Self::Cmyk(cmyk.lighten(factor)),
        }
    }

    /// Darkens a color by a given factor.
    ///
    /// Display: Darken
    /// Category: visualize
    #[func(Color)]
    pub fn darken(
        self,
        /// The factor to darken the color by.
        factor: Ratio,
    ) -> Color {
        match self {
            Self::Luma(luma) => Self::Luma(luma.darken(factor)),
            Self::Rgba(rgba) => Self::Rgba(rgba.darken(factor)),
            Self::Cmyk(cmyk) => Self::Cmyk(cmyk.darken(factor)),
        }
    }

    /// Produces the negative of the color.
    ///
    /// Display: Negate
    /// Category: visualize
    #[func(Color)]
    pub fn negate(self) -> Color {
        match self {
            Self::Luma(luma) => Self::Luma(luma.negate()),
            Self::Rgba(rgba) => Self::Rgba(rgba.negate()),
            Self::Cmyk(cmyk) => Self::Cmyk(cmyk.negate()),
        }
    }
}

impl Color {
    /// Convert this color to RGBA.
    pub fn to_rgba(self) -> RgbaColor {
        match self {
            Self::Luma(luma) => luma.to_rgba(),
            Self::Rgba(rgba) => rgba,
            Self::Cmyk(cmyk) => cmyk.to_rgba(),
        }
    }
}

impl Debug for Color {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Luma(c) => Debug::fmt(c, f),
            Self::Rgba(c) => Debug::fmt(c, f),
            Self::Cmyk(c) => Debug::fmt(c, f),
        }
    }
}

/// An integer or ratio component.
pub struct Component(u8);

cast! {
    Component,
    v: i64 => match v {
        0 ..= 255 => Self(v as u8),
        _ => Err("number must be between 0 and 255")?,
    },
    v: Ratio => if (0.0 ..= 1.0).contains(&v.get()) {
        Self((v.get() * 255.0).round() as u8)
    } else {
        Err("ratio must be between 0% and 100%")?
    },
}

/// A component that must be a ratio.
pub struct RatioComponent(u8);

cast! {
    RatioComponent,
    v: Ratio => if (0.0 ..= 1.0).contains(&v.get()) {
        Self((v.get() * 255.0).round() as u8)
    } else {
        Err("ratio must be between 0% and 100%")?
    },
}

/// An 8-bit grayscale color.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct LumaColor(pub u8);

impl LumaColor {
    /// Construct a new luma color.
    pub const fn new(luma: u8) -> Self {
        Self(luma)
    }

    /// Convert to an opque RGBA color.
    pub const fn to_rgba(self) -> RgbaColor {
        RgbaColor::new(self.0, self.0, self.0, u8::MAX)
    }

    /// Convert to CMYK as a fraction of true black.
    pub fn to_cmyk(self) -> CmykColor {
        CmykColor::new(
            round_u8(self.0 as f64 * 0.75),
            round_u8(self.0 as f64 * 0.68),
            round_u8(self.0 as f64 * 0.67),
            round_u8(self.0 as f64 * 0.90),
        )
    }

    /// Lighten this color by a factor.
    pub fn lighten(self, factor: Ratio) -> Self {
        let inc = round_u8((u8::MAX - self.0) as f64 * factor.get());
        Self(self.0.saturating_add(inc))
    }

    /// Darken this color by a factor.
    pub fn darken(self, factor: Ratio) -> Self {
        let dec = round_u8(self.0 as f64 * factor.get());
        Self(self.0.saturating_sub(dec))
    }

    /// Negate this color.
    pub fn negate(self) -> Self {
        Self(u8::MAX - self.0)
    }
}

impl Debug for LumaColor {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "luma({})", self.0)
    }
}

impl From<LumaColor> for Color {
    fn from(luma: LumaColor) -> Self {
        Self::Luma(luma)
    }
}

/// An 8-bit RGBA color.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct RgbaColor {
    /// Red channel.
    pub r: u8,
    /// Green channel.
    pub g: u8,
    /// Blue channel.
    pub b: u8,
    /// Alpha channel.
    pub a: u8,
}

impl RgbaColor {
    /// Construct a new RGBA color.
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    /// Lighten this color by a factor.
    ///
    /// The alpha channel is not affected.
    pub fn lighten(self, factor: Ratio) -> Self {
        let lighten =
            |c: u8| c.saturating_add(round_u8((u8::MAX - c) as f64 * factor.get()));
        Self {
            r: lighten(self.r),
            g: lighten(self.g),
            b: lighten(self.b),
            a: self.a,
        }
    }

    /// Darken this color by a factor.
    ///
    /// The alpha channel is not affected.
    pub fn darken(self, factor: Ratio) -> Self {
        let darken = |c: u8| c.saturating_sub(round_u8(c as f64 * factor.get()));
        Self {
            r: darken(self.r),
            g: darken(self.g),
            b: darken(self.b),
            a: self.a,
        }
    }

    /// Negate this color.
    ///
    /// The alpha channel is not affected.
    pub fn negate(self) -> Self {
        Self {
            r: u8::MAX - self.r,
            g: u8::MAX - self.g,
            b: u8::MAX - self.b,
            a: self.a,
        }
    }
}

impl FromStr for RgbaColor {
    type Err = &'static str;

    /// Constructs a new color from hex strings like the following:
    /// - `#aef` (shorthand, with leading hashtag),
    /// - `7a03c2` (without alpha),
    /// - `abcdefff` (with alpha).
    ///
    /// The hashtag is optional and both lower and upper case are fine.
    fn from_str(hex_str: &str) -> Result<Self, Self::Err> {
        let hex_str = hex_str.strip_prefix('#').unwrap_or(hex_str);
        if hex_str.chars().any(|c| !c.is_ascii_hexdigit()) {
            return Err("color string contains non-hexadecimal letters");
        }

        let len = hex_str.len();
        let long = len == 6 || len == 8;
        let short = len == 3 || len == 4;
        let alpha = len == 4 || len == 8;
        if !long && !short {
            return Err("color string has wrong length");
        }

        let mut values: [u8; 4] = [u8::MAX; 4];
        for elem in if alpha { 0..4 } else { 0..3 } {
            let item_len = if long { 2 } else { 1 };
            let pos = elem * item_len;

            let item = &hex_str[pos..(pos + item_len)];
            values[elem] = u8::from_str_radix(item, 16).unwrap();

            if short {
                // Duplicate number for shorthand notation, i.e. `a` -> `aa`
                values[elem] += values[elem] * 16;
            }
        }

        Ok(Self::new(values[0], values[1], values[2], values[3]))
    }
}

impl Debug for RgbaColor {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "rgba({}, {}, {}, {})", self.r, self.g, self.b, self.a,)?;
        } else {
            write!(f, "rgb(\"#{:02x}{:02x}{:02x}", self.r, self.g, self.b)?;
            if self.a != 255 {
                write!(f, "{:02x}", self.a)?;
            }
            write!(f, "\")")?;
        }
        Ok(())
    }
}

impl<T: Into<RgbaColor>> From<T> for Color {
    fn from(rgba: T) -> Self {
        Self::Rgba(rgba.into())
    }
}

cast! {
    RgbaColor,
    self => Value::Color(self.into()),
}

/// An 8-bit CMYK color.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct CmykColor {
    /// The cyan component.
    pub c: u8,
    /// The magenta component.
    pub m: u8,
    /// The yellow component.
    pub y: u8,
    /// The key (black) component.
    pub k: u8,
}

impl CmykColor {
    /// Construct a new CMYK color.
    pub const fn new(c: u8, m: u8, y: u8, k: u8) -> Self {
        Self { c, m, y, k }
    }

    /// Convert this color to RGBA.
    pub fn to_rgba(self) -> RgbaColor {
        let k = self.k as f64 / 255.0;
        let f = |c| {
            let c = c as f64 / 255.0;
            round_u8(255.0 * (1.0 - c) * (1.0 - k))
        };

        RgbaColor { r: f(self.c), g: f(self.m), b: f(self.y), a: 255 }
    }

    /// Lighten this color by a factor.
    pub fn lighten(self, factor: Ratio) -> Self {
        let lighten = |c: u8| c.saturating_sub(round_u8(c as f64 * factor.get()));
        Self {
            c: lighten(self.c),
            m: lighten(self.m),
            y: lighten(self.y),
            k: lighten(self.k),
        }
    }

    /// Darken this color by a factor.
    pub fn darken(self, factor: Ratio) -> Self {
        let darken =
            |c: u8| c.saturating_add(round_u8((u8::MAX - c) as f64 * factor.get()));
        Self {
            c: darken(self.c),
            m: darken(self.m),
            y: darken(self.y),
            k: darken(self.k),
        }
    }

    /// Negate this color.
    ///
    /// Does not affect the key component.
    pub fn negate(self) -> Self {
        Self {
            c: u8::MAX - self.c,
            m: u8::MAX - self.m,
            y: u8::MAX - self.y,
            k: self.k,
        }
    }
}

impl Debug for CmykColor {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let g = |c| 100.0 * (c as f64 / 255.0);
        write!(
            f,
            "cmyk({:.1}%, {:.1}%, {:.1}%, {:.1}%)",
            g(self.c),
            g(self.m),
            g(self.y),
            g(self.k),
        )
    }
}

impl From<CmykColor> for Color {
    fn from(cmyk: CmykColor) -> Self {
        Self::Cmyk(cmyk)
    }
}

/// Convert to the closest u8.
fn round_u8(value: f64) -> u8 {
    value.round() as u8
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_color_strings() {
        #[track_caller]
        fn test(hex: &str, r: u8, g: u8, b: u8, a: u8) {
            assert_eq!(RgbaColor::from_str(hex), Ok(RgbaColor::new(r, g, b, a)));
        }

        test("f61243ff", 0xf6, 0x12, 0x43, 0xff);
        test("b3d8b3", 0xb3, 0xd8, 0xb3, 0xff);
        test("fCd2a9AD", 0xfc, 0xd2, 0xa9, 0xad);
        test("233", 0x22, 0x33, 0x33, 0xff);
        test("111b", 0x11, 0x11, 0x11, 0xbb);
    }

    #[test]
    fn test_parse_invalid_colors() {
        #[track_caller]
        fn test(hex: &str, message: &str) {
            assert_eq!(RgbaColor::from_str(hex), Err(message));
        }

        test("a5", "color string has wrong length");
        test("12345", "color string has wrong length");
        test("f075ff011", "color string has wrong length");
        test("hmmm", "color string contains non-hexadecimal letters");
        test("14B2AH", "color string contains non-hexadecimal letters");
    }
}
