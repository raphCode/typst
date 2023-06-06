use std::fmt::{self, Debug, Formatter};

use ecow::EcoString;

use crate::eval::{func, ty};

/// A label for an element.
///
/// Inserting a label into content attaches it to the closest previous element
/// that is not a space. Then, the element can be [referenced]($func/ref) and
/// styled through the label.
///
/// ## Example { #example }
/// ```example
/// #show <a>: set text(blue)
/// #show label("b"): set text(red)
///
/// = Heading <a>
/// *Strong* #label("b")
/// ```
///
/// ## Syntax { #syntax }
/// This function also has dedicated syntax: You can create a label by enclosing
/// its name in angle brackets. This works both in markup and code.
///
/// Display: Label
/// Category: meta
#[ty("label")]
#[constructor(Label::new_func())]
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Label(pub EcoString);

impl Label {
    /// Creates a label from a string.
    ///
    /// Display: Construct
    /// Category: meta
    #[func(Label)]
    pub fn new(
        /// The name of the label.
        name: EcoString,
    ) -> Label {
        Self(name)
    }
}

impl Debug for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "<{}>", self.0)
    }
}

/// Indicates that an element cannot be labelled.
pub trait Unlabellable {}
