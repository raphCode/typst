use ecow::eco_format;
use once_cell::sync::Lazy;
#[doc(inline)]
pub use typst_macros::ty;

use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

use crate::diag::StrResult;

use super::{func, Func, Scope, Value};

/// A value's type.
///
/// Display: Type
/// Category: Foundations
#[ty("type")]
#[constructor(Type::new_func())]
#[derive(Copy, Clone)]
pub struct Type(&'static NativeType);

impl Type {
    /// Get the type for `T`.
    pub fn of<T: TypeOf>() -> Self {
        T::ty()
    }

    /// Determine a value's type.
    ///
    /// Returns the name of the value's type.
    ///
    /// ## Example { #example }
    /// ```example
    /// #type(12) \
    /// #type(14.7) \
    /// #type("hello") \
    /// #type(none) \
    /// #type([Hi]) \
    /// #type(x => x + 1)
    /// ```
    ///
    /// Display: Type
    /// Category: foundations
    #[func(Type)]
    pub fn new(
        /// The value whose type's to determine.
        value: Value,
    ) -> Type {
        value.ty()
    }

    /// This type's name.
    pub fn name(&self) -> &'static str {
        self.0.info.name
    }

    /// This type's long name.
    pub fn long(&self) -> &'static str {
        self.0.info.long
    }

    /// Get the type's scope.
    pub fn scope(&self) -> &Scope {
        &self.0.info.scope
    }

    /// Get a field from this type's scope, if possible.
    pub fn get(&self, field: &str) -> StrResult<&Value> {
        self.info().scope.get(field).ok_or_else(|| {
            eco_format!("type `{}` does not contain field `{}`", self.name(), field)
        })
    }

    /// This type's constructor.
    pub fn constructor(&self) -> StrResult<&Func> {
        self.0
            .info
            .constructor
            .as_ref()
            .ok_or_else(|| eco_format!("type {self} does not have a constructor"))
    }

    /// Extract details the type.
    pub fn info(&self) -> &TypeInfo {
        &self.0.info
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.pad(self.name())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.pad(self.long())
    }
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name().cmp(other.name())
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const _ as usize);
    }
}

/// Implemented by types.
pub trait TypeOf {
    // The type.
    fn ty() -> Type;
}

impl From<&'static NativeType> for Type {
    fn from(native: &'static NativeType) -> Self {
        Self(native)
    }
}

/// A Type defined by a native Rust type.
pub struct NativeType {
    /// Details about the type.
    pub info: Lazy<TypeInfo>,
}

/// Details about a type.
#[derive(Debug, Clone)]
pub struct TypeInfo {
    /// The type's name.
    pub name: &'static str,
    /// The long name of the type.
    pub long: &'static str,
    /// The display name of the type.
    pub display: &'static str,
    /// Documentation for the type.
    pub docs: &'static str,
    /// Which category the type is part of.
    pub category: &'static str,
    /// The type's constructor.
    pub constructor: Option<Func>,
    /// The type's own scope of fields and sub-functions.
    pub scope: Scope,
}
