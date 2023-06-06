use std::any::Any;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use ecow::eco_format;
use siphasher::sip128::{Hasher128, SipHasher13};

use super::{
    cast, format_str, ops, Args, Array, AutoValue, Bool, CastInfo, Content, Dict, Float,
    FromValue, Func, Int, IntoValue, Module, NoneValue, Reflect, Scope, Str, Symbol,
    Type, TypeOf,
};
use crate::diag::StrResult;
use crate::geom::{Abs, Angle, Color, Em, Fr, Length, Ratio, Rel};
use crate::model::{Label, Styles};
use crate::syntax::{ast, Span};

/// A computational value.
#[derive(Default, Clone, Hash)]
#[allow(clippy::derived_hash_with_manual_eq)]
pub enum Value {
    /// The value that indicates the absence of a meaningful value.
    #[default]
    None,
    /// A value that indicates some smart default behaviour.
    Auto,
    /// A boolean: `true, false`.
    Bool(Bool),
    /// An integer: `120`.
    Int(Int),
    /// A floating-point number: `1.2`, `10e-4`.
    Float(Float),
    /// A length: `12pt`, `3cm`, `1.5em`, `1em - 2pt`.
    Length(Length),
    /// An angle: `1.5rad`, `90deg`.
    Angle(Angle),
    /// A ratio: `50%`.
    Ratio(Ratio),
    /// A relative length, combination of a ratio and a length: `20% + 5cm`.
    Relative(Rel<Length>),
    /// A fraction: `1fr`.
    Fraction(Fr),
    /// A color value: `#f79143ff`.
    Color(Color),
    /// A symbol: `arrow.l`.
    Symbol(Symbol),
    /// A string: `"string"`.
    Str(Str),
    /// A label: `<intro>`.
    Label(Label),
    /// A content value: `[*Hi* there]`.
    Content(Content),
    // Content styles.
    Styles(Styles),
    /// An array of values: `(1, "hi", 12cm)`.
    Array(Array),
    /// A dictionary value: `(a: 1, b: "hi")`.
    Dict(Dict),
    /// An executable function.
    Func(Func),
    /// Captured arguments to a function.
    Args(Args),
    /// A type.
    Type(Type),
    /// A module.
    Module(Module),
    /// A dynamic value.
    Dyn(Dynamic),
}

impl Value {
    /// Create a new dynamic value.
    pub fn dynamic<T>(any: T) -> Self
    where
        T: Debug + TypeOf + PartialEq + Hash + Sync + Send + 'static,
    {
        Self::Dyn(Dynamic::new(any))
    }

    /// Create a numeric value from a number with a unit.
    pub fn numeric(pair: (f64, ast::Unit)) -> Self {
        let (v, unit) = pair;
        match unit {
            ast::Unit::Length(unit) => Abs::with_unit(v, unit).into_value(),
            ast::Unit::Angle(unit) => Angle::with_unit(v, unit).into_value(),
            ast::Unit::Em => Em::new(v).into_value(),
            ast::Unit::Fr => Fr::new(v).into_value(),
            ast::Unit::Percent => Ratio::new(v / 100.0).into_value(),
        }
    }

    /// The type of this value.
    pub fn ty(&self) -> Type {
        match self {
            Self::None => Type::of::<NoneValue>(),
            Self::Auto => Type::of::<AutoValue>(),
            Self::Bool(_) => Type::of::<Bool>(),
            Self::Int(_) => Type::of::<Int>(),
            Self::Float(_) => Type::of::<Float>(),
            Self::Length(_) => Type::of::<Length>(),
            Self::Angle(_) => Type::of::<Angle>(),
            Self::Ratio(_) => Type::of::<Ratio>(),
            Self::Relative(_) => Type::of::<Rel<Length>>(),
            Self::Fraction(_) => Type::of::<Fr>(),
            Self::Color(_) => Type::of::<Color>(),
            Self::Symbol(_) => Type::of::<Symbol>(),
            Self::Str(_) => Type::of::<Str>(),
            Self::Label(_) => Type::of::<Label>(),
            Self::Content(_) => Type::of::<Content>(),
            Self::Styles(_) => Type::of::<Styles>(),
            Self::Array(_) => Type::of::<Array>(),
            Self::Dict(_) => Type::of::<Dict>(),
            Self::Func(_) => Type::of::<Func>(),
            Self::Args(_) => Type::of::<Args>(),
            Self::Type(_) => Type::of::<Type>(),
            Self::Module(_) => Type::of::<Module>(),
            Self::Dyn(v) => v.ty(),
        }
    }

    /// Try to cast the value into a specific type.
    pub fn cast<T: FromValue>(self) -> StrResult<T> {
        T::from_value(self)
    }

    /// Try to access a field on the value.
    pub fn field(&self, field: &str) -> StrResult<Value> {
        match self {
            Self::Symbol(symbol) => symbol.clone().modified(field).map(Self::Symbol),
            Self::Dict(dict) => dict.at(field.into(), None),
            Self::Content(content) => content.at(field.into(), None),
            Self::Type(ty) => ty.get(field).cloned(),
            Self::Func(func) => func.get(field).cloned(),
            Self::Module(module) => module.get(field).cloned(),
            v => Err(eco_format!("cannot access fields on type {}", v.ty())),
        }
    }

    /// The associated scope, if this is a function, type, or module.
    pub fn scope(&self) -> Option<&Scope> {
        match self {
            Self::Func(func) => func.info().map(|info| &info.scope),
            Self::Type(ty) => Some(&ty.info().scope),
            Self::Module(module) => Some(module.scope()),
            _ => None,
        }
    }

    /// Return the debug representation of the value.
    pub fn repr(&self) -> Str {
        format_str!("{:?}", self)
    }

    /// Attach a span to the value, if possible.
    pub fn spanned(self, span: Span) -> Self {
        match self {
            Value::Content(v) => Value::Content(v.spanned(span)),
            Value::Func(v) => Value::Func(v.spanned(span)),
            v => v,
        }
    }

    /// Return the display representation of the value.
    pub fn display(self) -> Content {
        match self {
            Self::None => Content::empty(),
            Self::Int(v) => item!(text)(eco_format!("{}", v)),
            Self::Float(v) => item!(text)(eco_format!("{}", v)),
            Self::Str(v) => item!(text)(v.into()),
            Self::Symbol(v) => item!(text)(v.get().into()),
            Self::Content(v) => v,
            Self::Func(_) => Content::empty(),
            Self::Type(_) => Content::empty(),
            Self::Module(module) => module.content(),
            _ => item!(raw)(self.repr().into(), Some("typc".into()), false),
        }
    }

    /// Try to extract documentation for the value.
    pub fn docs(&self) -> Option<&'static str> {
        match self {
            Self::Func(func) => func.info().map(|info| info.docs),
            _ => None,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::None => Debug::fmt(&NoneValue, f),
            Self::Auto => Debug::fmt(&AutoValue, f),
            Self::Bool(v) => Debug::fmt(v, f),
            Self::Int(v) => Debug::fmt(v, f),
            Self::Float(v) => Debug::fmt(v, f),
            Self::Length(v) => Debug::fmt(v, f),
            Self::Angle(v) => Debug::fmt(v, f),
            Self::Ratio(v) => Debug::fmt(v, f),
            Self::Relative(v) => Debug::fmt(v, f),
            Self::Fraction(v) => Debug::fmt(v, f),
            Self::Color(v) => Debug::fmt(v, f),
            Self::Symbol(v) => Debug::fmt(v, f),
            Self::Str(v) => Debug::fmt(v, f),
            Self::Label(v) => Debug::fmt(v, f),
            Self::Content(v) => Debug::fmt(v, f),
            Self::Styles(v) => Debug::fmt(v, f),
            Self::Array(v) => Debug::fmt(v, f),
            Self::Dict(v) => Debug::fmt(v, f),
            Self::Func(v) => Debug::fmt(v, f),
            Self::Args(v) => Debug::fmt(v, f),
            Self::Type(v) => Debug::fmt(v, f),
            Self::Module(v) => Debug::fmt(v, f),
            Self::Dyn(v) => Debug::fmt(v, f),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        ops::equal(self, other)
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        ops::compare(self, other).ok()
    }
}

/// A dynamic value.
#[derive(Clone, Hash)]
#[allow(clippy::derived_hash_with_manual_eq)]
pub struct Dynamic(Arc<dyn Bounds>);

impl Dynamic {
    /// Create a new instance from any value that satisfies the required bounds.
    pub fn new<T>(any: T) -> Self
    where
        T: Debug + TypeOf + PartialEq + Hash + Sync + Send + 'static,
    {
        Self(Arc::new(any))
    }

    /// Whether the wrapped type is `T`.
    pub fn is<T: 'static>(&self) -> bool {
        (*self.0).as_any().is::<T>()
    }

    /// Try to downcast to a reference to a specific type.
    pub fn downcast<T: 'static>(&self) -> Option<&T> {
        (*self.0).as_any().downcast_ref()
    }

    /// The name of the stored value's type.
    pub fn ty(&self) -> Type {
        self.0.dyn_ty()
    }
}

impl Debug for Dynamic {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl PartialEq for Dynamic {
    fn eq(&self, other: &Self) -> bool {
        self.0.dyn_eq(other)
    }
}

cast! {
    Dynamic,
    self => Value::Dyn(self),
}

trait Bounds: Debug + Sync + Send + 'static {
    fn as_any(&self) -> &dyn Any;
    fn dyn_eq(&self, other: &Dynamic) -> bool;
    fn dyn_ty(&self) -> Type;
    fn hash128(&self) -> u128;
}

impl<T> Bounds for T
where
    T: Debug + TypeOf + PartialEq + Hash + Sync + Send + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn dyn_eq(&self, other: &Dynamic) -> bool {
        let Some(other) = other.downcast::<Self>() else { return false };
        self == other
    }

    fn dyn_ty(&self) -> Type {
        Type::of::<T>()
    }

    #[tracing::instrument(skip_all)]
    fn hash128(&self) -> u128 {
        // Also hash the TypeId since values with different types but
        // equal data should be different.
        let mut state = SipHasher13::new();
        self.type_id().hash(&mut state);
        self.hash(&mut state);
        state.finish128().as_u128()
    }
}

impl Hash for dyn Bounds {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u128(self.hash128());
    }
}

/// Implement traits for primitives.
macro_rules! primitive {
    (
        $ty:ty: $name:literal, $variant:ident
        $(, $other:ident$(($binding:ident))? => $out:expr)*
    ) => {
        impl Reflect for $ty {
            fn describe() -> CastInfo {
                CastInfo::Type(Type::of::<Self>())
            }

            fn castable(value: &Value) -> bool {
                matches!(value, Value::$variant(_)
                    $(|  primitive!(@$other $(($binding))?))*)
            }
        }

        impl IntoValue for $ty {
            fn into_value(self) -> Value {
                Value::$variant(self)
            }
        }

        impl FromValue for $ty {
            fn from_value(value: Value) -> StrResult<Self> {
                match value {
                    Value::$variant(v) => Ok(v),
                    $(Value::$other$(($binding))? => Ok($out),)*
                    v => Err(eco_format!(
                        "expected {}, found {}",
                        Type::of::<Self>(),
                        v.ty(),
                    )),
                }
            }
        }
    };

    (@$other:ident($binding:ident)) => { Value::$other(_) };
    (@$other:ident) => { Value::$other };
}

primitive! { Bool: "boolean", Bool }
primitive! { Int: "integer", Int }
primitive! { Float: "float", Float, Int(v) => v.as_float() }
primitive! { Length: "length", Length }
primitive! { Angle: "angle", Angle }
primitive! { Ratio: "ratio", Ratio }
primitive! { Rel<Length>:  "relative length",
    Relative,
    Length(v) => v.into(),
    Ratio(v) => v.into()
}
primitive! { Fr: "fraction", Fraction }
primitive! { Color: "color", Color }
primitive! { Symbol: "symbol", Symbol }
primitive! {
    Str: "string",
    Str,
    Symbol(symbol) => symbol.get().into()
}
primitive! { Label: "label", Label }
primitive! { Content: "content",
    Content,
    None => Content::empty(),
    Symbol(v) => item!(text)(v.get().into()),
    Str(v) => item!(text)(v.into())
}
primitive! { Styles: "styles", Styles }
primitive! { Array: "array", Array }
primitive! { Dict: "dictionary", Dict }
primitive! {
    Func: "function",
    Func,
    Type(ty) => ty.constructor()?.clone()
}
primitive! { Args: "arguments", Args }
primitive! { Module: "module", Module }
primitive! { Type: "type", Type }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{array, dict};
    use crate::geom::RgbaColor;

    #[track_caller]
    fn test(value: impl IntoValue, exp: &str) {
        assert_eq!(format!("{:?}", value.into_value()), exp);
    }

    #[test]
    fn test_value_debug() {
        // Primitives.
        test(Value::None, "none");
        test(false, "false");
        test(12i64, "12");
        test(3.24, "3.24");
        test(Abs::pt(5.5), "5.5pt");
        test(Angle::deg(90.0), "90deg");
        test(Ratio::one() / 2.0, "50%");
        test(Ratio::new(0.3) + Length::from(Abs::cm(2.0)), "30% + 56.69pt");
        test(Fr::one() * 7.55, "7.55fr");
        test(Color::Rgba(RgbaColor::new(1, 1, 1, 0xff)), "rgb(\"#010101\")");

        // Collections.
        test("hello", r#""hello""#);
        test("\n", r#""\n""#);
        test("\\", r#""\\""#);
        test("\"", r#""\"""#);
        test(array![], "()");
        test(array![Value::None], "(none,)");
        test(array![1, 2], "(1, 2)");
        test(dict![], "(:)");
        test(dict!["one" => 1], "(one: 1)");
        test(dict!["two" => false, "one" => 1], "(two: false, one: 1)");
    }
}
