//! Operations on values.

use std::cmp::Ordering;
use std::fmt::Debug;

use ecow::eco_format;

use super::{format_str, IntoValue, Regex, Value};
use crate::diag::StrResult;
use crate::geom::{Axes, Axis, GenAlign, Length, Numeric, PartialStroke, Rel, Smart};
use Value::*;

/// Bail with a type mismatch error.
macro_rules! mismatch {
    ($fmt:expr, $($value:expr),* $(,)?) => {
        return Err(eco_format!($fmt, $($value.ty()),*))
    };
}

/// Join a value with another value.
pub fn join(lhs: Value, rhs: Value) -> StrResult<Value> {
    Ok(match (lhs, rhs) {
        (a, None) => a,
        (None, b) => b,
        (Symbol(a), Symbol(b)) => Str(format_str!("{a}{b}")),
        (Str(a), Str(b)) => Str(a + b),
        (Str(a), Symbol(b)) => Str(format_str!("{a}{b}")),
        (Symbol(a), Str(b)) => Str(format_str!("{a}{b}")),
        (Content(a), Content(b)) => Content(a + b),
        (Content(a), Symbol(b)) => Content(a + item!(text)(b.get().into())),
        (Content(a), Str(b)) => Content(a + item!(text)(b.into())),
        (Str(a), Content(b)) => Content(item!(text)(a.into()) + b),
        (Symbol(a), Content(b)) => Content(item!(text)(a.get().into()) + b),
        (Array(a), Array(b)) => Array(a + b),
        (Dict(a), Dict(b)) => Dict(a + b),
        (a, b) => mismatch!("cannot join {} with {}", a, b),
    })
}

/// Apply the unary plus operator to a value.
pub fn pos(value: Value) -> StrResult<Value> {
    Ok(match value {
        Int(v) => Int(v),
        Float(v) => Float(v),
        Length(v) => Length(v),
        Angle(v) => Angle(v),
        Ratio(v) => Ratio(v),
        Relative(v) => Relative(v),
        Fraction(v) => Fraction(v),
        v => mismatch!("cannot apply '+' to {}", v),
    })
}

/// Compute the negation of a value.
pub fn neg(value: Value) -> StrResult<Value> {
    Ok(match value {
        Int(v) => Int(v.checked_neg()?),
        Float(v) => Float(-v),
        Length(v) => Length(-v),
        Angle(v) => Angle(-v),
        Ratio(v) => Ratio(-v),
        Relative(v) => Relative(-v),
        Fraction(v) => Fraction(-v),
        v => mismatch!("cannot apply '-' to {}", v),
    })
}

/// Compute the sum of two values.
pub fn add(lhs: Value, rhs: Value) -> StrResult<Value> {
    Ok(match (lhs, rhs) {
        (a, None) => a,
        (None, b) => b,

        (Int(a), Int(b)) => Int(a.checked_add(b)?),
        (Int(a), Float(b)) => Float(a.as_float() + b),
        (Float(a), Int(b)) => Float(a + b.as_float()),
        (Float(a), Float(b)) => Float(a + b),

        (Angle(a), Angle(b)) => Angle(a + b),

        (Length(a), Length(b)) => Length(a + b),
        (Length(a), Ratio(b)) => Relative(b + a),
        (Length(a), Relative(b)) => Relative(b + a),

        (Ratio(a), Length(b)) => Relative(a + b),
        (Ratio(a), Ratio(b)) => Ratio(a + b),
        (Ratio(a), Relative(b)) => Relative(b + a),

        (Relative(a), Length(b)) => Relative(a + b),
        (Relative(a), Ratio(b)) => Relative(a + b),
        (Relative(a), Relative(b)) => Relative(a + b),

        (Fraction(a), Fraction(b)) => Fraction(a + b),

        (Symbol(a), Symbol(b)) => Str(format_str!("{a}{b}")),
        (Str(a), Str(b)) => Str(a + b),
        (Str(a), Symbol(b)) => Str(format_str!("{a}{b}")),
        (Symbol(a), Str(b)) => Str(format_str!("{a}{b}")),
        (Content(a), Content(b)) => Content(a + b),
        (Content(a), Symbol(b)) => Content(a + item!(text)(b.get().into())),
        (Content(a), Str(b)) => Content(a + item!(text)(b.into())),
        (Str(a), Content(b)) => Content(item!(text)(a.into()) + b),
        (Symbol(a), Content(b)) => Content(item!(text)(a.get().into()) + b),

        (Array(a), Array(b)) => Array(a + b),
        (Dict(a), Dict(b)) => Dict(a + b),

        (Color(color), Length(thickness)) | (Length(thickness), Color(color)) => {
            Value::dynamic(PartialStroke {
                paint: Smart::Custom(color.into()),
                thickness: Smart::Custom(thickness),
                ..PartialStroke::default()
            })
        }

        (Dyn(a), Dyn(b)) => {
            // 1D alignments can be summed into 2D alignments.
            if let (Some(&a), Some(&b)) =
                (a.downcast::<GenAlign>(), b.downcast::<GenAlign>())
            {
                if a.axis() == b.axis() {
                    return Err(eco_format!("cannot add two {:?} alignments", a.axis()));
                }

                return Ok(Value::dynamic(match a.axis() {
                    Axis::X => Axes { x: a, y: b },
                    Axis::Y => Axes { x: b, y: a },
                }));
            };

            mismatch!("cannot add {} and {}", a, b);
        }

        (a, b) => mismatch!("cannot add {} and {}", a, b),
    })
}

/// Compute the difference of two values.
pub fn sub(lhs: Value, rhs: Value) -> StrResult<Value> {
    Ok(match (lhs, rhs) {
        (Int(a), Int(b)) => Int(a.checked_sub(b)?),
        (Int(a), Float(b)) => Float(a.as_float() - b),
        (Float(a), Int(b)) => Float(a - b.as_float()),
        (Float(a), Float(b)) => Float(a - b),

        (Angle(a), Angle(b)) => Angle(a - b),

        (Length(a), Length(b)) => Length(a - b),
        (Length(a), Ratio(b)) => Relative(-b + a),
        (Length(a), Relative(b)) => Relative(-b + a),

        (Ratio(a), Length(b)) => Relative(a + -b),
        (Ratio(a), Ratio(b)) => Ratio(a - b),
        (Ratio(a), Relative(b)) => Relative(-b + a),

        (Relative(a), Length(b)) => Relative(a + -b),
        (Relative(a), Ratio(b)) => Relative(a + -b),
        (Relative(a), Relative(b)) => Relative(a - b),

        (Fraction(a), Fraction(b)) => Fraction(a - b),

        (a, b) => mismatch!("cannot subtract {1} from {0}", a, b),
    })
}

/// Compute the product of two values.
pub fn mul(lhs: Value, rhs: Value) -> StrResult<Value> {
    Ok(match (lhs, rhs) {
        (Int(a), Int(b)) => Int(a.checked_mul(b)?),
        (Int(a), Float(b)) => Float(a.as_float() * b),
        (Float(a), Int(b)) => Float(a * b.as_float()),
        (Float(a), Float(b)) => Float(a * b),

        (Length(a), Int(b)) => Length(a * b.as_float()),
        (Length(a), Float(b)) => Length(a * b),
        (Length(a), Ratio(b)) => Length(a * b.get()),
        (Int(a), Length(b)) => Length(b * a.as_float()),
        (Float(a), Length(b)) => Length(b * a),
        (Ratio(a), Length(b)) => Length(b * a.get()),

        (Angle(a), Int(b)) => Angle(a * b.as_float()),
        (Angle(a), Float(b)) => Angle(a * b),
        (Angle(a), Ratio(b)) => Angle(a * b.get()),
        (Int(a), Angle(b)) => Angle(a.as_float() * b),
        (Float(a), Angle(b)) => Angle(a * b),
        (Ratio(a), Angle(b)) => Angle(a.get() * b),

        (Ratio(a), Ratio(b)) => Ratio(a * b),
        (Ratio(a), Int(b)) => Ratio(a * b.as_float()),
        (Ratio(a), Float(b)) => Ratio(a * b),
        (Int(a), Ratio(b)) => Ratio(a.as_float() * b),
        (Float(a), Ratio(b)) => Ratio(a * b),

        (Relative(a), Int(b)) => Relative(a * b.as_float()),
        (Relative(a), Float(b)) => Relative(a * b),
        (Relative(a), Ratio(b)) => Relative(a * b.get()),
        (Int(a), Relative(b)) => Relative(a.as_float() * b),
        (Float(a), Relative(b)) => Relative(a * b),
        (Ratio(a), Relative(b)) => Relative(a.get() * b),

        (Fraction(a), Int(b)) => Fraction(a * b.as_float()),
        (Fraction(a), Float(b)) => Fraction(a * b),
        (Fraction(a), Ratio(b)) => Fraction(a * b.get()),
        (Int(a), Fraction(b)) => Fraction(a.as_float() * b),
        (Float(a), Fraction(b)) => Fraction(a * b),
        (Ratio(a), Fraction(b)) => Fraction(a.get() * b),

        (Str(a), b @ Int(_)) => Str(a.repeat(b.cast()?)?),
        (a @ Int(_), Str(b)) => Str(b.repeat(a.cast()?)?),
        (Array(a), b @ Int(_)) => Array(a.repeat(b.cast()?)?),
        (a @ Int(_), Array(b)) => Array(b.repeat(a.cast()?)?),
        (Content(a), b @ Int(_)) => Content(a.repeat(b.cast()?)),
        (a @ Int(_), Content(b)) => Content(b.repeat(a.cast()?)),

        (a, b) => mismatch!("cannot multiply {} with {}", a, b),
    })
}

/// Compute the quotient of two values.
pub fn div(lhs: Value, rhs: Value) -> StrResult<Value> {
    if is_zero(&rhs) {
        Err("cannot divide by zero")?;
    }

    Ok(match (lhs, rhs) {
        (Int(a), Int(b)) => Float(a.as_float() / b.as_float()),
        (Int(a), Float(b)) => Float(a.as_float() / b),
        (Float(a), Int(b)) => Float(a / b.as_float()),
        (Float(a), Float(b)) => Float(a / b),

        (Length(a), Int(b)) => Length(a / b.as_float()),
        (Length(a), Float(b)) => Length(a / b),
        (Length(a), Length(b)) => Float(try_div_length(a, b)?),
        (Length(a), Relative(b)) if b.rel.is_zero() => Float(try_div_length(a, b.abs)?),

        (Angle(a), Int(b)) => Angle(a / b.as_float()),
        (Angle(a), Float(b)) => Angle(a / b),
        (Angle(a), Angle(b)) => Float((a / b).into()),

        (Ratio(a), Int(b)) => Ratio(a / b.as_float()),
        (Ratio(a), Float(b)) => Ratio(a / b),
        (Ratio(a), Ratio(b)) => Float((a / b).into()),
        (Ratio(a), Relative(b)) if b.abs.is_zero() => Float((a / b.rel).into()),

        (Relative(a), Int(b)) => Relative(a / b.as_float()),
        (Relative(a), Float(b)) => Relative(a / b),
        (Relative(a), Length(b)) if a.rel.is_zero() => Float(try_div_length(a.abs, b)?),
        (Relative(a), Ratio(b)) if a.abs.is_zero() => Float((a.rel / b).into()),
        (Relative(a), Relative(b)) => Float(try_div_relative(a, b)?),

        (Fraction(a), Int(b)) => Fraction(a / b.as_float()),
        (Fraction(a), Float(b)) => Fraction(a / b),
        (Fraction(a), Fraction(b)) => Float((a / b).into()),

        (a, b) => mismatch!("cannot divide {} by {}", a, b),
    })
}

/// Whether a value is a numeric zero.
fn is_zero(v: &Value) -> bool {
    match *v {
        Int(v) => v.as_i64() == 0,
        Float(v) => v.as_f64() == 0.0,
        Length(v) => v.is_zero(),
        Angle(v) => v.is_zero(),
        Ratio(v) => v.is_zero(),
        Relative(v) => v.is_zero(),
        Fraction(v) => v.is_zero(),
        _ => false,
    }
}

/// Try to divide two lengths.
fn try_div_length(a: Length, b: Length) -> StrResult<super::Float> {
    a.try_div(b)
        .map(Into::into)
        .ok_or_else(|| "cannot divide these two lengths".into())
}

/// Try to divide two relative lengths.
fn try_div_relative(a: Rel<Length>, b: Rel<Length>) -> StrResult<super::Float> {
    a.try_div(b)
        .map(Into::into)
        .ok_or_else(|| "cannot divide these two relative lengths".into())
}

/// Compute the logical "not" of a value.
pub fn not(value: Value) -> StrResult<Value> {
    match value {
        Bool(b) => Ok(Bool(!b)),
        v => mismatch!("cannot apply 'not' to {}", v),
    }
}

/// Compute the logical "and" of two values.
pub fn and(lhs: Value, rhs: Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Bool(a), Bool(b)) => Ok(Bool(a & b)),
        (a, b) => mismatch!("cannot apply 'and' to {} and {}", a, b),
    }
}

/// Compute the logical "or" of two values.
pub fn or(lhs: Value, rhs: Value) -> StrResult<Value> {
    match (lhs, rhs) {
        (Bool(a), Bool(b)) => Ok(Bool(a | b)),
        (a, b) => mismatch!("cannot apply 'or' to {} and {}", a, b),
    }
}

/// Compute whether two values are equal.
pub fn eq(lhs: Value, rhs: Value) -> StrResult<Value> {
    Ok(equal(&lhs, &rhs).into_value())
}

/// Compute whether two values are unequal.
pub fn neq(lhs: Value, rhs: Value) -> StrResult<Value> {
    Ok((!equal(&lhs, &rhs)).into_value())
}

macro_rules! comparison {
    ($name:ident, $op:tt, $($pat:tt)*) => {
        /// Compute how a value compares with another value.
        pub fn $name(lhs: Value, rhs: Value) -> StrResult<Value> {
            let ordering = compare(&lhs, &rhs)?;
            Ok(matches!(ordering, $($pat)*).into_value())
        }
    };
}

comparison!(lt, "<", Ordering::Less);
comparison!(leq, "<=", Ordering::Less | Ordering::Equal);
comparison!(gt, ">", Ordering::Greater);
comparison!(geq, ">=", Ordering::Greater | Ordering::Equal);

/// Determine whether two values are equal.
pub fn equal(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        // Compare reflexively.
        (None, None) => true,
        (Auto, Auto) => true,
        (Bool(a), Bool(b)) => a == b,
        (Int(a), Int(b)) => a == b,
        (Float(a), Float(b)) => a == b,
        (Length(a), Length(b)) => a == b,
        (Angle(a), Angle(b)) => a == b,
        (Ratio(a), Ratio(b)) => a == b,
        (Relative(a), Relative(b)) => a == b,
        (Fraction(a), Fraction(b)) => a == b,
        (Color(a), Color(b)) => a == b,
        (Symbol(a), Symbol(b)) => a == b,
        (Str(a), Str(b)) => a == b,
        (Label(a), Label(b)) => a == b,
        (Content(a), Content(b)) => a == b,
        (Array(a), Array(b)) => a == b,
        (Dict(a), Dict(b)) => a == b,
        (Func(a), Func(b)) => a == b,
        (Args(a), Args(b)) => a == b,
        (Type(a), Type(b)) => a == b,
        (Module(a), Module(b)) => a == b,
        (Dyn(a), Dyn(b)) => a == b,

        // Some technically different things should compare equal.
        (&Int(a), &Float(b)) => a.as_float() == b,
        (&Float(a), &Int(b)) => a == b.as_float(),
        (&Length(a), &Relative(b)) => a == b.abs && b.rel.is_zero(),
        (&Ratio(a), &Relative(b)) => a == b.rel && b.abs.is_zero(),
        (&Relative(a), &Length(b)) => a.abs == b && a.rel.is_zero(),
        (&Relative(a), &Ratio(b)) => a.rel == b && a.abs.is_zero(),

        // Temporary for migration.
        (Type(a), Str(b)) | (Str(b), Type(a)) => a.long() == b.as_str(),

        _ => false,
    }
}

/// Compare two values.
pub fn compare(lhs: &Value, rhs: &Value) -> StrResult<Ordering> {
    Ok(match (lhs, rhs) {
        (Bool(a), Bool(b)) => a.cmp(b),
        (Int(a), Int(b)) => a.cmp(b),
        (Float(a), Float(b)) => try_cmp_values(a, b)?,
        (Length(a), Length(b)) => try_cmp_values(a, b)?,
        (Angle(a), Angle(b)) => a.cmp(b),
        (Ratio(a), Ratio(b)) => a.cmp(b),
        (Relative(a), Relative(b)) => try_cmp_values(a, b)?,
        (Fraction(a), Fraction(b)) => a.cmp(b),
        (Str(a), Str(b)) => a.cmp(b),

        // Some technically different things should be comparable.
        (Int(a), Float(b)) => try_cmp_values(&a.as_float(), b)?,
        (Float(a), Int(b)) => try_cmp_values(a, &b.as_float())?,
        (Length(a), Relative(b)) if b.rel.is_zero() => try_cmp_values(a, &b.abs)?,
        (Ratio(a), Relative(b)) if b.abs.is_zero() => a.cmp(&b.rel),
        (Relative(a), Length(b)) if a.rel.is_zero() => try_cmp_values(&a.abs, b)?,
        (Relative(a), Ratio(b)) if a.abs.is_zero() => a.rel.cmp(b),

        _ => mismatch!("cannot compare {} and {}", lhs, rhs),
    })
}

/// Try to compare two values.
fn try_cmp_values<T: PartialOrd + Debug>(a: &T, b: &T) -> StrResult<Ordering> {
    a.partial_cmp(b)
        .ok_or_else(|| eco_format!("cannot compare {:?} with {:?}", a, b))
}

/// Test whether one value is "in" another one.
pub fn in_(lhs: Value, rhs: Value) -> StrResult<Value> {
    if let Some(b) = contains(&lhs, &rhs) {
        Ok(b.into_value())
    } else {
        mismatch!("cannot apply 'in' to {} and {}", lhs, rhs)
    }
}

/// Test whether one value is "not in" another one.
pub fn not_in(lhs: Value, rhs: Value) -> StrResult<Value> {
    if let Some(b) = contains(&lhs, &rhs) {
        Ok((!b).into_value())
    } else {
        mismatch!("cannot apply 'not in' to {} and {}", lhs, rhs)
    }
}

/// Test for containment.
pub fn contains(lhs: &Value, rhs: &Value) -> Option<bool> {
    match (lhs, rhs) {
        (Str(a), Str(b)) => Some(b.as_str().contains(a.as_str())),
        (Dyn(a), Str(b)) => a.downcast::<Regex>().map(|regex| regex.is_match(b)),
        (Str(a), Dict(b)) => Some(b.contains(a)),
        (a, Array(b)) => Some(b.contains(a.clone())),
        _ => Option::None,
    }
}
