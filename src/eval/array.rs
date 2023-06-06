use std::cmp::Ordering;
use std::fmt::{self, Debug, Formatter};
use std::num::NonZeroI64;
use std::ops::{Add, AddAssign};

use ecow::{eco_format, EcoString, EcoVec};

use super::{
    func, ops, ty, Args, CastInfo, FromValue, Func, IntoValue, Reflect, Value, Vm,
};
use crate::diag::{At, SourceResult, StrResult};
use crate::syntax::Span;
use crate::util::pretty_array_like;

/// Create a new [`Array`] from values.
#[macro_export]
#[doc(hidden)]
macro_rules! __array {
    ($value:expr; $count:expr) => {
        $crate::eval::Array::from($crate::eval::eco_vec![
            $crate::eval::IntoValue::into_value($value);
            $count
        ])
    };

    ($($value:expr),* $(,)?) => {
        $crate::eval::Array::from($crate::eval::eco_vec![$(
            $crate::eval::IntoValue::into_value($value)
        ),*])
    };
}

#[doc(inline)]
pub use crate::__array as array;
use crate::eval::ops::{add, mul};
#[doc(hidden)]
pub use ecow::eco_vec;

/// A sequence of values.
///
/// You can construct an array by enclosing a comma-separated sequence of values
/// in parentheses. The values do not have to be of the same type.
///
/// You can access and update array items with the `.at()` method. Indices are
/// zero-based and negative indices wrap around to the end of the array. You can
/// iterate over an array using a [for loop]($scripting/#loops). Arrays can be
/// added together with the `+` operator, [joined together]($scripting/#blocks)
/// and multiplied with integers.
///
/// **Note:** An array of length one needs a trailing comma, as in `{(1,)}`.
/// This is to disambiguate from a simple parenthesized expressions like `{(1 +
/// 2) * 3}`. An empty array is written as `{()}`.
///
/// ## Example { #example }
/// ```example
/// #let values = (1, 7, 4, -3, 2)
///
/// #values.at(0) \
/// #(values.at(0) = 3)
/// #values.at(-1) \
/// #values.find(calc.even) \
/// #values.filter(calc.odd) \
/// #values.map(calc.abs) \
/// #values.rev() \
/// #(1, (2, 3)).flatten() \
/// #(("A", "B", "C")
///     .join(", ", last: " and "))
/// ```
///
/// Display: Array
/// Category: foundations
#[ty("array")]
#[scope({
    scope.define("len", Array::len_func());
    scope.define("first", Array::first_func());
    scope.define("last", Array::last_func());
    scope.define("at", Array::at_func());
    scope.define("push", Array::push_func());
    scope.define("pop", Array::pop_func());
    scope.define("insert", Array::insert_func());
    scope.define("remove", Array::remove_func());
    scope.define("slice", Array::slice_func());
    scope.define("contains", Array::contains_func());
    scope.define("find", Array::find_func());
    scope.define("position", Array::position_func());
    scope.define("range", Array::range_func());
    scope.define("filter", Array::filter_func());
    scope.define("map", Array::map_func());
    scope.define("enumerate", Array::enumerate_func());
    scope.define("zip", Array::zip_func());
    scope.define("fold", Array::fold_func());
    scope.define("sum", Array::sum_func());
    scope.define("product", Array::product_func());
    scope.define("any", Array::any_func());
    scope.define("all", Array::all_func());
    scope.define("flatten", Array::flatten_func());
    scope.define("rev", Array::rev_func());
    scope.define("split", Array::split_func());
    scope.define("join", Array::join_func());
    scope.define("sorted", Array::sorted_func());
    scope
})]
#[derive(Default, Clone, PartialEq, Hash)]
pub struct Array(EcoVec<Value>);

impl Array {
    /// Create a new, empty array.
    pub fn new() -> Self {
        Self::default()
    }

    /// Return `true` if the length is 0.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Repeat this array `n` times.
    pub fn repeat(&self, n: usize) -> StrResult<Self> {
        let count = self
            .0
            .len()
            .checked_mul(n)
            .ok_or_else(|| eco_format!("cannot repeat this array {n} times"))?;
        Ok(self.iter().cloned().cycle().take(count).collect())
    }

    /// Extract a slice of the whole array.
    pub fn as_slice(&self) -> &[Value] {
        self.0.as_slice()
    }

    /// Iterate over references to the contained values.
    pub fn iter(&self) -> std::slice::Iter<Value> {
        self.0.iter()
    }

    /// Resolve an index.
    fn locate(&self, index: i64) -> Option<usize> {
        usize::try_from(if index >= 0 {
            index
        } else {
            (self.len() as i64).checked_add(index)?
        })
        .ok()
    }
}

impl Array {
    /// The number of values in the array.
    ///
    /// Display: Length
    /// Category: foundations
    #[func(Array)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns the first item in the array. May be used on the left-hand side
    /// of an assignment. Fails with an error if the array is empty.
    ///
    /// Display: First
    /// Category: foundations
    #[func(Array)]
    pub fn first(&self) -> StrResult<Value> {
        self.0.first().cloned().ok_or_else(array_is_empty)
    }

    /// Mutably borrow the first value in the array.
    pub fn first_mut(&mut self) -> StrResult<&mut Value> {
        self.0.make_mut().first_mut().ok_or_else(array_is_empty)
    }

    /// Returns the last item in the array. May be used on the left-hand side of
    /// an assignment. Fails with an error if the array is empty.
    ///
    /// Display: Last
    /// Category: foundations
    #[func(Array)]
    pub fn last(&self) -> StrResult<Value> {
        self.0.last().cloned().ok_or_else(array_is_empty)
    }

    /// Mutably borrow the last value in the array.
    pub fn last_mut(&mut self) -> StrResult<&mut Value> {
        self.0.make_mut().last_mut().ok_or_else(array_is_empty)
    }

    /// Borrow the value at the given index.
    ///
    /// Returns the item at the specified index in the array. May be used on the
    /// left-hand side of an assignment. Returns the default value if the index
    /// is out of bounds or fails with an error if no default value was
    /// specified.
    ///
    /// Display: At
    /// Category: foundations
    #[func(Array)]
    pub fn at(
        &self,
        /// The index at which to retrieve the item. If negative, indexes from
        /// the back.
        index: i64,
        /// A default value to return if the index is out of bounds.
        #[named]
        default: Option<Value>,
    ) -> StrResult<Value> {
        self.locate(index)
            .and_then(|i| self.0.get(i).cloned())
            .or(default)
            .ok_or_else(|| out_of_bounds_no_default(index, self.len()))
    }

    /// Mutably borrow the value at the given index.
    pub fn at_mut(&mut self, index: i64) -> StrResult<&mut Value> {
        let len = self.len();
        self.locate(index)
            .and_then(move |i| self.0.make_mut().get_mut(i))
            .ok_or_else(|| out_of_bounds_no_default(index, len))
    }

    /// Add a value to the end of the array.
    ///
    /// Display: Push
    /// Category: foundations
    #[func(Array)]
    pub fn push(
        &mut self,
        /// The value to insert at the end of the array.
        value: Value,
    ) {
        self.0.push(value);
    }

    /// Remove the last item from the array and return it. Fails with an error
    /// if the array is empty.
    ///
    /// Display: Pop
    /// Category: foundations
    #[func(Array)]
    pub fn pop(&mut self) -> StrResult<Value> {
        self.0.pop().ok_or_else(array_is_empty)
    }

    /// Insert a value into the array at the specified index.
    /// Fails with an error if the index is out of bounds.
    ///
    /// Display: Insert
    /// Category: foundations
    #[func(Array)]
    pub fn insert(
        &mut self,
        /// The index at which to insert the item. If negative, indexes from
        /// the back.
        index: i64,
        /// The value to insert into the array.
        value: Value,
    ) -> StrResult<()> {
        let len = self.len();
        let i = self
            .locate(index)
            .filter(|&i| i <= self.0.len())
            .ok_or_else(|| out_of_bounds(index, len))?;
        self.0.insert(i, value);
        Ok(())
    }

    /// Remove the value at the specified index from the array and return it.
    ///
    /// Display: Remove
    /// Category: foundations
    #[func(Array)]
    pub fn remove(
        &mut self,
        /// The index at which to remove the item. If negative, indexes from
        /// the back.
        index: i64,
    ) -> StrResult<Value> {
        let len = self.len();
        let i = self
            .locate(index)
            .filter(|&i| i < self.0.len())
            .ok_or_else(|| out_of_bounds(index, len))?;

        Ok(self.0.remove(i))
    }

    /// Extract a subslice of the array. Fails with an error if the start or
    /// index is out of bounds.
    ///
    /// Display: Slice
    /// Category: foundations
    #[func(Array)]
    pub fn slice(
        &self,
        /// The start index (inclusive). If negative, indexes from the back.
        start: i64,
        /// The end index (exclusive). If omitted, the whole slice until the end
        /// of the array is extracted. If negative, indexes from the back.
        #[default]
        end: Option<i64>,
        /// The number of items to extract. This is equivalent to passing `start
        /// + count` as the `end` position. Mutually exclusive with `end`.
        #[named]
        count: Option<i64>,
    ) -> StrResult<Array> {
        let len = self.len();
        let end = end.or(count.map(|c| start + c)).unwrap_or(len as i64);

        let start = self
            .locate(start)
            .filter(|&start| start <= len)
            .ok_or_else(|| out_of_bounds(start, len))?;

        let end = self
            .locate(end)
            .filter(|&end| end <= len)
            .ok_or_else(|| out_of_bounds(end, len))?
            .max(start);

        Ok(self.0[start..end].into())
    }

    /// Whether the array contains the specified value.
    ///
    /// This method also has dedicated syntax: You can write `{2 in (1, 2, 3)}`
    /// instead of `{(1, 2, 3).contains(2)}`.
    ///
    /// Display: Contains
    /// Category: foundations
    #[func(Array)]
    pub fn contains(
        &self,
        /// The value to search for.
        value: Value,
    ) -> bool {
        self.0.contains(&value)
    }

    /// Searches for an item for which the given function returns `{true}` and
    /// returns the first match or `{none}` if there is no match.
    ///
    /// Display: Find
    /// Category: foundations
    #[func(Array)]
    pub fn find(
        &self,
        /// The function to apply to each item. Must return a boolean.
        searcher: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<Option<Value>> {
        for item in self.iter() {
            let args = Args::new(searcher.span(), [item.clone()]);
            if searcher.call_vm(vm, args)?.cast::<bool>().at(searcher.span())? {
                return Ok(Some(item.clone()));
            }
        }
        Ok(None)
    }

    /// Searches for an item for which the given function returns `{true}` and
    /// returns the index of the first match or `{none}` if there is no match.
    ///
    /// Display: Position
    /// Category: foundations
    #[func(Array)]
    pub fn position(
        &self,
        /// The function to apply to each item. Must return a boolean.
        searcher: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<Option<i64>> {
        for (i, item) in self.iter().enumerate() {
            let args = Args::new(searcher.span(), [item.clone()]);
            if searcher.call_vm(vm, args)?.cast::<bool>().at(searcher.span())? {
                return Ok(Some(i as i64));
            }
        }

        Ok(None)
    }

    /// Create an array consisting of a sequence of numbers.
    ///
    /// If you pass just one positional parameter, it is interpreted as the
    /// `end` of the range. If you pass two, they describe the `start` and `end`
    /// of the range.
    ///
    /// ## Example { #example }
    /// ```example
    /// #range(5) \
    /// #range(2, 5) \
    /// #range(20, step: 4) \
    /// #range(21, step: 4) \
    /// #range(5, 2, step: -1)
    /// ```
    ///
    /// Display: Range
    /// Category: foundations
    #[func(Array)]
    pub fn range(
        /// The start of the range (inclusive).
        #[external]
        #[default]
        start: i64,
        /// The end of the range (exclusive).
        #[external]
        end: i64,
        /// The distance between the generated numbers.
        #[named]
        #[default(NonZeroI64::new(1).unwrap())]
        step: NonZeroI64,
        /// The real arguments (the other arguments are just for the docs, this
        /// function is a bit involved, so we parse the arguments manually).
        args: Args,
    ) -> SourceResult<Array> {
        let mut args = args;
        let first = args.expect::<i64>("end")?;
        let (start, end) = match args.eat::<i64>()? {
            Some(second) => (first, second),
            None => (0, first),
        };
        args.finish()?;

        let step = step.get();

        let mut x = start;
        let mut array = Self::new();

        while x.cmp(&end) == 0.cmp(&step) {
            array.push(x.into_value());
            x += step;
        }

        Ok(array)
    }

    /// Produces a new array with only the items from the original one for which
    /// the given function returns true.
    ///
    /// Display: Filter
    /// Category: foundations
    #[func(Array)]
    pub fn filter(
        &self,
        /// The function to apply to each item. Must return a boolean.
        test: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<Array> {
        let mut kept = EcoVec::new();
        for item in self.iter() {
            let args = Args::new(test.span(), [item.clone()]);
            if test.call_vm(vm, args)?.cast::<bool>().at(test.span())? {
                kept.push(item.clone())
            }
        }
        Ok(kept.into())
    }

    /// Produces a new array in which all items from the original one were
    /// transformed with the given function.
    ///
    /// Display: Map
    /// Category: foundations
    #[func(Array)]
    pub fn map(
        &self,
        /// The function to apply to each item.
        mapper: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<Array> {
        self.iter()
            .map(|item| {
                let args = Args::new(mapper.span(), [item.clone()]);
                mapper.call_vm(vm, args)
            })
            .collect()
    }

    /// Returns a new array with the values alongside their indices.
    ///
    /// The returned array consists of `(index, value)` pairs in the form of
    /// length-2 arrays. These can be [destructured]($scripting/#bindings) with
    /// a let binding or for loop.
    ///
    /// Display: Enumerate
    /// Category: foundations
    #[func(Array)]
    pub fn enumerate(&self) -> Array {
        self.iter()
            .enumerate()
            .map(|(i, value)| Value::Array(array![i, value.clone()]))
            .collect()
    }

    /// Zips the array with another array. If the two arrays are of unequal
    /// length, it will only zip up until the last element of the smaller array
    /// and the remaining elements will be ignored. The return value is an array
    /// where each element is yet another array of size 2.
    ///
    /// Display: Zip
    /// Category: foundations
    #[func(Array)]
    pub fn zip(
        &self,
        /// The other array which should be zipped with the current one.
        other: Array,
    ) -> Array {
        self.iter()
            .zip(other)
            .map(|(first, second)| Value::Array(array![first.clone(), second]))
            .collect()
    }

    /// Folds all items into a single value using an accumulator function.
    ///
    /// Display: Fold
    /// Category: foundations
    #[func(Array)]
    pub fn fold(
        &self,
        /// The initial value to start with.
        init: Value,
        /// The folding function. Must have two parameters: One for the
        /// accumulated value and one for an item.
        folder: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<Value> {
        let mut acc = init;
        for item in self.iter() {
            let args = Args::new(folder.span(), [acc, item.clone()]);
            acc = folder.call_vm(vm, args)?;
        }
        Ok(acc)
    }

    /// Sums all items (works for all types that can be added).
    ///
    /// Display: Sum
    /// Category: foundations
    #[func(Array)]
    pub fn sum(
        &self,
        /// What to return if the array is empty. Must be set if the array can
        /// be empty.
        #[default]
        default: Option<Value>,
    ) -> StrResult<Value> {
        let mut acc = self
            .0
            .first()
            .cloned()
            .or(default)
            .ok_or("cannot calculate sum of empty array with no default")?;
        for i in self.iter().skip(1) {
            acc = add(acc, i.clone())?;
        }
        Ok(acc)
    }

    /// Calculates the product all items (works for all types that can be
    /// multiplied).
    ///
    /// Display: Product
    /// Category: foundations
    #[func(Array)]
    pub fn product(
        &self,
        /// What to return if the array is empty. Must be set if the array can
        /// be empty.
        #[default]
        default: Option<Value>,
    ) -> StrResult<Value> {
        let mut acc = self
            .0
            .first()
            .cloned()
            .or(default)
            .ok_or("cannot calculate product of empty array with no default")?;
        for i in self.iter().skip(1) {
            acc = mul(acc, i.clone())?;
        }
        Ok(acc)
    }

    /// Whether the given function returns `{true}` for any item in the array.
    ///
    /// Display: Any
    /// Category: foundations
    #[func(Array)]
    pub fn any(
        &self,
        /// The function to apply to each item. Must return a boolean.
        test: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<bool> {
        for item in self.iter() {
            let args = Args::new(test.span(), [item.clone()]);
            if test.call_vm(vm, args)?.cast::<bool>().at(test.span())? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    /// Whether the given function returns `{true}` for all items in the array.
    ///
    /// Display: All
    /// Category: foundations
    #[func(Array)]
    pub fn all(
        &self,
        /// The function to apply to each item. Must return a boolean.
        test: Func,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> SourceResult<bool> {
        for item in self.iter() {
            let args = Args::new(test.span(), [item.clone()]);
            if !test.call_vm(vm, args)?.cast::<bool>().at(test.span())? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Combine all nested arrays into a single flat one.
    ///
    /// Display: Flatten
    /// Category: foundations
    #[func(Array)]
    pub fn flatten(&self) -> Array {
        let mut flat = EcoVec::with_capacity(self.0.len());
        for item in self.iter() {
            if let Value::Array(nested) = item {
                flat.extend(nested.flatten().into_iter());
            } else {
                flat.push(item.clone());
            }
        }
        flat.into()
    }

    /// Return a new array with the same items, but in reverse order.
    ///
    /// Display: Reverse
    /// Category: foundations
    #[func(Array)]
    pub fn rev(&self) -> Array {
        self.0.iter().cloned().rev().collect()
    }

    /// Split the array at occurrences of the specified value.
    ///
    /// Display: Split
    /// Category: foundations
    #[func(Array)]
    pub fn split(
        &self,
        /// The value to split at.
        at: Value,
    ) -> Array {
        self.as_slice()
            .split(|value| *value == at)
            .map(|subslice| Value::Array(subslice.iter().cloned().collect()))
            .collect()
    }

    /// Combine all items in the array into one.
    ///
    /// Display: Join
    /// Category: foundations
    #[func(Array)]
    pub fn join(
        &self,
        /// A value to insert between each item of the array.
        #[default]
        separator: Option<Value>,
        /// An alternative separator between the last two items
        #[named]
        last: Option<Value>,
    ) -> StrResult<Value> {
        let len = self.0.len();
        let separator = separator.unwrap_or(Value::None);

        let mut last = last;
        let mut result = Value::None;
        for (i, value) in self.iter().cloned().enumerate() {
            if i > 0 {
                if i + 1 == len && last.is_some() {
                    result = ops::join(result, last.take().unwrap())?;
                } else {
                    result = ops::join(result, separator.clone())?;
                }
            }

            result = ops::join(result, value)?;
        }

        Ok(result)
    }

    /// Return a new array with the same items, but sorted.
    ///
    /// Display: Sorted
    /// Category: foundations
    #[func(Array)]
    pub fn sorted(
        &self,
        /// If given, applies this function to the elements in the array to
        /// determine the keys to sort by.
        #[named]
        key: Option<Func>,
        /// The virtual machine.
        vm: &mut Vm,
        /// The callsite span.
        span: Span,
    ) -> SourceResult<Array> {
        let mut result = Ok(());
        let mut vec = self.0.clone();
        let mut key_of = |x: Value| match &key {
            // NOTE: We are relying on `comemo`'s memoization of function
            // evaluation to not excessively reevaluate the `key`.
            Some(f) => f.call_vm(vm, Args::new(f.span(), [x])),
            None => Ok(x),
        };
        vec.make_mut().sort_by(|a, b| {
            // Until we get `try` blocks :)
            match (key_of(a.clone()), key_of(b.clone())) {
                (Ok(a), Ok(b)) => {
                    typst::eval::ops::compare(&a, &b).unwrap_or_else(|err| {
                        if result.is_ok() {
                            result = Err(err).at(span);
                        }
                        Ordering::Equal
                    })
                }
                (Err(e), _) | (_, Err(e)) => {
                    if result.is_ok() {
                        result = Err(e);
                    }
                    Ordering::Equal
                }
            }
        });
        result.map(|_| vec.into())
    }
}

impl Debug for Array {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let pieces: Vec<_> = self.iter().map(|value| eco_format!("{value:?}")).collect();
        f.write_str(&pretty_array_like(&pieces, self.len() == 1))
    }
}

impl Add for Array {
    type Output = Self;

    fn add(mut self, rhs: Array) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Array {
    fn add_assign(&mut self, rhs: Array) {
        self.0.extend(rhs.0);
    }
}

impl Extend<Value> for Array {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl FromIterator<Value> for Array {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for Array {
    type Item = Value;
    type IntoIter = ecow::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Array {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl From<EcoVec<Value>> for Array {
    fn from(v: EcoVec<Value>) -> Self {
        Array(v)
    }
}

impl From<&[Value]> for Array {
    fn from(v: &[Value]) -> Self {
        Array(v.into())
    }
}

impl<T> Reflect for Vec<T> {
    fn describe() -> CastInfo {
        Array::describe()
    }

    fn castable(value: &Value) -> bool {
        Array::castable(value)
    }
}

impl<T: IntoValue> IntoValue for Vec<T> {
    fn into_value(self) -> Value {
        Value::Array(self.into_iter().map(IntoValue::into_value).collect())
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: Value) -> StrResult<Self> {
        value.cast::<Array>()?.into_iter().map(Value::cast).collect()
    }
}

/// The error message when the array is empty.
#[cold]
fn array_is_empty() -> EcoString {
    "array is empty".into()
}

/// The out of bounds access error message.
#[cold]
fn out_of_bounds(index: i64, len: usize) -> EcoString {
    eco_format!("array index out of bounds (index: {index}, len: {len})")
}

/// The out of bounds access error message when no default value was given.
#[cold]
fn out_of_bounds_no_default(index: i64, len: usize) -> EcoString {
    eco_format!(
        "array index out of bounds (index: {index}, len: {len}) \
         and no default value was specified",
    )
}
