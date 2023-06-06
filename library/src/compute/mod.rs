//! Computational functions.

pub mod calc;
mod data;
mod foundations;

use typst::eval::Datetime;
use typst::eval::Module;
use typst::eval::Regex;

pub use self::data::*;
pub use self::foundations::*;

use crate::prelude::*;

/// Hook up all compute definitions.
pub(super) fn define(global: &mut Scope) {
    global.define("bool", Type::of::<Bool>());
    global.define("int", Type::of::<Int>());
    global.define("float", Type::of::<Float>());
    global.define("str", Type::of::<Str>());
    global.define("content", Type::of::<Content>());
    global.define("array", Type::of::<Array>());
    global.define("dict", Type::of::<Dict>());
    global.define("func", Type::of::<Func>());
    global.define("args", Type::of::<Args>());
    global.define("module", Type::of::<Module>());
    global.define("type", Type::of::<Type>());
    global.define("datetime", Type::of::<Datetime>());
    global.define("regex", Type::of::<Regex>());
    global.define("repr", repr_func());
    global.define("panic", panic_func());
    global.define("eval", eval_func());
    global.define("assert", assert_func());
    global.define("read", read_func());
    global.define("csv", csv_func());
    global.define("json", json_func());
    global.define("toml", toml_func());
    global.define("yaml", yaml_func());
    global.define("xml", xml_func());
    global.define("calc", calc::module());

    // Prelude.
    global.define("range", Array::range_func());
}
