use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

use ecow::{eco_format, EcoString, EcoVec};
use time::error::{Format, InvalidFormatDescription};
use time::{format_description, Month, PrimitiveDateTime};

use crate::diag::{bail, StrResult};
use crate::eval::{cast, func, ty, Vm};
use crate::geom::Smart;
use crate::util::pretty_array_like;
use crate::World;

/// Represents a date, a time, or a combination of both.
///
/// Can be created by either specifying a custom datetime using this type's
/// constructor function or getting the current date with
/// [`datetime.today`]($type/datetime.today).
///
/// ## Example { #example }
/// ```example
/// #let date = datetime(
///   year: 2020,
///   month: 10,
///   day: 4,
/// )
///
/// #date.display() \
/// #date.display("y:[year repr:last_two]")
///
/// #let time = datetime(
///   hour: 18,
///   minute: 2,
///   second: 23,
/// )
///
/// #time.display() \
/// #time.display("h:[hour repr:12][period]")
/// ```
///
/// ## Format { #format }
/// You can specify a customized formatting using the `display` method. The
/// format of a datetime is specified by providing _components_ with a specified
/// number of _modifiers_. A component represents a certain part of the datetime
/// that you want to display, and with the help of modifiers you can define how
/// you want to display that component. In order to display a component, you
/// wrap the name of the component in square brackets (e.g. `[year]` will
/// display the year). In order to add modifiers, you add a space after the
/// component name followed by the name of the modifier, a colon and the value
/// of the modifier (e.g. `[month repr:short]` will display the short
/// representation of the month).
///
/// The possible combination of components and their respective modifiers is as
/// follows:
///
/// - `year`: Displays the year of the datetime.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     year is padded.
///   - `repr` Can be either `full` in which case the full year is displayed or
///     `last_two` in which case only the last two digits are displayed.
///   - `sign`: Can be either `automatic` or `mandatory`. Specifies when the
///     sign should be displayed.
/// - `month`: Displays the month of the datetime.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     month is padded.
///   - `repr`: Can be either `numerical`, `long` or `short`. Specifies if the
///     month should be displayed as a number or a word. Unfortunately, when
///     choosing the word representation, it can currently only display the
///     English version. In the future, it is planned to support localization.
/// - `day`: Displays the day of the datetime.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     day is padded.
/// - `week_number`: Displays the week number of the datetime.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     week number is padded.
///   - `repr`: Can be either `ISO`, `sunday` or `monday`. In the case of `ISO`,
///      week numbers are between 1 and 53, while the other ones are between 0
///      and 53.
/// - `weekday`: Displays the weekday of the date.
///   - `repr` Can be either `long`, `short`, `sunday` or `monday`. In the case
///     of `long` and `short`, the corresponding English name will be displayed
///     (same as for the month, other languages are currently not supported). In
///     the case of `sunday` and `monday`, the numerical value will be displayed
///     (assuming Sunday and Monday as the first day of the week, respectively).
///   - `one_indexed`: Can be either `true` or `false`. Defines whether the
///     numerical representation of the week starts with 0 or 1.
/// - `hour`: Displays the hour of the date.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     hour is padded.
///   - `repr`: Can be either `24` or `12`. Changes whether the hour is
///     displayed in the 24-hour or 12-hour format.
/// - `period`: The AM/PM part of the hour
///   - `case`: Can be `lower` to display it in lower case and `upper` to
///     display it in upper case.
/// - `minute`: Displays the minute of the date.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     minute is padded.
/// - `second`: Displays the second of the date.
///   - `padding`: Can be either `zero`, `space` or `none`. Specifies how the
///     second is padded.
///
/// Keep in mind that not always all components can be used. For example, if you
/// create a new datetime with `#datetime(year: 2023, month: 10, day: 13)`, it
/// will be stored as a plain date internally, meaning that you cannot use
/// components such as `hour` or `minute`, which would only work on datetimes
/// that have a specified time.
///
/// Display: Datetime
/// Category: foundations
#[ty("datetime")]
#[constructor(Datetime::new_func())]
#[scope(
    scope.define("today", Datetime::today_func());
    scope.define("display", Datetime::display_func());
    scope.define("year", Datetime::year_func());
    scope.define("month", Datetime::month_func());
    scope.define("weekday", Datetime::weekday_func());
    scope.define("day", Datetime::day_func());
    scope.define("hour", Datetime::hour_func());
    scope.define("minute", Datetime::minute_func());
    scope.define("second", Datetime::second_func());
    scope
)]
#[derive(Clone, Copy, PartialEq, Hash)]
pub enum Datetime {
    /// Representation as a date.
    Date(time::Date),
    /// Representation as a time.
    Datetime(time::PrimitiveDateTime),
    /// Representation as a combination of date and time.
    Time(time::Time),
}

impl Datetime {
    /// Create a datetime from year, month, and day.
    pub fn from_ymd(year: i32, month: u8, day: u8) -> Option<Self> {
        Some(Datetime::Date(
            time::Date::from_calendar_date(year, time::Month::try_from(month).ok()?, day)
                .ok()?,
        ))
    }

    /// Create a datetime from hour, minute, and second.
    pub fn from_hms(hour: u8, minute: u8, second: u8) -> Option<Self> {
        Some(Datetime::Time(time::Time::from_hms(hour, minute, second).ok()?))
    }

    /// Create a datetime from day and time.
    pub fn from_ymd_hms(
        year: i32,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
    ) -> Option<Self> {
        let date =
            time::Date::from_calendar_date(year, time::Month::try_from(month).ok()?, day)
                .ok()?;
        let time = time::Time::from_hms(hour, minute, second).ok()?;
        Some(Datetime::Datetime(PrimitiveDateTime::new(date, time)))
    }

    /// Create a new datetime.
    ///
    /// You can specify the [datetime]($type/datetime) using a year, month, day,
    /// hour, minute, and second.
    ///
    /// ## Example
    /// ```example
    /// #datetime(
    ///   year: 2012,
    ///   month: 8,
    ///   day: 3,
    /// ).display()
    /// ```
    ///
    /// ## Format
    /// _Note_: Depending on which components of the datetime you specify, Typst
    /// will store it in one of the following three ways:
    /// * If you specify year, month and day, Typst will store just a date.
    /// * If you specify hour, minute and second, Typst will store just a time.
    /// * If you specify all of year, month, day, hour, minute and second, Typst
    ///   will store a full datetime.
    ///
    /// Depending on how it is stored, the [`display`]($type/datetime.display)
    /// method will choose a different formatting by default.
    ///
    /// Display: Construct
    /// Category: foundations
    #[func(Datetime)]
    pub fn new(
        /// The year of the datetime.
        #[named]
        year: Option<YearComponent>,
        /// The month of the datetime.
        #[named]
        month: Option<MonthComponent>,
        /// The day of the datetime.
        #[named]
        day: Option<DayComponent>,
        /// The hour of the datetime.
        #[named]
        hour: Option<HourComponent>,
        /// The minute of the datetime.
        #[named]
        minute: Option<MinuteComponent>,
        /// The second of the datetime.
        #[named]
        second: Option<SecondComponent>,
    ) -> StrResult<Datetime> {
        let time = match (hour, minute, second) {
            (Some(hour), Some(minute), Some(second)) => {
                match time::Time::from_hms(hour.0, minute.0, second.0) {
                    Ok(time) => Some(time),
                    Err(_) => bail!("time is invalid"),
                }
            }
            (None, None, None) => None,
            _ => bail!("time is incomplete"),
        };

        let date = match (year, month, day) {
            (Some(year), Some(month), Some(day)) => {
                match time::Date::from_calendar_date(year.0, month.0, day.0) {
                    Ok(date) => Some(date),
                    Err(_) => bail!("date is invalid"),
                }
            }
            (None, None, None) => None,
            _ => bail!("date is incomplete"),
        };

        Ok(match (date, time) {
            (Some(date), Some(time)) => {
                Datetime::Datetime(PrimitiveDateTime::new(date, time))
            }
            (Some(date), None) => Datetime::Date(date),
            (None, Some(time)) => Datetime::Time(time),
            (None, None) => {
                bail!("at least one of date or time must be fully specified")
            }
        })
    }

    /// Returns the current date.
    ///
    /// ## Example
    /// ```example
    /// Today's date is
    /// #datetime.today().display().
    /// ```
    ///
    /// Display: Today
    /// Category: foundations
    #[func(Datetime)]
    pub fn today(
        /// An offset to apply to the current UTC date. If set to `{auto}`, the
        /// offset will be the local offset.
        #[named]
        #[default]
        offset: Smart<i64>,
        /// The virtual machine.
        vm: &mut Vm,
    ) -> StrResult<Datetime> {
        Ok(vm
            .vt
            .world
            .today(offset.as_custom())
            .ok_or("unable to get the current date")?)
    }

    /// Displays the datetime in a specified format. Depending on whether you
    /// have defined just a date, a time or both, the default format will be
    /// different. If you specified a date, it will be `[year]-[month]-[day]`.
    /// If you specified a time, it will be `[hour]:[minute]:[second]`. In the
    /// case of a datetime, it will be `[year]-[month]-[day]
    /// [hour]:[minute]:[second]`.
    ///
    /// Display: Display
    /// Category: foundations
    #[func(Datetime)]
    pub fn display(
        &self,
        /// The format used to display the datetime.
        #[default]
        pattern: Option<EcoString>,
    ) -> StrResult<EcoString> {
        let pattern = pattern.as_ref().map(EcoString::as_str).unwrap_or(match self {
            Datetime::Date(_) => "[year]-[month]-[day]",
            Datetime::Time(_) => "[hour]:[minute]:[second]",
            Datetime::Datetime(_) => "[year]-[month]-[day] [hour]:[minute]:[second]",
        });

        let format = format_description::parse(pattern)
            .map_err(format_time_invalid_format_description_error)?;

        let formatted_result = match self {
            Datetime::Date(date) => date.format(&format),
            Datetime::Time(time) => time.format(&format),
            Datetime::Datetime(datetime) => datetime.format(&format),
        }
        .map(EcoString::from);

        formatted_result.map_err(format_time_format_error)
    }

    /// Return the year if it was specified or `{none}`, otherwise.
    ///
    /// Display: Year
    /// Category: foundations
    #[func(Datetime)]
    pub fn year(&self) -> Option<i32> {
        match self {
            Datetime::Date(date) => Some(date.year()),

            Datetime::Time(_) => None,
            Datetime::Datetime(datetime) => Some(datetime.year()),
        }
    }

    /// Return the month if it was specified or `{none}`, otherwise.
    ///
    /// Display: Month
    /// Category: foundations
    #[func(Datetime)]
    pub fn month(&self) -> Option<u8> {
        match self {
            Datetime::Date(date) => Some(date.month().into()),
            Datetime::Time(_) => None,
            Datetime::Datetime(datetime) => Some(datetime.month().into()),
        }
    }

    /// Return the weekday if it was specified or `{none}`, otherwise.
    ///
    /// Display: Weekday
    /// Category: foundations
    #[func(Datetime)]
    pub fn weekday(&self) -> Option<u8> {
        match self {
            Datetime::Date(date) => Some(date.weekday().number_from_monday()),
            Datetime::Time(_) => None,
            Datetime::Datetime(datetime) => Some(datetime.weekday().number_from_monday()),
        }
    }

    /// Return the day if it was specified or `{none}`, otherwise.
    ///
    /// Display: Day
    /// Category: foundations
    #[func(Datetime)]
    pub fn day(&self) -> Option<u8> {
        match self {
            Datetime::Date(date) => Some(date.day()),
            Datetime::Time(_) => None,
            Datetime::Datetime(datetime) => Some(datetime.day()),
        }
    }

    /// Return the hour if it was specified or `{none}`, otherwise.
    ///
    /// Display: Hour
    /// Category: foundations
    #[func(Datetime)]
    pub fn hour(&self) -> Option<u8> {
        match self {
            Datetime::Date(_) => None,
            Datetime::Time(time) => Some(time.hour()),
            Datetime::Datetime(datetime) => Some(datetime.hour()),
        }
    }

    /// Return the minute if it was specified or `{none}`, otherwise.
    ///
    /// Display: Minute
    /// Category: foundations
    #[func(Datetime)]
    pub fn minute(&self) -> Option<u8> {
        match self {
            Datetime::Date(_) => None,
            Datetime::Time(time) => Some(time.minute()),
            Datetime::Datetime(datetime) => Some(datetime.minute()),
        }
    }

    /// Return the second if it was specified or `{none}`, otherwise.
    ///
    /// Display: Second
    /// Category: foundations
    #[func(Datetime)]
    pub fn second(&self) -> Option<u8> {
        match self {
            Datetime::Date(_) => None,
            Datetime::Time(time) => Some(time.second()),
            Datetime::Datetime(datetime) => Some(datetime.second()),
        }
    }
}

impl Debug for Datetime {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let year = self.year().map(|y| eco_format!("year: {y}"));
        let month = self.month().map(|m| eco_format!("month: {m}"));
        let day = self.day().map(|d| eco_format!("day: {d}"));
        let hour = self.hour().map(|h| eco_format!("hour: {h}"));
        let minute = self.minute().map(|m| eco_format!("minute: {m}"));
        let second = self.second().map(|s| eco_format!("second: {s}"));
        let filtered = [year, month, day, hour, minute, second]
            .into_iter()
            .flatten()
            .collect::<EcoVec<_>>();

        write!(f, "datetime{}", &pretty_array_like(&filtered, false))
    }
}

pub struct YearComponent(i32);
pub struct MonthComponent(Month);
pub struct DayComponent(u8);
pub struct HourComponent(u8);
pub struct MinuteComponent(u8);
pub struct SecondComponent(u8);

cast! {
    type Datetime,
}

cast! {
    YearComponent,
    v: i32 => Self(v),
}

cast! {
    MonthComponent,
    v: u8 => Self(Month::try_from(v).map_err(|_| "month is invalid")?)
}

cast! {
    DayComponent,
    v: u8 => Self(v),
}

cast! {
    HourComponent,
    v: u8 => Self(v),
}

cast! {
    MinuteComponent,
    v: u8 => Self(v),
}

cast! {
    SecondComponent,
    v: u8 => Self(v),
}

/// Format the `Format` error of the time crate in an appropriate way.
fn format_time_format_error(error: Format) -> EcoString {
    match error {
        Format::InvalidComponent(name) => eco_format!("invalid component '{}'", name),
        _ => "failed to format datetime in the requested format".into(),
    }
}

/// Format the `InvalidFormatDescription` error of the time crate in an
/// appropriate way.
fn format_time_invalid_format_description_error(
    error: InvalidFormatDescription,
) -> EcoString {
    match error {
        InvalidFormatDescription::UnclosedOpeningBracket { index, .. } => {
            eco_format!("missing closing bracket for bracket at index {}", index)
        }
        InvalidFormatDescription::InvalidComponentName { name, index, .. } => {
            eco_format!("invalid component name '{}' at index {}", name, index)
        }
        InvalidFormatDescription::InvalidModifier { value, index, .. } => {
            eco_format!("invalid modifier '{}' at index {}", value, index)
        }
        InvalidFormatDescription::Expected { what, index, .. } => {
            eco_format!("expected {} at index {}", what, index)
        }
        InvalidFormatDescription::MissingComponentName { index, .. } => {
            eco_format!("expected component name at index {}", index)
        }
        InvalidFormatDescription::MissingRequiredModifier { name, index, .. } => {
            eco_format!(
                "missing required modifier {} for component at index {}",
                name,
                index
            )
        }
        InvalidFormatDescription::NotSupported { context, what, index, .. } => {
            eco_format!("{} is not supported in {} at index {}", what, context, index)
        }
        _ => "failed to parse datetime format".into(),
    }
}
