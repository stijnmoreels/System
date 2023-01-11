namespace System

open System.Text.RegularExpressions
open System.Globalization
open System.Runtime.CompilerServices

/// Represents the measurements in which dates are described.
type DateMeasurement = Year | Month | Day | Hour | Minute | Second | Millisecond

/// Represents the difference between two dates.
type DateDiff =
  { Years : int
    Months : int
    Days : int
    Hours : int 
    Minutes : int
    Seconds : int
    Milliseconds : int }

/// Dev-easy date operations on `DateTimeOffset`.
module Date =
  /// Gets current UTC date.
  [<CompiledName("Utc")>]
  let utc = DateTimeOffset.UtcNow
  /// Gets all the measures (Year -> Millisecond) used as unit of measure during the date operations.
  [<CompiledName("Measurements")>]
  let measures = seq { Year; Month; Day; Hour; Minute; Second; Millisecond }
  /// Matches user functions to date measures.
  let matches fyear fmonth fday fhour fminute fsecond fmillisecond m x =
    match m with
    | Year -> fyear x
    | Month -> fmonth x
    | Day -> fday x
    | Hour -> fhour x
    | Minute -> fminute x
    | Second -> fsecond x
    | Millisecond -> fmillisecond x
  /// Gets all date measures above a given date measure (ex. measures below Day are Hour, Minute, Second & MilliSecond).
  [<CompiledName("MeasurementsBelow")>]
  let measuresBelow m = measures |> Seq.rev |> Seq.takeWhile ((<>) m) |> Seq.rev
  /// Gets all date measures below a given date measure (ex. measures above Day are Year & Month). 
  [<CompiledName("MeasurementsAbove")>]
  let measuresAbove m = Seq.except (measuresBelow m) measures
  /// Add an unit of a date measure to a given date.
  let add x measure (date : DateTimeOffset) =
    match measure with
    | Year -> date.AddYears x
    | Month -> date.AddMonths x
    | Day -> date.AddDays x
    | Hour -> date.AddHours x
    | Minute -> date.AddMinutes x
    | Second -> date.AddSeconds x
    | Millisecond -> date.AddMilliseconds x
  /// Add 1 date measure to a given date.
  let add1 measure date = add 1 measure date
  /// Add an unit of a date measure to a given date.
  let next x measure date = add x measure date
  /// Add 1 date measure to a given date.
  let next1 measure date = add1 measure date
  /// Subtracts an unit of a date measure to a given date.
  let subtract x measure date = add -x measure date
  /// Subtracts 1 date measure to a given date.
  let subtract1 measure date = subtract 1 measure date
  /// Subtracts an unit of a date measure to a given date.
  let prev x measure date = subtract x measure date
  /// Subtracts 1 date measure to a given date.
  let prev1 measure date = subtract1 measure date
  /// Adds an amount of weeks to a given date.
  let addWeeks x date = add (7 * x) Day date
  /// Subtracts an amount of weeks to a given date.
  let subtractWeeks x date = subtract (7 * x) Day date
  /// Gets all the hours in a day.
  [<CompiledName("HoursInDay")>]
  let hoursInDay = [1..23]
  /// Gets all the minutes in a hour.
  [<CompiledName("MinutesInHour")>]
  let minutesInHour = [1..59]
  /// Gets all the secons in a minute.
  [<CompiledName("SecondsInMinute")>]
  let secondsInMinute = [1..59]
  /// Gets all the milliseconds in a second.
  [<CompiledName("MillisecondsInSecond")>]
  let millisecondsInSecond = [1..999]
  /// Creats a `DateTimeOffset` with all the date measurements passed in, including the offset.
  let create year month day hour minute second milli offset = DateTimeOffset (year, month, day, hour, minute, second, milli, offset)
  /// Sets the year in a given date; could fail if the set year does not match the other date measurements.
  let setYear y (d : DateTimeOffset) = create y d.Month d.Day d.Hour d.Minute d.Second d.Millisecond d.Offset
  /// Sets the month in a given date; could fail if the set month does not match the other date measurements.
  let setMonth m (d : DateTimeOffset) = create d.Year m d.Day d.Hour d.Minute d.Second d.Millisecond d.Offset
  /// Sets the day in a given date; could fail if the set day does not match the other date measurements.
  let setDay day (d : DateTimeOffset) = create d.Year d.Month day d.Hour d.Minute d.Second d.Millisecond d.Offset
  /// Sets the hour in a given date.
  let setHour h (d : DateTimeOffset) = create d.Year d.Month d.Day h d.Minute d.Second d.Millisecond d.Offset
  /// Sets the minute in a given date.
  let setMinute m (d : DateTimeOffset) = create d.Year d.Month d.Day d.Hour m d.Second d.Millisecond d.Offset
  /// Sets the second in a given date.
  let setSecond s (d : DateTimeOffset) = create d.Year d.Month d.Day d.Hour d.Minute s d.Millisecond d.Offset
  /// Sets the millisecond in a given date.
  let setMillisecond ms (d : DateTimeOffset) = create d.Year d.Month d.Day d.Hour d.Minute d.Second ms d.Offset
  /// Sets the year in a given date.
  let setOffset o (d : DateTimeOffset) = create d.Year d.Month d.Day d.Hour d.Minute d.Second d.Millisecond o
  /// Sets a value in a date measurement in a given date; could fail if the set value does not match the other date measurements.
  let set measure x d = matches setYear setMonth setDay setHour setMinute setSecond setMillisecond measure x d
  /// Gets the year of the given date.
  let year (d : DateTimeOffset) = d.Year
  /// Gets the month of the given date.
  let month (d : DateTimeOffset) = d.Month
  /// Gets the day of the given date.
  let day (d : DateTimeOffset) = d.Day
  /// Gets the hour of the given date.
  let hour (d : DateTimeOffset) = d.Hour
  /// Gets the minute of the given date.
  let minute (d : DateTimeOffset) = d.Minute
  /// Gets the second of the given date.
  let second (d : DateTimeOffset) = d.Second
  /// Gets the millisecond of the given date.
  let millisecond (d : DateTimeOffset) = d.Millisecond
  /// Gets a value of a date measurement of the given date.
  let get measure date = matches year month day hour minute second millisecond measure date
  /// Gets the flag indicating whether or not the given date is within a leap year.
  [<CompiledName("IsLeapYear")>]
  let isLeapYear date = year date % 4 = 0
  /// Gets the flag indicating whether or not the given date is on the leap day.
  [<CompiledName("IsLeapDay")>]
  let isLeapDay date = get Day date = 29 && get Month date = 2
  /// Gets the amount of days in a given month in a given year.
  let daysInMonth year month = DateTime.DaysInMonth (year, month)
  /// Date operations on a date difference.
  module Diff =
    /// Gets a value represeing no date difference.
    let none = { Years = 0; Months = 0; Days = 0; Hours = 0; Minutes = 0; Seconds = 0; Milliseconds = 0 }
    /// Gets the value of a date measurement to a date difference.
    let get m diff =
      match m with
      | Year -> diff.Years
      | Month -> diff.Months
      | Day -> diff.Days
      | Hour -> diff.Hours
      | Minute -> diff.Minutes
      | Second -> diff.Seconds
      | Millisecond -> diff.Milliseconds
    /// Sets the value of a date measurement to a date difference.
    let set x m diff =
      match m with
      | Year -> { diff with Years = x }
      | Month -> { diff with Months = x }
      | Day -> { diff with Days = x }
      | Hour -> { diff with Hours = x }
      | Minute -> { diff with Minutes = x }
      | Second -> { diff with Seconds = x }
      | Millisecond -> { diff with Milliseconds = x }
    /// Creates a date difference based on a single value of a date measurement.
    let create x m = set x m none
    /// Add a value of a date measurement to a date difference.
    let add x m diff =
      match m with
      | Year -> set (diff.Years + x) m diff
      | Month -> set (diff.Months + x) m diff
      | Day -> set (diff.Days + x) m diff
      | Hour -> set (diff.Hours + x) m diff
      | Minute -> set (diff.Minutes + x) m diff
      | Second -> set (diff.Seconds + x) m diff
      | Millisecond -> set (diff.Milliseconds + x) m diff
    let addTo (date : DateTimeOffset) diff =
      date.AddYears(diff.Years).AddMonths(diff.Months).AddDays(diff.Days).AddHours(diff.Hours).AddMinutes(diff.Minutes).AddSeconds(diff.Seconds).AddMilliseconds(diff.Milliseconds)

  let private ensure min max = if min < max then min, max else max, min

  /// Calculates the different between to given dates.
  [<CompiledName("Difference")>]
  let diff min max =
    let min, max = ensure min max
    let rec addTill m r =
      if Diff.addTo min r |> add 1 m > max then r
      else addTill m (Diff.add 1 m r)

    if min = max then Diff.none 
    else Seq.fold (fun r m -> addTill m r) Diff.none measures
  /// Gets all the available dates between two dates in a given measurement.
  [<CompiledName("Range")>]
  let range measure (start : DateTimeOffset) (finish : DateTimeOffset) =
    let start, finish = ensure start finish
    let rangeYear (start : DateTimeOffset) (finish : DateTimeOffset) =
      let duration = finish.Year - start.Year
      seq { start; for x in 1..duration do add x Year start }
    
    let rangeMonth start finish =
      let rec addTill date xs =
        let next = add 1 Month date
        if next > finish then xs
        else addTill next (next::xs)
      addTill start [ start ] |> List.rev

    let regular getTotal fromMeasure =
      let duration = finish - start
      seq { start; for x in 1. ..getTotal duration do start.Add (fromMeasure x) }

    match measure with
    | Year -> rangeYear start finish
    | Month -> rangeMonth start finish
    | Day -> regular (fun (x : TimeSpan) -> x.TotalDays) TimeSpan.FromDays
    | Hour -> regular (fun (x : TimeSpan) -> x.TotalHours) TimeSpan.FromHours
    | Minute -> regular (fun (x : TimeSpan) -> x.TotalMinutes) TimeSpan.FromMinutes
    | Second -> regular (fun (x : TimeSpan) -> x.TotalSeconds) TimeSpan.FromSeconds
    | Millisecond -> regular (fun (x : TimeSpan) -> x.TotalMilliseconds) TimeSpan.FromMilliseconds
  /// Trims the given date on the given date measurement.
  /// i.e. date trimmed on days will have no hours, minutes, seconds or milliseconds.
  let trim measure (date : DateTimeOffset) =
    match measure with
    | Year -> create date.Year 1 1 0 0 0 0 date.Offset
    | Month -> create date.Year date.Month 1 0 0 0 0 date.Offset
    | Day -> create date.Year date.Month date.Day 0 0 0 0 date.Offset
    | Hour -> create date.Year date.Month date.Day date.Hour 0 0 0 date.Offset
    | Minute -> create date.Year date.Month date.Day date.Hour date.Minute 0 0 date.Offset
    | Second -> create date.Year date.Month date.Day date.Hour date.Minute date.Second 0 date.Offset
    | Millisecond -> create date.Year date.Month date.Day date.Hour date.Minute date.Second date.Millisecond date.Offset

  let private euDayOfWeek = 
    match utc.DayOfWeek with
    | DayOfWeek.Sunday -> 6
    | x -> int x - 1

  let private week x =
    let start = add x Day utc |> trim Day
    let finish = add 6 Day start
    range Day start finish
  
  /// Gets all the days of the week in the European continent.
  [<CompiledName("EuDaysOfWeek")>]
  let euDaysOfWeek = [ DayOfWeek.Monday; DayOfWeek.Tuesday; DayOfWeek.Wednesday; DayOfWeek.Thursday; DayOfWeek.Friday; DayOfWeek.Saturday; DayOfWeek.Sunday ]
  /// Gets the date of today, trimmed on Day.
  [<CompiledName("Today")>]
  let today = trim Day utc 
  /// Gets the date of tommorrow; trimmed on Day.
  [<CompiledName("Tommorrow")>]
  let tommorrow = add1 Day today
  /// Gets the date of yesterday; trimmed on Day.
  [<CompiledName("Yesterday")>]
  let yesterday = add -1 Day today
  /// Gets the date of the day after tommorrow; trimmed on Day.
  [<CompiledName("DayAfterTommorrow")>]
  let dayAfterTommorrow = add 2 Day today
  /// Gets all the dates from this week; trimmed on Day.
  [<CompiledName("ThisWeek")>]
  let thisWeek = week (- euDayOfWeek)
  /// Gets all the dates from next week; trimmed on Day.
  [<CompiledName("NextWeek")>]
  let nextWeek = week (7 - euDayOfWeek)
  /// Gets all the dates from last week; trimmed on Day.
  [<CompiledName("LastWeek")>]
  let lastWeek = week (-7 - euDayOfWeek)
  /// Formats a given date on a date measurement.
  let formatOn measure (date : DateTimeOffset) =
    match measure with
    | Year -> date.ToString ("yyyyZ")
    | Month -> date.ToString ("yyyy-MMZ")
    | Day -> date.ToString ("yyyy-MM-ddZ")
    | Hour -> date.ToString ("yyyy-MM-ddTHHZ")
    | Minute -> date.ToString ("yyyy-MM-ddTHH:mmZ")
    | Second -> date.ToString ("yyyy-MM-ddTHH:mm:ssZ")
    | Millisecond -> date.ToString ("yyyy-MM-dd-THH:mm:ss.fffZ")
  let format date = formatOn Millisecond date

  let private yearPattern = Regex ("^[0-9]{4}Z$", RegexOptions.Compiled)
  let private monthPattern = Regex ("^[0-9]{4}\-[0-9]{2}Z$", RegexOptions.Compiled)
  /// Parses the incoming text value, assuming an universal date format.
  [<CompiledName("ParseUtc")>]
  let parse (str : string) =
    try match str with
        | null -> Error (ArgumentNullException "str" :> exn)
        | x when x.Length = 5 && yearPattern.IsMatch x -> str.TrimEnd('Z') + "-01-01Z" |> Ok
        | x when x.Length = 8 && monthPattern.IsMatch x -> str.TrimEnd('Z') + "-01Z" |> Ok
        | x -> Ok x
        |> Result.map (fun str -> DateTimeOffset.Parse(str, styles=DateTimeStyles.AssumeUniversal))
    with ex -> Error ex
  /// Maps two dates with a function to a resulting value, trimming by a date measurement fault margin while doing so.
  let map2 op faultMargin left right =
    let left = trim faultMargin left
    let right = trim faultMargin right
    op left right
  /// Determines if two dates are equal, based on a date measurement.
  let equal faultMargin left right =
    map2 (=) faultMargin left right
  /// Determines if one date is greater than the other, based on a date measurement.
  let greaterThan faultMargin min max =
    map2 (<) faultMargin min max
  /// Determines if one date is less than the other, based on a date measurement.
  let lessThan faultMargin min max =
    map2 (>) faultMargin min max
  /// Pattern to get the year, month, and day from a given date.
  let (|YearMonthDay|_|) (date : DateTimeOffset) =
    Some (date.Year, date.Month, date.Day)

/// Dev-easy extensions for date operations on `DateTimeOffset`.
[<Extension>]
type DateExtensions () =
  /// Trims the date on a given date measurement.
  [<Extension>]
  static member Trim (date, measure) = Date.trim measure date
  /// Formats the date by an universal format.
  [<Extension>]
  static member FormatUtc (date) = Date.format date
  /// Determines if two dates are equal, based on a date measurement.
  [<Extension>]
  static member Equals (date, other, measure) = Date.equal measure date other
  /// Determines if one date is greater than the other, based on a date measurement.
  [<Extension>]
  static member GreaterThan (date, other, measure) = Date.greaterThan measure other date
  /// Determines if one date is less than the other, based on a date measurement.
  [<Extension>]
  static member LessThan (date, other, measure) = Date.lessThan measure other date
  /// Maps two dates with a function to a resulting value, trimming by a date measurement fault margin while doing so.
  [<Extension>]
  static member Select (date, (operation : Func<_, _, _>), other, measure) = 
    if isNull operation then nullArg (nameof(operation))
    Date.map2 (fun d1 d2 -> operation.Invoke (d1, d2)) measure other date
  /// Adds an amount of weeks to a given date.
  [<Extension>]
  static member AddWeeks (date, amount) = Date.addWeeks amount date
  /// Subtracts an amount of weeks to a given date.
  [<Extension>]
  static member SubtractWeeks (date, amount) = Date.subtractWeeks amount date
