pub mod Time2.DateTime {
    use Time2.DateTime.getBalancedDateTime
    use Time2.DateTime.UtcOffset.UtcOffset
    use Time.Duration
    use Time2.{Flow}

    ///
    /// Represents a date and time. The time is stored in its 24-hour clock representation. 
    ///
    pub enum DateTime({
        year = Int32, 
        month = Int32,
        day = Int32,
        hour = Int32, 
        minute = Int32,
        second = Int32,
        milli = Int32,
        micro = Int32,
        nano = Int32,
        zone = Option[UtcOffset]
    })

    ///
    /// Represents a UTC time zone as the duration between it and UTC+00:00.
    ///
    pub enum UtcOffset(Duration) with Eq, Order, Hash

    instance Eq[DateTime] {
        pub def eq(x: DateTime, y: DateTime): Bool = 
            let DateTime(rec1) = x;
            let DateTime(rec2) = y;
            let sameZone = match (rec1#zone, rec2#zone) {
                case (Some(z1), Some(z2)) => z1 == z2
                case _ => true
            };
            rec1#year == rec2#year and 
            rec1#month == rec2#month and 
            rec1#day == rec2#day and 
            rec1#hour == rec2#hour and 
            rec1#minute == rec2#minute and 
            rec1#second == rec2#second and 
            rec1#milli == rec2#milli and 
            rec1#micro == rec2#micro and 
            rec1#nano == rec2#nano and 
            sameZone
    }

    instance ToString[DateTime] {
        pub def toString(x: DateTime): String = 
            let DateTime(rec) = x;
            "${rec#year}:${rec#month}:${rec#day}:${rec#hour}:${rec#minute}:${rec#second}:${rec#milli}:${rec#micro}:${rec#nano}:${rec#zone}"
    }
    
    instance Order[DateTime] {
        pub def compare(dt1: DateTime, dt2: DateTime): Comparison =
            let diff = difference(dt1, dt2);
            if (diff < Duration.zero()) Comparison.LessThan
            else if (diff == Duration.zero()) Comparison.EqualTo
            else Comparison.GreaterThan
    }

    instance ToString[UtcOffset] {
        pub def toString(x: UtcOffset): String = 
            let UtcOffset.UtcOffset(dur) = x;
            let hour = Time.Duration.toHours(dur) |> Int64.clampToInt32(min = -12, max = 14);
            let minute = (dur - Time.Duration.hours(hour)) |> Time.Duration.toMinutes |>  Int64.clampToInt32(min = -60, max = 60);
            let op = if (Time.Duration.isPositive(dur)) "+" else "-";
            let hourStr = if (Int32.abs(hour) < 10) "0${Int32.abs(hour)}" else "${Int32.abs(hour)}";
            let minuteStr = if (Int32.abs(minute) < 10) "0${Int32.abs(minute)}" else "${Int32.abs(minute)}";
            "UTC${op}${hourStr}:${minuteStr}"
    }

    instance ToString[DayOfWeek] {
        pub def toString(x: DayOfWeek): String = match x {
            case DayOfWeek.Monday => "Monday"
            case DayOfWeek.Tuesday => "Tuesday"
            case DayOfWeek.Wednesday => "Wednesday"
            case DayOfWeek.Thursday => "Thursday"
            case DayOfWeek.Friday => "Friday"
            case DayOfWeek.Sunday => "Sunday"
            case DayOfWeek.Saturday => "Saturday"
        }
    
    }

    ///
    /// Represents the days of the week.
    ///
    pub enum DayOfWeek with Order, Eq, Hash {
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
    } 

    ///
    /// Represents AM and PM
    ///
    pub enum AmPm with Order, Eq, Hash {
        case AM, PM
    }

    ///
    /// Gets the hours stored in a DateTime
    ///
    pub def getHours(dt: DateTime): Int32 = 
        let DateTime({hour = h | _ }) = dt;
        h
    
    ///
    /// Gets the minutes stored in a DateTime
    ///
    pub def getMinutes(dt: DateTime): Int32 = 
        let DateTime({minute = m | _ }) = dt;
        m

    ///
    /// Gets the seconds stored in a DateTime
    ///
    pub def getSeconds(dt: DateTime): Int32 = 
        let DateTime({second = s | _ }) = dt;
        s

    ///
    /// Gets the year stored in a DateTime
    ///
    pub def getYear(dt: DateTime): Int32 = 
        let DateTime({year = y | _ }) = dt;
        y

    ///
    /// Gets the month stored in a DateTime
    ///
    pub def getMonth(dt: DateTime): Int32 = 
        let DateTime({month = m | _ }) = dt;
        m

    ///
    /// Gets the milliseconds stored in a DateTime
    ///
    pub def getMilliseconds(dt: DateTime): Int32 = 
        let DateTime({milli = m | _ }) = dt;
        m

    ///
    /// Gets the microseconds stored in a DateTime
    ///
    pub def getMicroseconds(dt: DateTime): Int32 = 
        let DateTime({micro = m | _ }) = dt;
        m

    ///
    /// Gets the nanoseconds stored in a DateTime
    ///
    pub def getNanoseconds(dt: DateTime): Int32 = 
        let DateTime({nano = n | _ }) = dt;
        n
    
    ///
    /// Gets the minutes stored in a DateTime
    ///
    pub def getDayOfYear(dt: DateTime): Int32 = 
        let DateTime(rec) = dt;
        List.range(1, rec#month) 
        |> List.map(m -> getDaysOfMonth(m, rec#year))
        |> List.cons(rec#day)
        |> List.sum

    ///
    /// Gets the day of month stored in a DateTime
    ///
    pub def getDayOfMonth(dt: DateTime): Int32 = 
        let DateTime({day = d | _ }) = dt;
        d

    ///
    /// Returns the weekday of the date in `dt`.
    ///
    pub def getDayOfWeek(dt: DateTime): DayOfWeek \ {} = 
        let ref = getDate(2026, 5, 25); // monday
        let diff = difference(dt, ref) |> Duration.toDays;
        let dayDiff = Int64.modulo(diff, 7i64) |>  Int64.clampToInt32(min = 0, max = 6);
        match dayDiff {
            case 0 => DayOfWeek.Monday
            case 1 => DayOfWeek.Tuesday
            case 2 => DayOfWeek.Wednesday
            case 3 => DayOfWeek.Thursday
            case 4 => DayOfWeek.Friday
            case 5 => DayOfWeek.Saturday
            case 6 => DayOfWeek.Sunday
            case _ => unreachable!()
        }

    ///
    /// Returns the hour in `dt` as its 12-hour representation.
    ///
    pub def getAmPm(dt: DateTime): (Int32, AmPm) = 
        let DateTime({ hour = h | _ }) = dt;
        if (h < 12) {
            (h, AmPm.AM)
        } else {
            if (h == 12)
                (h, AmPm.AM)
            else
                (Int32.modulo(h, 12), AmPm.PM)
        }

    ///
    /// Gets the UTC-offset timezone of `dt`
    ///
    pub def getZone(dt: DateTime): Option[UtcOffset] = 
        let DateTime({zone = z | _ }) = dt;
        z

    ///
    /// Returns a `DateTime` holding the given date. 
    ///
    /// Input is clamped to be within legal range. 
    ///
    pub def getDate(year: Int32, month: Int32, day: Int32): DateTime = 
        getDateTime(year, month, day, 0, 0, 0, 0, 0, 0, None)

    def clamp(min: {min = Int32}, max: {max = Int32}, x: Int32): Int32 = 
        if (x < min#min) {
            min#min
        } else if (x > max#max) {
            max#max
        } else {
            x
        }

    def clampInt64(min: {min = Int64}, max: {max = Int64}, x: Int64): Int64 = 
        if (x < min#min) {
            min#min
        } else if (x > max#max) {
            max#max
        } else {
            x
        }

    ///
    /// Returns a `DateTime` holding the given date and time. 
    ///
    /// Input is clamped to be within legal range. 
    ///
    pub def getDateTime(
        year: Int32, month: Int32, day: Int32, 
        hours: Int32, minutes: Int32, seconds: Int32, 
        milli: Int32, micro: Int32, nano: Int32, zone: Option[UtcOffset]): DateTime = 
        let mo = month |> clamp(min = 1, max = 12);
        let z = match zone {
            case None => None
            case Some(UtcOffset(Duration(s))) => Some(UtcOffset(Duration(clampInt64(min = -12i64, max = 14i64, s))))
        };
        let rec1 = {
            year = year, 
            month = mo,
            day = day |> clamp(min = 1, max = getDaysOfMonth(mo, year)),
            hour = hours |> clamp(min = 0, max = 23), 
            minute = minutes |> clamp(min = 0, max = 59),
            second = seconds |> clamp(min = 0, max = 59),
            milli = milli |> clamp(min = 0, max = 999),
            micro = micro |> clamp(min = 0, max = 999),
            nano = nano |> clamp(min = 0, max = 999),
            zone = z
        };
        DateTime(rec1)

    ///
    /// Returns the month of the date corresponding to `dayOfYear` in `year`.
    ///
    pub def getMonthFromDayOfYear(dayOfYear: Int32, year: Int32): Int32 = 
        def recur(doy, m) = {
            let dm = getDaysOfMonth(m, year);
            if (doy > dm) {
                recur(doy - dm, m + 1)
            } else {
                m
            }
        };
        if ((dayOfYear > 366 and isLeapYear(year)) or (dayOfYear > 365 and (not isLeapYear(year))) or dayOfYear < 1) {
            0
        } else {
            recur(dayOfYear, 1)
        }

    ///
    /// Returns the date corresponding to `dayOfYear` and `year`. Returns `None` if `dayOfYear` is invalid.
    ///
    pub def getDateFromDayOfYear(dayOfYear: Int32, year: Int32): Option[DateTime] \ {} = 
        if ((dayOfYear > 366 and isLeapYear(year)) or (dayOfYear > 365 and (not isLeapYear(year))) or dayOfYear < 1) { None } else 
        let m = getMonthFromDayOfYear(dayOfYear, year);
        let s = List.range(1, m) |> List.map(x -> getDaysOfMonth(x, year)) |> List.sum;
        let d = dayOfYear - s;
        Some(getDateTime(year, m, d, 0, 0, 0, 0, 0, 0, None))

    ///
    /// Returns `true` if `year` is a leap year.
    ///
    pub def isLeapYear(year: Int32): Bool = 
        use Int32.modulo;
        modulo(year, 4) == 0 and ((not (modulo(year, 100) == 0)) or modulo(year, 400) == 0)

    ///
    /// Returns 366 if the `year` is a leap year. Otherwise, returns 365.
    /// 
    pub def getDaysOfYear(year: Int32): Int32 = if (isLeapYear(year)) 366 else 365

    ///
    /// Returns the number of days in a given month. 
    /// 
    /// Returns 0 for `month < 1` and `month > 12`
    ///
    pub def getDaysOfMonth(month: Int32, year: Int32): Int32 = 
        if (month == 1 or month == 3 or month == 5 or month == 7 or month == 8 or month == 10 or month == 12) {
            31
        } else if (month == 4 or month == 6 or month == 9 or month == 11) {
            30
        } else if (month == 2 and isLeapYear(year)) {
            29
        } else if (month == 2) {
            28
        } else {
            0
        }

    ///
    /// Returns a `DateTime` where all fields have been balanced to legal values.
    /// 
    /// Calculates new date based on overflow and underflow.
    ///
    pub def balanceDateTime(dt: DateTime): DateTime = 
        use Int32.toInt64;
        let DateTime(rec) = dt;
        getBalancedDateTime(
            rec#year |> toInt64, rec#month |> toInt64, 
            rec#day |> toInt64, rec#hour |> toInt64, 
            rec#minute |> toInt64, rec#second |> toInt64, 
            rec#milli |> toInt64, rec#micro |> toInt64, 
            rec#nano |> toInt64, rec#zone
        )

    ///
    /// Returns a new `DateTime` where all fields have been balanced to valid values.
    /// 
    /// Calculates new date based on overflow and underflow.
    ///
    pub def getBalancedDateTime(
        year: Int64, month: Int64, day: Int64, 
        hours: Int64, minutes: Int64, seconds: Int64, 
        milli: Int64, micro: Int64, nano: Int64, zone: Option[UtcOffset]): DateTime = 
        use Int64.clampToInt32;
        use Int32.toInt64;
        def getModAndOverflow(n: Int64, m: Int64): (Int64, Int64) = {
            let mod_ = Int64.modulo(n, m);
            let over = n / m;
            (mod_, over)
        };
        def recur(y, mo: Int64, d: Int64, h: Int64, min: Int64, s: Int64, mil: Int64, mic: Int64, nan: Int64, z): DateTime = {
            if (nan >= 1000i64 or nan < 0i64) {
                let (nan1, over) = getModAndOverflow(nan, 1000i64);
                let mic1 = mic + over;
                recur(y, mo, d, h, min, s, mil, mic1, nan1, z)
            } else if (mic >= 1000i64 or mic < 0i64) {
                let (mic1, over) = getModAndOverflow(mic, 1000i64);
                let mil1 = mil + over;
                recur(y, mo, d, h, min, s, mil1, mic1, nan, z)
            } else if (mil >= 1000i64 or mil < 0i64) {
                let (mil1, over) = getModAndOverflow(mil, 1000i64);
                let s1 = s + over;
                recur(y, mo, d, h, min, s1, mil1, mic, nan, z)
            } else if (s >= 60i64 or s < 0i64) {
                let (s1, over) = getModAndOverflow(s, 60i64);
                let min1 = min + over;
                recur(y, mo, d, h, min1, s1, mil, mic, nan, z)
            } else if (min >= 60i64 or min < 0i64) {
                let (min1, over) = getModAndOverflow(min, 60i64);
                let h1 = h + over;
                recur(y, mo, d, h1, min1, s, mil, mic, nan, z)
            } else if (h >= 24i64 or h < 0i64) {
                let (h1, over) = getModAndOverflow(h, 24i64);
                let d1 = d + over;
                recur(y, mo, d1, h1, min, s, mil, mic, nan, z)
            } else if (mo > 12i64) {
                let mo1 = mo - 12i64;
                let y1 = y + 1;
                recur(y1, mo1, d, h, min, s, mil, mic, nan, z)
            } else if (mo < 0i64) {
                let mo1 = mo + 12i64;
                let y1 = y - 1;
                recur(y1, mo1, d, h, min, s, mil, mic, nan, z)
            } else 
            let balancedMo = Int64.clampToInt32(min = 0, max = 12, mo);
            let dom = getDaysOfMonth(balancedMo, y)
            |> toInt64;
            if (d > dom) {
                let d1 = d - dom;
                let mo1 = mo + 1i64;
                recur(y, mo1, d1, h, min, s, mil, mic, nan, z)
            } else if (d < 0i64) {
                let lastMo = if (balancedMo == 1) { 12 } else {balancedMo - 1};
                let d1 = d + ((getDaysOfMonth(lastMo, y) + 1) |> toInt64);
                let mo1 = mo - 1i64;
                recur(y, mo1, d1, h, min, s, mil, mic, nan, z)
            } else {
                getDateTime(
                    y, 
                    balancedMo, 
                    d   |> clampToInt32(min = 1, max = 31), 
                    h   |> clampToInt32(min = 0, max = 24), 
                    min |> clampToInt32(min = 0, max = 60), 
                    s   |> clampToInt32(min = 0, max = 60), 
                    mil |> clampToInt32(min = 0, max = 1000), 
                    mic |> clampToInt32(min = 0, max = 1000), 
                    nan |> clampToInt32(min = 0, max = 1000), 
                    z
                )
            }
        };
        recur(
            year |> clampToInt32(min = Int32.minValue(), max = Int32.maxValue()), 
            month, day, hours, minutes, seconds, milli, micro, nano, zone
        )

    ///
    /// Finds the time elapsed between `dt1` and `dt2`.
    ///
    pub def difference(dt1: DateTime, dt2: DateTime): Duration \ {} = 
        let (DateTime(rec1), DateTime(rec2)) = (dt1, dt2);
        let dnano = (rec1#nano - rec2#nano) |> Int32.toInt64 |> Duration.nanoseconds;
        let dmicro = (rec1#micro - rec2#micro) |> Duration.microseconds;
        let dmilli = (rec1#milli - rec2#milli) |> Duration.milliseconds;
        let dsecond = (rec1#second - rec2#second) |> Duration.seconds;
        let dminute = (rec1#minute - rec2#minute) |> Duration.minutes;
        let dhour = (rec1#hour - rec2#hour) |> Duration.hours;
        let ddays = (rec1#day - rec2#day) |> Duration.days;
        let dmonth = (rec1#month, rec2#month) 
        |> match (d1, d2) -> List.range(Int32.min(d1, d2), Int32.max(d1,d2))
        |> List.foldLeft(accDays -> monthidx -> accDays + getDaysOfMonth(monthidx, rec2#year), 0)
        |> (x) -> (if (d1 < d2) -1*x else x) |> Duration.days;
        let dyear = (rec1#year, rec2#year)
        |> match (y1, y2) -> List.range(Int32.min(y1, y2), Int32.max(y1,y2))
        |> List.foldLeft(accDays -> year -> accDays + getDaysOfYear(year), 0)
        |> (x) -> (if (y1 < y2) -1*x else x)  |> Duration.days;
        let dzone = match (rec1#zone, rec2#zone) {
            case (Some(UtcOffset(d1)), Some(UtcOffset(d2))) => d1 - d2
            case _ => Duration(0i64)
        };
        dnano + dmicro + dmilli + dsecond + dminute + dhour + ddays + dmonth + dyear - dzone

    ///
    /// Returns UNIX-epoch 0 as a `DateTime`
    ///
    pub def epoch(): DateTime = 
        Time2.DateTime.getDateTime(1970, 1, 1, 0, 0, 0, 0, 0, 0, Some (UtcOffset(Duration.zero())))
}