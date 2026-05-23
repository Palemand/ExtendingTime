pub mod Time2.DateTime {
    use Time2.DateTime.getDaysOfMonth
    use Time2.DateTime.UtcOffset
    use Time.Duration
    use Time2.DateTime.isLeapYear
    use Time2.{Flow}

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
            let DateTime(rec1) = x;
            "${rec1#year}:${rec1#month}:${rec1#day}"
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

    pub enum DayOfWeek with Order, Eq, Hash {
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
    } 

    pub enum AmPm with Order, Eq, Hash {
        case AM, PM
    }

    pub def getHours(dt: DateTime): Int32 = 
        let DateTime({hour = h | _ }) = dt;
        h
    
    pub def getMinutes(dt: DateTime): Int32 = 
        let DateTime({minute = m | _ }) = dt;
        m

    pub def getSeconds(dt: DateTime): Int32 = 
        let DateTime({second = s | _ }) = dt;
        s

    pub def getYear(dt: DateTime): Int32 = 
        let DateTime({year = y | _ }) = dt;
        y

    pub def getMonth(dt: DateTime): Int32 = 
        let DateTime({month = m | _ }) = dt;
        m

    pub def getMilliseconds(dt: DateTime): Int32 = 
        let DateTime({milli = m | _ }) = dt;
        m

    pub def getMicroseconds(dt: DateTime): Int32 = 
        let DateTime({micro = m | _ }) = dt;
        m

    pub def getNanoseconds(dt: DateTime): Int32 = 
        let DateTime({nano = n | _ }) = dt;
        n
    
    pub def getDayOfYear(dt: DateTime): Int32 = 
        ???

    pub def getDayOfMonth(dt: DateTime): Int32 = 
        let DateTime({day = d | _ }) = dt;
        d

    pub def getDayOfWeek(dt: DateTime): DayOfWeek = 
        ???

    pub def getAmPm(dt: DateTime): (Int32, AmPm) = 
        let DateTime({ hour = h | _ }) = dt;
        if (h < 13) {
            (h, AmPm.AM)
        } else {
            (h, AmPm.PM)
        }

    pub def getZone(dt: DateTime): Option[UtcOffset] = 
        let DateTime({zone = z | _ }) = dt;
        z

    pub def getDate(year: Int32, month: Int32, day: Int32): DateTime = 
        getDateTime(year, month, day, 0, 0, 0, 0, 0, 0, None)

    pub def getDateTime(
        year: Int32, month: Int32, day: Int32, 
        hours: Int32, minutes: Int32, seconds: Int32, 
        milli: Int32, micro: Int32, nano: Int32, zone: Option[UtcOffset]): DateTime = 
        let rec1 = {
            year = year, 
            month = month,
            day = day,
            hour = hours, 
            minute = minutes,
            second = seconds,
            milli = milli,
            micro = micro,
            nano = nano,
            zone = zone   
        };
        DateTime(rec1)

    def getMonthFromDayOfYear(dayOfYear: Int32, year: Int32): Int32 \ {} = 
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

    pub def getDateFromDayOfYear(dayOfYear: Int32, year: Int32): Option[DateTime] \ {} = 
        if ((dayOfYear > 366 and isLeapYear(year)) or (dayOfYear > 365 and (not isLeapYear(year))) or dayOfYear < 1) { None } else 
        let m = getMonthFromDayOfYear(dayOfYear, year);
        let s = List.range(1, m) |> List.map(x -> getDaysOfMonth(x, year)) |> List.sum;
        let d = dayOfYear - s;
        Some(getDateTime(year, m, d, 0, 0, 0, 0, 0, 0, None))

    pub def isLeapYear(year: Int32): Bool = 
        use Int32.modulo;
        modulo(year, 4) == 0 and ((not (modulo(year, 100) == 0)) or modulo(year, 400) == 0)

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

    pub def balanceDateTime(dt: DateTime): DateTime = 
        def getModAndOverflow(n, m) = {
            let mod_ = Int32.modulo(n, m);
            let over = n / m;
            (mod_, over)
        };
        def recur(y, mo, d, h, min, s, mil, mic, nan, z): DateTime = { //refactor to work with i64
            if (nan >= 1000 or nan < 0) {
                let (nan1, over) = getModAndOverflow(nan, 1000);
                let mic1 = mic + over;
                recur(y, mo, d, h, min, s, mil, mic1, nan1, z)
            } else if (mic >= 1000) {
                let (mic1, over) = getModAndOverflow(mic, 1000);
                let mil1 = mil + over;
                recur(y, mo, d, h, min, s, mil1, mic1, nan, z)
            } else if (mil >= 1000) {
                let (mil1, over) = getModAndOverflow(mil, 1000);
                let s1 = s + over;
                recur(y, mo, d, h, min, s1, mil1, mic, nan, z)
            } else if (s >= 60) {
                let (s1, over) = getModAndOverflow(s, 60);
                let min1 = min + over;
                recur(y, mo, d, h, min1, s1, mil, mic, nan, z)
            } else if (min >= 60) {
                let (min1, over) = getModAndOverflow(min, 60);
                let h1 = h + over;
                recur(y, mo, d, h1, min1, s, mil, mic, nan, z)
            } else if (h >= 24) {
                let (h1, over) = getModAndOverflow(min, 24);
                let d1 = d + over;
                recur(y, mo, d1, h1, min, s, mil, mic, nan, z)
            } else if (d > getDaysOfMonth(mo, y)) {
                let d1 = d - getDaysOfMonth(mo, y);
                let mo1 = mo + 1;
                recur(y, mo1, d1, h, min, s, mil, mic, nan, z)
            } else if (mo >= 12) {
                let mo1 = mo - 12;
                let y1 = y + 1;
                recur(y1, mo1, d, h, min, s, mil, mic, nan, z)
            } else {
                getDateTime(y, mo, d, h, min, s, mil, mic, nan, z)
            }
        };
        let DateTime(rec) = dt;
        let {
            year = year,
            month = month,
            day = day,
            hour = hour, 
            minute = minute,
            second = second,
            milli = milli,
            micro = micro,
            nano = nano,
            zone = zone 
        } = rec;
        recur(year, month, day, hour, minute, second, milli, micro, nano, zone)
}