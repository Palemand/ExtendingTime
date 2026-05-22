pub mod Time2.DateTime {
    use Time.Duration
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
        zone = UtcOffset
    })

    pub enum UtcOffset(Duration) with Eq, Order, Hash

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
        let DateTime({second = h | _ }) = dt;
        h

    pub def getYear(dt: DateTime): Int32 = 
        let DateTime({year = h | _ }) = dt;
        h

    pub def getMonth(dt: DateTime): Int32 = 
        let DateTime({month = h | _ }) = dt;
        h

    pub def getMilliseconds(dt: DateTime): Int32 = 
        let DateTime({milli = h | _ }) = dt;
        h

    pub def getMicroseconds(dt: DateTime): Int32 = 
        let DateTime({micro = h | _ }) = dt;
        h

    pub def getNanoseconds(dt: DateTime): Int32 = 
        let DateTime({nano = h | _ }) = dt;
        h
    
    pub def getDayOfYear(dt: DateTime): Int32 = 
        ???

    pub def getDayOfMonth(dt: DateTime): Int32 = 
        let DateTime({day = h | _ }) = dt;
        h

    pub def getDayOfWeek(dt: DateTime): DayOfWeek = 
        ???

    pub def getAmPm(dt: DateTime): (Int32, AmPm) = 
        ???

    pub def getZone(dt: DateTime): UtcOffset = 
        let DateTime({zone = z | _ }) = dt;
        z

    pub def getDate(year: Int32, month: Int32, day: Int32): DateTime = 
        ???

    pub def getDateTime(
        year: Int32, month: Int32, day: Int32, 
        hours: Int32, minutes: Int32, seconds: Int32, 
        milli: Int32, micro: Int32, nano: Int32, zone: Int32): DateTime = 
        ???
}