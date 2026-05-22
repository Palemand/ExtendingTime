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
        zone = Duration
    })

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

    pub def getZone(dt: DateTime): Int32 = 
        ???

    pub def getDate(year: Int32, month: Int32, day: Int32): DateTime = 
        ???

    pub def getDateTime(
        year: Int32, month: Int32, day: Int32, 
        hours: Int32, minutes: Int32, seconds: Int32, 
        milli: Int32, micro: Int32, nano: Int32, zone: Int32): DateTime = 
        ???
}