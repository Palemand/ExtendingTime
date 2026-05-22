pub mod Time2.DateTime {
    use Time2.Flow
    pub enum DateTime(Date, Time, Zone) with Eq, Order, Hash

    pub enum Date(Int32, Int32, Int32) with Eq, Order, Hash //Maybe make it record for named fields

    // Time: Hour, Minute, Second, Milli, Micro, Nano
    pub enum Time(Int32, Int32, Int32, Int32, Int32, Int32) with Eq, Order, Hash // maybe record later

    pub enum Zone(Int32) with Eq, Order, Hash

    pub def addDur(dt: DateTime, dur: ?Duration): DateTime \ Flow = ???
    
}