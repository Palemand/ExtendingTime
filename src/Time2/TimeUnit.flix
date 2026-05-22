pub mod Time2.TimeUnit {

    use Time.TimeUnit

    ///
    /// Represents a time unit.
    ///
    pub enum TimeUnit with Eq, ToString, Order {
        case Days
        case Hours
        case Minutes
        case Seconds
        case Milliseconds
        case Microseconds
        case Nanoseconds
    }

    ///
    /// Returns the number of milliseconds for timeunit `u`
    ///
    pub def toMilliseconds(u: Time.TimeUnit): Int32 = match u {
        case TimeUnit.Days         => 24 * 60 * 60 * 1_000,
        case TimeUnit.Hours        => 60 * 60 * 1_000,
        case TimeUnit.Microseconds => 1 / 1_000,
        case TimeUnit.Milliseconds => 1,
        case TimeUnit.Minutes      => 60 * 1_000,
        case TimeUnit.Nanoseconds  => 1 / 1_000_000,
        case TimeUnit.Seconds      => 1_000
    }

}