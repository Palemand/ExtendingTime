pub mod Time2.Clock {
    use Time2.DateTime.UtcOffset.UtcOffset
    import java.time.{ZonedDateTime => JZonedDateTime}
    use Time2.Clock
    
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Clock) + IO =
        run {
            f()
        } with handler Clock {
            def now(k) = {
                let jdt = JZonedDateTime.now();
                let jNanos = jdt.getNano();
                let millis = jNanos / 1_000_000;
                let micros = jNanos / 1_000 - millis * 1_000;
                let nanos = jNanos - millis * 1_000_000 - micros * 1_000;

                k(Time2.DateTime({
                    year = jdt.getYear(), 
                    month = jdt.getMonthValue(),
                    day = jdt.getDayOfMonth(),
                    hour = jdt.getHour(), 
                    minute = jdt.getMinute(),
                    second = jdt.getSecond(),
                    milli = millis,
                    micro = micros,
                    nano = nanos,
                    zone = jdt.getOffset().getTotalSeconds() |> x -> UtcOffset(Time.Duration.seconds(x))
                }))
            }
        }
}