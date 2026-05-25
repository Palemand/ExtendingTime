pub mod Time2.Clock {
    // rename to calendar effect.
    // or maybe rename old Clock to SysTime

    import java.time.{ZonedDateTime => JZonedDateTime}
    use Time.Duration
    use Time2.DateTime.UtcOffset.UtcOffset
    use Time2.DateTime
    use Time2.Clock
    use Time2.Flow

    ///
    /// Returns epoch, the time since 1970/01/01, as an Int64.
    ///
    pub def epoch(): Int64 \ Clock =
        DateTime.difference(Clock.now(), DateTime.epochZero()) |> Duration.toMillis

    ///
    /// Runs the `Clock` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Clock` effect using the `IO` effect.
    ///
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
                    zone = jdt.getOffset().getTotalSeconds() |> x -> Some(UtcOffset(Time.Duration.seconds(x)))
                }))
            }
        }
    
    ///
    /// Middleware that intercepts the `Clock` effect, modifying the flow by adding a constant `offset`.
    ///
    pub def withOffset(offset: Duration, f:Unit -> a \ ef): a \ (ef - Clock) + Clock =
        run {
            f()
        } with handler Clock {
            def now(k) = run {
                k(Flow.add(Clock.now(), offset))
            } with Flow.runWithPure
        }

    pub def withFlow(f: Unit -> a \ ef): a \ (ef - Clock) + Clock + Flow =
        region rc {
            let lastTime = Ref.fresh(rc, None);
            run {
                f()
            } with handler Clock {
                def now(k) = 
                    let nowActual = Clock.now();
                    let last: Option[DateTime] = Ref.get(lastTime);
                    match last {
                        case Some(dt) => {
                            let diff = Time2.DateTime.difference(dt, nowActual);
                            Flow.add(dt, diff) !> ((x) -> Ref.put(Some(x), lastTime)) |> k
                        }
                        case None => {
                            k(nowActual)
                        }
                    }
                    
            }
        }

}