pub mod Time2.Clock {
    use Time2.DateTime.UTCOffset
    // rename to calendar effect.
    // or maybe rename old Clock to SysTime

    import java.time.{ZonedDateTime => JZonedDateTime}
    use Time.Duration
    use Time2.DateTime.UTCOffset.UTCOffset
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
                k(ToFlix.toFlix(jdt))
            }
        }
    
    ///
    /// Middleware that intercepts the `Clock` effect, modifying the time by adding a constant `offset`.
    ///
    pub def withOffset(offset: Duration, f:Unit -> a \ ef): a \ (ef - Clock) + Clock =
        run {
            f()
        } with handler Clock {
            def now(k) = run {
                k(Flow.add(Clock.now(), offset))
            } with Flow.runWithPure
        }

    ///
    /// Middleware that intercepts the `Clock` effect, modifying timezone to be this zone.
    /// Calculates the time in that zone.
    ///
    pub def withZone(zone: UTCOffset, f:Unit -> a \ ef): a \ (ef - Clock) + Clock =
        run {
            f()
        } with handler Clock {
            def now(k) =
                k(DateTime.withZone(zone, Clock.now()))
        }
    ///
    /// Middleware that adds the Flow effect for calculating elapsed time between calls to `Clock.now` and the first call.
    /// The first call will behave as normal.
    ///
    pub def withFlow(f: Unit -> a \ ef): a \ (ef - Clock) + Clock + Flow =
        region rc {
            let rf = Ref.fresh(rc, None);
            run {
                f()
            } with handler Clock {
                def now(k) = 
                    let now = Clock.now();
                    match Ref.get(rf) {
                        case None => {Ref.put(Some(now), rf); k(now)}
                        case Some(first) => 
                            DateTime.difference(now, first)
                            |> Flow.add(first) |> k
                    }
            }
        }

}