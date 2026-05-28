pub mod Time2.Clock {
    use Time2.DateTime.UtcOffset
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
    pub def withZone(zone: UtcOffset, f:Unit -> a \ ef): a \ (ef - Clock) + Clock =
        run {
            f()
        } with handler Clock {
            def now(k) =
                k(DateTime.withZone(zone, Clock.now()))
        }
    ///
    /// Middleware that adds the Flow effect for calculating elapsed time between calls to `Clock.now`.
    /// The first call will behave as normal.
    ///
    pub def withFlow(f: Unit -> a \ ef): a \ (ef - Clock) + Clock + Flow =
        region rc {
            let lastTime = Ref.fresh(rc, None);
            let lastClean = Ref.fresh(rc, None);
            run {
                f()
            } with handler Clock {
                def now(k) = 
                    let nowActual = Clock.now();
                    match (Ref.get(lastTime), Ref.get(lastClean)) {
                        case (Some(dt), Some(clean)) => {
                            let diff = Time2.DateTime.difference(nowActual, clean);
                            Ref.put(Some(nowActual), lastClean);
                            Flow.add(dt, diff) !> ((x) -> Ref.put(Some(x), lastTime)) |> k
                        }
                        case _ => {
                            Ref.put(Some(nowActual), lastClean);
                            nowActual !> ((x) -> Ref.put(Some(x), lastTime)) |> k
                        }
                    }
                    
            }
        }

}