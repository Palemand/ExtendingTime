pub mod Time2.Flow {
    use Time2.DateTime.getBalancedDateTime
    use Time.Duration
    use Time2.DateTime
    use Time2.Flow

    def addDuration(dt: DateTime, dur: Duration): DateTime = // barely correct. 
        use Int32.toInt64;
        let DateTime.DateTime(rec) = dt;
        let oNano = rec#nano;
        let DateTime.DateTime(rec_) = getBalancedDateTime(
            rec#year |> toInt64,
            rec#month |> toInt64,
            rec#day |> toInt64,
            rec#hour |> toInt64, 
            rec#minute |> toInt64,
            rec#second |> toInt64,
            rec#milli |> toInt64,
            rec#micro |> toInt64,
            Duration.toNanos(dur),
            rec#zone);
        DateTime.DateTime({nano = rec_#nano + oNano | rec_ }) |> Time2.DateTime.balanceDateTime

    ///
    /// Runs the `Flow` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Flow` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Flow) + IO =
        run {
            f()
        } with handler Flow {
            def add(dt, dur, k) = 
                k(addDuration(dt, dur))
        }

    ///
    /// Runs the `Flow` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Flow` effect without adding an effect.
    ///
    pub def runWithPure(f: Unit -> a \ ef): a \ ef - Flow = 
        run {
            f()
        } with handler Flow {
            def add(dt, dur, k) = 
                k(addDuration(dt, dur))
        }

    ///
    /// Middleware that intercepts the `Flow` effect, modifying the flow by multiplying it with `m`.
    ///
    pub def withMultiplier(m: Int64, f: Unit -> a \ ef): a \ (ef - Flow) + Flow = 
        run {
            f()
        } with handler Flow {
            def add(dt, dur, k) = 
                let Time.Duration(d) = dur;
                let newDur = Time.Duration(d * m);
                k(Flow.add(dt, newDur))
        }

    ///
    /// Middleware that intercepts the `Flow` effect, modifying the flow by applying `m` to it.
    ///
    pub def withModification(m: Duration -> Int64 \ ef1, f: Unit -> a \ ef2): a \ ((ef1 + ef2) - Flow) + Flow = 
        run {
            f()
        } with handler Flow {
            def add(dt, dur, k) = 
                let newDur = Time.Duration(m(dur));
                k(add(dt, newDur))
        }

}