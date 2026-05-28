pub mod Time2.Flow {
    use Time2.DateTime.getBalancedDateTime
    use Time.Duration
    use Time2.DateTime
    use Time2.TimeUnit
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

    def unitToDur(u: TimeUnit, dur: Duration): Float64 =
        match u {
            case TimeUnit.Days => Duration.toNanos(dur) |> Int64.toFloat64 |> x -> x/(1_000_000_000f64 * 60f64 * 60f64 * 24f64)
            case TimeUnit.Hours => Duration.toNanos(dur) |> Int64.toFloat64 |> x -> x/(1_000_000_000f64 * 60f64 * 60f64)
            case TimeUnit.Minutes => Duration.toNanos(dur) |> Int64.toFloat64 |> x -> x/(1_000_000_000f64 * 60f64)
            case TimeUnit.Seconds => Duration.toNanos(dur) |> Int64.toFloat64 |> x -> x/1_000_000_000f64
            case TimeUnit.Milliseconds => Duration.toNanos(dur) |> Int64.toFloat64 |> x -> x/1_000_000f64
            case TimeUnit.Microseconds => Duration.toNanos(dur) |> Int64.toFloat64 |> x -> x/1_000f64
            case TimeUnit.Nanoseconds => Duration.toNanos(dur) |> Int64.toFloat64
        }
    
    def unitToDurInv(u: TimeUnit, res: Float64): Duration =
        match u {
            case TimeUnit.Days => res |> (x -> x * 1_000_000_000f64 * 60f64 * 60f64 * 24f64) |> Float64.truncateToInt64 |> Duration.nanoseconds
            case TimeUnit.Hours => res |> (x -> x * 1_000_000_000f64 * 60f64 * 60f64) |> Float64.truncateToInt64 |> Duration.nanoseconds
            case TimeUnit.Minutes => res |> (x -> x * 1_000_000_000f64 * 60f64) |> Float64.truncateToInt64 |> Duration.nanoseconds
            case TimeUnit.Seconds => res |> (x -> x * 1_000_000_000f64) |> Float64.truncateToInt64 |> Duration.nanoseconds
            case TimeUnit.Milliseconds => res |> (x -> x * 1_000_000f64) |> Float64.truncateToInt64 |> Duration.nanoseconds
            case TimeUnit.Microseconds => res |> (x -> x * 1_000f64) |> Float64.truncateToInt64 |> Duration.nanoseconds
            case TimeUnit.Nanoseconds => res |> Float64.truncateToInt64 |> Duration.nanoseconds
        }
    ///
    /// Middleware handler that changes the duration, following the function `m` with the TimeUnit `u`.
    ///
    /// Will follow the function on every call to Flow.add, not from the first call.
    ///
    pub def withFunction(m: Float64 -> Float64, u: TimeUnit, f: Unit -> a \ ef): a \ ef - Flow + Flow =
        region rc {
            let _rf = Ref.fresh(rc, None);
            let _rfc = Ref.fresh(rc, None);
            run {
                f()
            } with handler Flow {
                def add(dt, dur, k) = {
                    let dur_ = unitToDur(u, dur) |> m |> unitToDurInv(u);
                    k(Flow.add(dt, dur_))
                }
            }
        }

}