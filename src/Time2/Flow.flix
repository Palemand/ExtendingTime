pub mod Time2.Flow {
    use Time.Duration
    use Time2.DateTime
    use Time2.Flow

    def addDuration(dt: DateTime, dur: Duration): DateTime = // barely correct. 
        let drawSize = (Int32.maxValue() - 999) |> Int32.toInt64;
        def loop(accDt, accDur) = {
            let DateTime.DateTime(rec) = accDt;
            let toAdd = Int64.min(drawSize, accDur);
            let toAdd32 = toAdd |> Int64.clampToInt32(min = 0, max = Int32.maxValue());
            let dt_ = DateTime.DateTime({nano = rec#nano + toAdd32 | rec})
            |> DateTime.balanceDateTime;
            if (accDur != 0i64) loop(dt_, accDur - toAdd) 
            else (dt_, 0i64)
        };
        fst (loop(dt, Duration.toNanos(dur)))
        
        // use Int64.modulo;
        // let DateTime(rec) = dt;
        // let {
        //     year = year,
        //     month = month,
        //     day = day,
        //     hour = hour, 
        //     minute = minute,
        //     second = second,
        //     milli = milli,
        //     micro = micro,
        //     nano = nano,
        //     zone = zone
        // } = rec;
        // let Duration(durNano) = dur;
        // region rc {
        //     let nanoRef = Ref.fresh(rc, durNano);
        //     let n1 = Ref.get(nanoRef);
        //     let nano1 = modulo(n1, 1000i64);
        //     Ref.transform(x -> x / 1_000i64, nanoRef);
        //     let n2 = Ref.get(nanoRef);
        //     let micro1 = modulo(n2, 1000i64);
        //     Ref.transform(x -> x / 1_000i64, nanoRef);
        //     let n3 = Ref.get(nanoRef);
        //     let milli1 = modulo(n3, 1000i64);
        //     Ref.transform(x -> x / 1_000i64, nanoRef);
        //     let n4 = Ref.get(nanoRef);
        //     let second1 = modulo(n4, 60i64);
        //     Ref.transform(x -> x / 60i64, nanoRef);
        //     let n5 = Ref.get(nanoRef);
        //     let minute1 = modulo(n5, 60i64);
        //     Ref.transform(x -> x / 60i64, nanoRef);
        //     let n6 = Ref.get(nanoRef);
        //     let hour1 = modulo(n6, 24i64);
        //     Ref.transform(x -> x / 24i64, nanoRef);
        //     let day1 = Ref.get(nanoRef);
        //     let dayOfYear = Time2.DateTime.getDayOfYear(dt);
        //     let dt1 = Time2.DateTime.getDateFromDayOfYear(dayOfYear + Option.getWithDefault(unreachable!(), Int64.tryToInt32(day1)), year); // might be wonky
        //     let _ = Time2.DateTime.getDateTime(
        //         year + Time2.DateTime.getYear(dt1),
        //         month + Time2.DateTime.getMonth(dt1),
        //         day + Time2.DateTime.getDayOfMonth(dt1),
        //         hour + Int64.clampToInt32(min = 0, max = 1000, hour1),
        //         minute + Int64.clampToInt32(min = 0, max = 1000, minute1),
        //         second + Int64.clampToInt32(min = 0, max = 1000, second1),
        //         milli + Int64.clampToInt32(min = 0, max = 1000, milli1),
        //         micro + Int64.clampToInt32(min = 0, max = 1000, micro1),
        //         nano + Int64.clampToInt32(min = 0, max = 1000, nano1), 
        //         zone
        //     );
        //     ???
        // }

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
                k(add(dt, newDur))
        }

    ///
    /// Middleware that intercepts the `Flow` effect, modifying the flow by applying `m` to it.
    ///
    pub def withModification(m: Int64 -> Int64 \ ef1, f: Unit -> a \ ef2): a \ ((ef1 + ef2) - Flow) + Flow = 
        run {
            f()
        } with handler Flow {
            def add(dt, dur, k) = 
                let Time.Duration(d) = dur;
                let newDur = Time.Duration(m(d));
                k(add(dt, newDur))
        }

}