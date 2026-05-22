pub mod Time2.Flow {
    use Time.Duration
    use Time2.DateTime
    use Time2.Flow

    
    def addDuration(dt: DateTime, dur: Duration): DateTime = 
        use Int64.modulo;
        use Int32.toInt64;
        let DateTime(rec) = dt;
        let {
            year = year,
            month = month,
            day = day,
            hour = hour, 
            minute = minute,
            second = second,
            milli = milli,
            micro = micro,
            nano = nano,
            zone = zone
        } = rec;
        let Duration(durNano) = dur;
        let nano1 = modulo(Int32.toInt64(nano) + modulo(durNano, 1_000i64));
        let micro1 = modulo(toInt64(nano) / 1_000i64 + modulo(durNano, 1_000i64), 1_000i64) + toInt64(nano) + modulo(durNano, 1_000i64) / 1_000i64;
        let milli1 = modulo(toInt64(nano) / 1_000i64 + modulo(durNano, 1_000i64), 1_000i64) + toInt64(nano) + modulo(durNano, 1_000i64) / 1_000i64;
        ???

    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Flow) + IO =
        run {
            f()
        } with handler Flow {
            def add(dt, dur, k) = 
                k(addDuration(dt, dur))
        }

}