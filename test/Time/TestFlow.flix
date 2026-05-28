pub mod TestFlow {
    use Assert.assertEq
    use Time2.DateTime
    use Time2.Flow
    use Time.Duration
    use Time.Sleep
    use Time2.Clock
    @Test
    pub def testAdd01(): Unit \ Assert + Flow =
        let dt = DateTime.getDate(2019, 2, 5);
        let dur = Duration.days(1);
        let exp = DateTime.getDate(2019, 2, 6);
        assertEq(expected = exp, Flow.add(dt, dur))

    pub def testFlow02(): Unit \ IO =
        run {
            println(Clock.now());
            Time.Sleep.sleep(Duration.seconds(1));
            println(Clock.now());
            Time.Sleep.sleep(Duration.seconds(1));
            println(Clock.now());
            Time.Sleep.sleep(Duration.seconds(1));
            println(Clock.now())
        } 
        with Clock.withFlow
        with Flow.withMultiplier(2i64)
        with Flow.runWithPure
        with Sleep.runWithIO
        with Clock.runWithIO
    
    pub def testFlow03(): Unit \ IO =
        run {
            let dt = DateTime.getDateTime(2000, 1, 1, 12, 0, 0, 0, 0, 0, None);
            let dt1 = Flow.add(dt, Duration.nanoseconds(2_500_000_000i64));
            let dt2 = Flow.add(dt1, Duration.seconds(5));
            List#{dt, dt1, dt2} |> List.forEach(x -> DateTime.format(x) |> println)
        } 
        with Flow.withFunction(x -> x*x, Time2.TimeUnit.Seconds)
        with Flow.runWithPure
        with Time2.DateTime.Format.runWithISO
}