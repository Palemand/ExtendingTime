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

    def helper01(x: Duration): Int64 = 
        let d = Duration.toSeconds(x); 
        let actually_doing_something = d*d;
        let clamp = Int64.clampToInt32({min=Int32.minValue()}, {max=Int32.maxValue()}, actually_doing_something);
        Duration.toNanos(Duration.seconds(clamp))

    pub def testFlow01(): Unit \ IO =
        run {
            // One initial Clock.now and then three, ~1 sec between each 
            let dt1 = Clock.now();
            println("clock.now     : ${dt1}");
            // Three Flow.add with exactly 1 sec between each, then modified by middleware
            let dt1_1 = Flow.add(dt1, Duration.seconds(2));
            println("flow.add nr. 1: ${dt1_1}");
            let dt1_2 = Flow.add(dt1_1, Duration.seconds(2));
            println("flow.add nr. 2: ${dt1_2}");
            let dt1_3 = Flow.add(dt1_2, Duration.seconds(2));
            println("flow.add nr. 3: ${dt1_3}")
        } 
        with Clock.runWithIO
        with Flow.withModification(helper01)
        with Flow.runWithIO
        with Sleep.runWithIO

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
        with Flow.withMultiplier(200i64)
        with Flow.runWithPure
        with Sleep.runWithIO
        with Clock.runWithIO
}