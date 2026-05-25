pub mod TestTime {
    use Assert.assert
    use Time.Duration
    use Time2.DateTime
    use Assert.assertTrue
    use Time.{Clock => OClock}
    use Time2.Clock
    @Test
    pub def testTime01(): Unit \ Assert + IO =
        run {
            let now = Clock.now();
            let m1 = OClock.now();
            let m2 = DateTime.difference(now, DateTime.epoch()) |> Duration.toMillis;
            let x = 500i64;
            //println("m1: ${m1}, m2: ${m2}, diff: ${(m1 - m2) |> Int64.abs}");
            assert(m2 - x < m1 and m1 < m2 + x, 
            RichString.fromString("Assertion failed: m1: ${m1}, m2: ${m2}, diff: ${(m1 - m2) |> Int64.abs}"))
        } with Clock.runWithIO
        with OClock.runWithIO
}