pub mod TestDateTime {
    use Assert.assertEq
    use Time.Duration.Duration
    use Time2.DateTime.UtcOffset.UtcOffset
    use Time2.DateTime
    @Test
    pub def testDateTime01(): Unit \ Assert =
        let dt = DateTime.DateTime({
            year = 2016, 
            month = 1,
            day = 1,
            hour = 12, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = UtcOffset(Time.Duration.hours(-10) - Time.Duration.minutes(30))
        });
        assertEq(expected = "UTC-10:30", Time2.DateTime.getZone(dt) |> ToString.toString)
}