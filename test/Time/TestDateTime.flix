pub mod TestDateTime {
    use Assert.assertEq
    use Time.Duration.Duration
    use Time2.DateTime.UtcOffset.UtcOffset
    use Time2.DateTime
    
    @Test
    def isLeapYear01(): Unit \ Assert = 
        use Time2.DateTime.isLeapYear;
        Assert.assertTrue(isLeapYear(2020));
        Assert.assertTrue(isLeapYear(2024));
        Assert.assertTrue(isLeapYear(2000));
        Assert.assertTrue(not isLeapYear(1700));
        Assert.assertTrue(not isLeapYear(2025))

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