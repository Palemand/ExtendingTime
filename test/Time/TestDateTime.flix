pub mod TestDateTime {
    use Time2.DateTime.getDate
    use Time2.DateTime.{getDateFromDayOfYear}
    use Assert.{assertEq, assertEqWithMsg, assertNone}
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
            zone = Some(UtcOffset(Time.Duration.hours(-10) - Time.Duration.minutes(30)))
        });
        assertEq(expected = "UTC-10:30", Option.getWithDefault(UtcOffset(Duration(0i64)), Time2.DateTime.getZone(dt)) |> ToString.toString)

    @Test
    def testGetDateFromDayOfYear01(): Unit \ Assert + Abort = 
        let dt = getDateFromDayOfYear(10, 2000);
        let exp = getDate(2000, 1, 10);
        assertEq(expected = exp, Option.getOrAbort(dt))

    @Test
    def testGetDateFromDayOfYear02(): Unit \ Assert + Abort = 
        let dt = getDateFromDayOfYear(366, 2004);
        let exp = getDate(2004, 12, 31);
        assertEq(expected = exp, Option.getOrAbort(dt))

    @Test
    def testGetDateFromDayOfYear03(): Unit \ Assert = 
        let dt = getDateFromDayOfYear(700, 2004);
        assertNone(dt)
}