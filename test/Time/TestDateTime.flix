pub mod TestDateTime {
    use Assert.{assertEq, assertEqWithMsg, assertNone}
    use Time2.DateTime.{getDateFromDayOfYear, getDate}
    use Time2.DateTime.UtcOffset.UtcOffset
    use Time2.DateTime
    use Time.Duration.Duration
    
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

    @Test
    def testBalanceDate01(): Unit \ Assert = 
        let dt = DateTime.balanceDateTime(DateTime.DateTime({
            year = 2016, 
            month = 13,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        }));
        let exp = DateTime.getDate(2017, 1, 1);
        assertEq(expected = exp, dt)

    @Test
    def testBalanceDate02(): Unit \ Assert + Abort = 
        let ns = Option.getOrAbort(Int64.tryToInt32(Time.Duration.toSeconds(Time.Duration.days(1))));
        let dt = DateTime.balanceDateTime(DateTime.DateTime({
            year = 2016, 
            month = 12,
            day = 1,
            hour = 0, 
            minute = 0,
            second = ns,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        }));
        let exp = DateTime.getDate(2016, 12, 2);
        assertEq(expected = exp, dt)

    @Test
    def testBalanceDate03(): Unit \ Assert + Abort = 
        let ns = Option.getOrAbort(Int64.tryToInt32(Time.Duration.toSeconds(Time.Duration.days(5))));
        let dt = DateTime.balanceDateTime(DateTime.DateTime({
            year = 2016, 
            month = 2,
            day = 27,
            hour = 0, 
            minute = 0,
            second = ns,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        }));
        let exp = DateTime.getDate(2016, 3, 3);
        assertEq(expected = exp, dt)

    @Test
    def testBalanceDate04(): Unit \ Assert + Abort = 
        let ns = Option.getOrAbort(Int64.tryToInt32(Time.Duration.toSeconds(Time.Duration.days(5))));
        let ms = Option.getOrAbort(Int64.tryToInt32(Time.Duration.toMillis(Time.Duration.hours(5))));
        let dt = DateTime.balanceDateTime(DateTime.DateTime({
            year = 2000, 
            month = 1,
            day = 31,
            hour = 0, 
            minute = 0,
            second = ns,
            milli = ms,
            micro = 0,
            nano = 0,
            zone = None
        }));
        let exp = DateTime.getDateTime(2000, 2, 5, 5, 0, 0, 0, 0, 0, None);
        assertEq(expected = exp, dt)

    @Test
    def testBalanceDate05(): Unit \ Assert = 
        let dt = DateTime.balanceDateTime(DateTime.DateTime({
            year = 2000, 
            month = 2,
            day = -1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        }));
        let exp = DateTime.getDate(2000, 1, 31);
        assertEq(expected = exp, dt)

    @Test
    def testBalanceDate06(): Unit \ Assert = 
        let dt = DateTime.balanceDateTime(DateTime.DateTime({
            year = 2000, 
            month = 2,
            day = -31,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        }));
        let exp = DateTime.getDate(2000, 1, 1);
        assertEq(expected = exp, dt)
    
    @Test
    pub def testDifference01(): Unit \ Assert =
        let dt1 = DateTime.DateTime({
            year = 2016, 
            month = 1,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        });
        let dt2 = DateTime.DateTime({
            year = 2026, 
            month = 3,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        });
        assertEq(expected = Duration.days(-(3653 + 59)), DateTime.difference(dt1, dt2))


    @Test
    pub def testDifference02(): Unit \ Assert =
        let dt1 = DateTime.DateTime({
            year = 2025, 
            month = 10,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        });
        let dt2 = DateTime.DateTime({
            year = 2010, 
            month = 3,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = None
        });
        assertEq(expected = Duration.days(5693), DateTime.difference(dt1, dt2))

    @Test
    pub def testDifference03(): Unit \ Assert =
        let dt1 = DateTime.DateTime({
            year = 2000, 
            month = 1,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = Some(UtcOffset(Duration.hours(1)))
        });
        let dt2 = DateTime.DateTime({
            year = 2000, 
            month = 1,
            day = 1,
            hour = 0, 
            minute = 0,
            second = 0,
            milli = 0,
            micro = 0,
            nano = 0,
            zone = Some(UtcOffset(Duration.hours(-2)))
        });
        assertEq(expected = Duration.hours(3), DateTime.difference(dt1, dt2))
}