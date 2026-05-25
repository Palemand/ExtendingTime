pub mod TestParse {
    use Time2.DateTime.UtcOffset.UtcOffset
    use Time2.DateTime.Parse
    use Time2.DateTime
    @Test
    pub def testParse01(): Unit \ Assert + IO =
        run {
            let expected = DateTime.getDateTime(2026, 1, 1, 0, 0, 0, 0, 0, 0, Some(UtcOffset(Time.Duration.zero())));
            let parsed = DateTime.Parse.parse("2026-01-01T00:00:00Z");
            Assert.assertEq(expected = expected, parsed)
        } with Parse.runWithIO
}