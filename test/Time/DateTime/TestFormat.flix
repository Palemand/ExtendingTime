pub mod TestFormat {
    use Time2.DateTime.Format
    use Time2.DateTime
    @Test
    pub def testFormat01(): Unit \ Assert + IO =
        run {
            let date = DateTime.getDateTime(2016, 5, 22, 05, 30, 10, 0, 0, 0, None);
            let fmtStr = DateTime.Format.format(date);
            Assert.assertEq(expected = "2016-05-22T05:30:10Z", fmtStr)
        } with Format.runWithIO
}