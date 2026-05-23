pub mod TestFlow {
    use Assert.assertEq
    use Time2.DateTime
    use Time2.Flow
    use Time.Duration
    @Test
    pub def testAdd01(): Unit \ Assert + Flow =
        let dt = DateTime.getDate(2019, 2, 5);
        let dur = Duration.days(1);
        let exp = DateTime.getDate(2019, 2, 6);
        assertEq(expected = exp, Flow.add(dt, dur))
}