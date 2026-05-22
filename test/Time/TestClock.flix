pub mod TestClock {
    use Assert.{assert, assertTrue}
    use Time2.Clock
    use Time2.DateTime
    @Test
    pub def testClock01(): Unit \ Assert + Clock = 
        let now = Clock.now();
        let DateTime(rec) = now;
        assert(rec#nano < 1000, RichString.fromString("nano"));
        assertTrue(rec#micro < 1000);
        assertTrue(rec#milli < 1000);
        assertTrue(rec#second < 60);
        assertTrue(rec#minute < 60)
}