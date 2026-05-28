pub mod TestClock {
    use Assert.assertEq
    use Time2.Flow
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
    @Test
    pub def testClock02(): Unit \ Assert + IO = 
        region rc {
            let rf = Ref.fresh(rc, 0);
            run {
                let fst = Clock.now();
                let snd = Clock.now();
                assertEq(expected = Time.Duration.days(-1), DateTime.difference(fst, snd))
            } with handler Clock {
                def now(k) = {
                    Ref.transform(x -> x+1, rf);
                    k(Time2.DateTime.getDate(1969, 1, Ref.get(rf)))
                }
            }
        }
    @Test
    pub def testWithFlow01(): Unit \ Assert =
        region rc {
            let rf = Ref.fresh(rc, 0);
            run {
                let fst = Clock.now();
                //2 dage
                let snd = Clock.now();
                //2 dage
                let third = Clock.now();
                assertEq(expected = Time.Duration.days(-4), DateTime.difference(fst, snd));
                assertEq(expected = Time.Duration.days(-16), DateTime.difference(fst, third))
            } with Clock.withFlow
            with Flow.withFunction(x -> x*x, Time2.TimeUnit.Days)
            with Flow.runWithPure
            with handler Clock {
                def now(k) = {
                    Ref.transform(x -> x+2, rf);
                    k(Time2.DateTime.getDate(1969, 1, Ref.get(rf)))
                }
            }
        }
}