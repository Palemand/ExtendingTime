pub mod Time2 {
    use Time2.{DateTime}
    use Time.Duration

    pub eff Flow {
        def add(dt: DateTime, dur: Duration): DateTime
    }
    
    pub eff Clock {
        def now(): Time2.DateTime
    }
}