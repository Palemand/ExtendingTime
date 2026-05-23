pub mod Time2 {
    use Time2.{DateTime}
    use Time.Duration

    ///
    /// Effect representing the flow of time.
    ///
    pub eff Flow {
        def add(dt: DateTime, dur: Duration): DateTime
    }
    
    ///
    /// Effect used to access the current date and time.
    ///
    pub eff Clock {
        def now(): DateTime
    }
}