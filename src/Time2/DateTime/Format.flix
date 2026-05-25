pub mod Time2.DateTime.Format {
    use Time2.DateTime
    use Time2.DateTime.Format

    import java.time.format.DateTimeFormatter
    import java.time.ZonedDateTime

    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Format) + IO =
        run {
            f()
        } with handler Format {
            def format(dt, k) = 
                let jdt = ToJava.toJava(dt);
                k(jdt.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
        }

}