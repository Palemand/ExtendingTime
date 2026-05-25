pub mod Time2.DateTime.Parse {
    use Time2.DateTime
    use Time2.DateTime.Parse

    import java.time.format.DateTimeFormatter
    import java.time.ZonedDateTime

    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Parse) + IO =
        run {
            f()
        } with handler Parse {
            def parse(x, k) = 
                let jdt = ZonedDateTime.parse(x, DateTimeFormatter.ISO_ZONED_DATE_TIME);
                k(ToFlix.toFlix(jdt))
        }

    pub def runWithISO(f: Unit -> a \ ef): a \ (ef - Parse) =
        run {
            f()
        } with handler Parse {
            def parse(x, k) =
                let jdt = unsafe IO {ZonedDateTime.parse(x, DateTimeFormatter.ISO_ZONED_DATE_TIME)};
                k(ToFlix.toFlix(jdt))
        }

    pub def runWithRFC(f: Unit -> a \ ef): a \ (ef - Parse) =
        run {
            f()
        } with handler Parse {
            def parse(x, k) =
                let jdt = unsafe IO {ZonedDateTime.parse(x, DateTimeFormatter.RFC_1123_DATE_TIME)};
                k(ToFlix.toFlix(jdt))
        }
}