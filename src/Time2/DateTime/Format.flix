pub mod Time2.DateTime.Format {
    use Time2.DateTime
    use Time2.DateTime.Format

    import java.time.format.DateTimeFormatter
    import java.time.ZonedDateTime
    import java.lang.Exception

    ///
    /// Sets `DateTime` format according to ISO 8601 
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Format) + IO =
        run {
            f()
        } with handler Format {
            def format(dt, k) = 
                let jdt = ToJava.toJava(dt);
                k(jdt.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
        }

    ///
    /// Sets `DateTime` format according to ISO 8601
    ///
    pub def runWithISO(f: Unit -> a \ ef): a \ (ef - Format) =
        run {
            f()
        } with handler Format {
            def format(dt, k) = 
                let jdt = ToJava.toJava(dt);
                k(unsafe IO {jdt.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)})
        }

    ///
    /// Sets `DateTime` format according to RFC 1123
    ///
    pub def runWithRfc(f: Unit -> a \ ef): a \ (ef - Format) =
        run {
            f()
        } with handler Format {
            def format(dt, k) = 
                let jdt = ToJava.toJava(dt);
                k(unsafe IO {jdt.format(DateTimeFormatter.RFC_1123_DATE_TIME)})
        }
    ///
    /// Sets `DateTime` format based on `pattern`. 
    /// Follows convention described at https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
    ///
    pub def runWithPattern(pattern: String, f: Unit -> a \ ef): a \ ef - Format =
        let formatter = unsafe IO { try {
            Some (DateTimeFormatter.ofPattern(pattern))
        } catch {
            case _: Exception => None
        }};
        run {
            f()
        } with handler Format {
            def format(dt, k) = 
                let jdt = ToJava.toJava(dt);
                let fmt = match formatter {
                    case None => DateTimeFormatter.ISO_ZONED_DATE_TIME
                    case Some(x) => x
                };
                k(unsafe IO {jdt.format(fmt)})
        }
}