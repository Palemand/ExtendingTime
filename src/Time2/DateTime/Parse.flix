pub mod Time2.DateTime.Parse {
    use IoError.{ErrorKind, IoError}
    use Time2.DateTime
    use Time2.DateTime.Parse
    import java.lang.Exception
    import java.time.format.DateTimeFormatter
    import java.time.ZonedDateTime


    ///
    /// Runs the parse effect by parsing with ISO 8601 standard.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Parse) + IO =
        run {
            f()
        } with handler Parse {
            def parse(x, k) = 
                let s = try {
                    Ok(ToFlix.toFlix(ZonedDateTime.parse(x, DateTimeFormatter.ISO_ZONED_DATE_TIME)))
                } catch {
                    case e: Exception => Err(IoError(ErrorKind.Other, e.toString()))
                };
                k(s)
        }
    ///
    /// Runs the parse effect by parsing with ISO 8601 standard.
    ///
    pub def runWithISO(f: Unit -> a \ ef): a \ (ef - Parse) =
        run {
            f()
        } with handler Parse {
            def parse(x, k) = 
                let s = unsafe IO { try {
                    Ok(ToFlix.toFlix(ZonedDateTime.parse(x, DateTimeFormatter.ISO_ZONED_DATE_TIME)))
                } catch {
                    case e: Exception => Err(IoError(ErrorKind.Other, e.toString()))
                }};
                k(s)
        }
    ///
    /// Runs the parse effect by parsing with rfc 1123 standard.
    ///
    pub def runWithRFC(f: Unit -> a \ ef): a \ (ef - Parse) =
        run {
            f()
        } with handler Parse {
            def parse(x, k) = 
                let s = unsafe IO { try {
                    Ok(ToFlix.toFlix(ZonedDateTime.parse(x, DateTimeFormatter.RFC_1123_DATE_TIME)))
                } catch {
                    case e: Exception => Err(IoError(ErrorKind.Other, e.toString()))
                }};
                k(s)
        }
    ///
    /// Runs the parse effect by parsing with a user defined pattern.
    /// Follows convention described at https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
    ///
    pub def runWithPattern(pattern: String, f: Unit -> a \ ef): a \ ef - Parse =
        let formatter = unsafe IO { try {
            Ok(DateTimeFormatter.ofPattern(pattern))
        } catch {
            case e: Exception => Err(IoError(ErrorKind.Other, e.toString()))
        }};
        run {
            f()
        } with handler Parse {
            def parse(x, k) = 
                match formatter {
                    case Err(t) => k(Err(t))
                    case Ok(fmt) => {
                        let s = unsafe IO { try {
                            Ok(ToFlix.toFlix(ZonedDateTime.parse(x, fmt)))
                        } catch {
                            case e: Exception => Err(IoError(ErrorKind.Other, e.toString()))
                        }};
                        k(s)
                    }
                }
        }
}