pub mod Time2.DateTime.Parse {
    use IoError.ErrorKind
    use IoError.IoError
    use Time2.DateTime
    use Time2.DateTime.Parse
    import java.lang.Exception
    import java.time.format.DateTimeFormatter
    import java.time.ZonedDateTime

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