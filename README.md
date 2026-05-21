# ExtendingTime
This project is working to extend the Time module in Flix into a fully capable and usable module.

It is a part of a Bachelors assignment from the 3 contributors.

## Goals
* Be able as a use to get a timestamp without resorting to Java code.
* Do calculations between timestamps in a conventional way
* Be able to modify the flow of time using depency injections through effes.
* more ...

## General idea structure
### Time
- TimeUnit
- TimeStamp enum
    - Date
    - Time
- Clock effect
    - now() operation

- Flow effect
    - Some operation to model an alternate "change" in time.