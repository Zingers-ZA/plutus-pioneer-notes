https://www.youtube.com/watch?v=LPzwMqOnWvk&list=PLNEK_Ejlx3x2zXSjHRKLSc5Jn9vJFA3_O&index=2&ab_channel=IOGAcademy
- validator script should never need to fail under normal circumstances
- this is because the script can be validated under exactly the same circumstances off chain before submitting to the chain
- meaning the wallet can always validate that a transaction is valid before submitting

## txInfoValidRange

### validating scripts off chain presents a challenge though:
- the time at which the script is validated off chain, may be different than the time the validator is executed when the transaction arrives at a node

- this is solved by a field inside `TxInfo` called `txInfoValidRange`
  - type of `POSIXTimeRange`
  - it is a timespan in which the transaction will be valid
  - specified in the transaction
  - validation fails immediately(before validator script execution) if a node evaluates a transaction and the current time does not fall within the transactions valid range
  - <b>this means that you can always assume that inside validator code, the current time is within the transaction valid time</b>
  - <b>by default</b> all transactions use an infinite valid time


- Oroborous(the cardano consensus mech) uses slots to count time, not time
- much easier to use POSIX(UNIX) time, so there are conversion functions

> <b>side note</b> :  
>Using slot presents a complication, because slot length is a chain param that can change.   
>This means that transactions should not be created with valid time ranges that are too far in the future, or rather, further >than the farthest time that you are sure the slot length will be the same as it currently is. Currently this is `36 hours`.  


## POSIXTimeRange:
- base type is `Interval POSIXTime`

### POSIX time:
- number of ms since 1970-01-01

### Interval:
- consists of `UpperBound` and `LowerBound`
 - LowerBound and UpperBound can either be -Infinity, +Infinity or a specific value 

- various functions:
  - `member :: a -> Interval a -> Bool` returns bool for whether a time is within an interval
  - `from :: a -> Interval a` returns an interval that starts at your interval and ends at +Infinity
  - `to :: a -> Interval a` returns an interval that starts at -Infinity and ends at your interval
  - `hull :: .. Interval a -> Interval b -> Interval ab` returns the smallest interval that contains both interval a and b
  - `intersection :: .. Interval a -> Interval b -> Interval ab` returns the biggest overlapping interval within a and b
  - `overlaps :: .. -> Interval a -> Interval b -> Bool` returns whether there are intervals that are members of both
  - `contains :: .. -> Interval a -> Interval b -> Bool` whether the entire of b is contained within a
  - `before :: .. a -> Interval b -> Bool` check whether value a is before the start of Interval b
  - `after :: .. a -> Interval b -> Bool` check whether value a is after the end of Interval b