# Playing with advent of code 21's edition

### [Day One](src/main/scala/co/romanwlm/aoc21/DayOne.scala)

#### Part 1

_Use stream and fold's accumulator to keep track of previous value and count on condition_

```
Day One - Part 1 - Sample day_one_0.txt - found 7 increased values
Day One - Part 1 - Sample day_one_1.txt - found 1502 increased values
```

#### Part 2

_Use stream as part 1 and zipWithPreviousNext + reduce them by sum and fold's accumulator technique._

```
Day One - Part 2 - Sample day_one_0.txt - found 5 increased values
Day One - Part 2 - Sample day_one_1.txt - found 1538 increased values
```

### [Day Two](src/main/scala/co/romanwlm/aoc21/DayTwo.scala)

_Stream based - Define here Command and Position data types with combination. For second part Just added aim attribute to position type and adapted move's logic_

#### Part 1
```
Day Two - Part 1 - Sample day_two_sample.txt - Position (horizontal : 15, depth : 10) => 150
Day Two - Part 1 - Sample day_two_input.txt - Position (horizontal : 1990, depth : 1000) => 1990000
```

#### Part 2
```
Day Two - Part 2 - Sample day_two_sample.txt - Position (horizontal : 15, depth : 60, aim: 10) => 900
Day Two - Part 2 - Sample day_two_input.txt - Position (horizontal : 1990, depth : 992674, aim: 1000) => 1975421260
```