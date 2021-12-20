package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DaySevenTest extends AnyFlatSpec with should.Matchers {

  "Sample file - part 1 - " should " costs a total of 37 fuel" in {
    DaySeven.part1("day_seven_sample.txt").unsafeRunSync() should be(37)
  }

  "Sample file - part 2 - " should " costs a total of 168 fuel" in {
    DaySeven.part2("day_seven_sample.txt").unsafeRunSync() should be(168)
  }

}
