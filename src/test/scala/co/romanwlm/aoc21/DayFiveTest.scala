package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DayFiveTest extends AnyFlatSpec with should.Matchers {

  "Sample file - part 1 - " should " have 5 intersections " in {
    DayFive.dayFivePart1("day_five_sample.txt").unsafeRunSync() should be(5)
  }

  "Sample file - part 2 - " should " have 12 intersections " in {
    DayFive.dayFivePart2("day_five_sample.txt").unsafeRunSync() should be(12)
  }

}
