package co.romanwlm.aoc21

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats.effect.unsafe.implicits.global

class DayThreeTest extends AnyFlatSpec with should.Matchers {

  "Sample file - part 1 - " should " have gamma * epsilon = 198" in {
    DayThree.dayThreePart1("day_three_sample.txt").unsafeRunSync() should be(198)
  }
  "Sample file - part 2 - " should " have O2 Rating * CO2 Rating = 230" in {
    DayThree.dayThreePart2("day_three_sample.txt").unsafeRunSync() should be(230)
  }
}
