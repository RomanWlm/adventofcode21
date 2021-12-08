package co.romanwlm.aoc21

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats.effect.unsafe.implicits.global

class DayTwoTest extends AnyFlatSpec with should.Matchers {

  "Sample file - moving rules 1 - " should "have final position as 150" in {
    DayTwo.dayTwoPart1("day_two_sample.txt").unsafeRunSync() should be(150)
  }

  "Sample file - moving rules 2 - " should "have final position as 900" in {
    DayTwo.dayTwoPart2("day_two_sample.txt").unsafeRunSync() should be(900)
  }
}
