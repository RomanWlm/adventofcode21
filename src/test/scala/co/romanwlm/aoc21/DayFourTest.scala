package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DayFourTest extends AnyFlatSpec with should.Matchers {

  "Sample file - part 1 - Bingo ! the final score " should " be 188 * 24 = 4512" in {
    DayFour.dayFourPart1("day_four_sample.txt").unsafeRunSync() should be(4512)
  }
  "Sample file - part 2 - Bingo ! the final score of worst board " should " be 148 * 13 = 1924" in {
    DayFour.dayFourPart2("day_four_sample.txt").unsafeRunSync() should be(1924)
  }

}
