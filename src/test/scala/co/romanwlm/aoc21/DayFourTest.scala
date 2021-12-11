package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DayFourTest extends AnyFlatSpec with should.Matchers {

  "Sample file - part 1 - Bingo ! the final score " should " be 188 * 24 = 4512" in {
    DayFour.dayFourPart1("day_four_sample.txt").unsafeRunSync() should be(4512)
  }

}
