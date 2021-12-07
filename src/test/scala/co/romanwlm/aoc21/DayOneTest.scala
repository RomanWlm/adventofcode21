package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import org.scalatest._
import flatspec._
import matchers._

class DayOneTest extends AnyFlatSpec with should.Matchers {

  "Sample file" should "have 7 values increased" in {
    DayOne.dayOnePart1("day_one_0.txt").unsafeRunSync() should be (7)
  }
}
