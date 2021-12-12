package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DaySixTest extends AnyFlatSpec with should.Matchers {

  "Sample file - part 1 - " should " have 5934 lantern fishes after 80 days " in {
    DaySix.part1("day_six_sample.txt").unsafeRunSync() should be(5934)
  }

  "Sample file - part 2 - " should " have 26984457539 (Long) fishes after 256 days " in {
    DaySix.part2("day_six_sample.txt").unsafeRunSync() should be(26984457539L)
  }
}
