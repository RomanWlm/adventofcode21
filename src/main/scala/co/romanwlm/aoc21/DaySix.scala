package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import cats.implicits.*
import co.romanwlm.aoc21.DaySix.LanternFishAquarium
import fs2.io.file.{Files, Path}
import fs2.{Stream, hash, text}

import scala.annotation.tailrec
import scala.io.Source

object DaySix extends IOApp.Simple {

  val run: IO[Unit] =
    IO.println("---\n-- Day Six Part 1 - Test Sample File \n---")
      >> part1("day_six_sample.txt")
      >> IO.println("---\n-- Day Six Part 1 - Input File \n--- ")
      >> part1("day_six_input.txt")
      >> IO.println("---\n-- Day Six Part 2 - Test Sample File \n---")
      >> part2("day_six_sample.txt")
      >> IO.println("---\n-- Day Six Part 2 - Input File \n--- ")
      >> part2("day_six_input.txt")
      >> IO.unit

  type Age = Int
  type LanternFishAquarium = Array[Long]

  implicit class EnrishedLanternFishAquarium(fishes: LanternFishAquarium) {

    def newFish(age: Age): LanternFishAquarium = {
      fishes(age) = fishes(age) + 1L
      fishes
    }

    def countFishes: Long = fishes.sum

    def passDays(numberofDay: Int): LanternFishAquarium =
      numberofDay match {
        case 0 => fishes
        case _ =>
          val zeroFish = fishes(0)
          for (d <- 1 until fishes.length) {
            fishes(d - 1) = fishes(d)
          }
          fishes(8) = zeroFish
          fishes(6) = fishes(6) + zeroFish
          passDays(numberofDay - 1)
      }

    def asString: String = fishes.toList.zipWithIndex.map((count, day) => s"Day:$day Count:$count").mkString(" , ")
  }

  @tailrec
  def makeLanternFishOlder(days: Int, lanternFishes: List[Int]): List[Int] =
    days match {
      case 0 => lanternFishes
      case _ =>
        val newFishes = lanternFishes.count(f => f == 0)
        val newState = lanternFishes.map(f => if f == 0 then 6 else f - 1) ++ List.fill[Int](newFishes)(8)
        makeLanternFishOlder(days - 1, newState)
    }

  def part1(inputFile: String): IO[Int] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      inputs: List[List[Int]] <- Utils.fileAsStream[List[Int]](path, s => IO(s.split(",").map(Integer.parseInt).toList)).compile.toList
      _ <- IO.println(s"Day Six - Part 1 - $inputFile - initial state : ${inputs.head.mkString(",")}")
      finalStage <- IO(makeLanternFishOlder(80, inputs.head))
      _ <- IO.println(s"Day Six - Part 1 - $inputFile - after 80 days : ${finalStage.length} lanternfishes")
    } yield finalStage.length

  def part2(inputFile: String): IO[Long] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      inputs: List[List[Int]] <- Utils.fileAsStream[List[Int]](path, s => IO(s.split(",").map(Integer.parseInt).toList)).compile.toList
      _ <- IO.println(s"Day Six - Part 2 - $inputFile - initial state : ${inputs.head.mkString(",")}")
      lanternFishes: LanternFishAquarium <- IO(inputs.head.foldLeft[LanternFishAquarium](Array.fill[Long](9)(0))((aquarium, fish) => aquarium.newFish(fish)))
      finalStage <- IO(lanternFishes.passDays(256))
      //_ <- finalStage.fold()
      _ <- IO.println(s"Day Six - Part 2 - $inputFile - after 256 days : ${finalStage.countFishes} \nLanternfishes => ${finalStage.asString}")
    } yield finalStage.countFishes

}
