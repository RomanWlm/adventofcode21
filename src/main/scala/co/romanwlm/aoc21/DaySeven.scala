package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import cats.implicits.*
import co.romanwlm.aoc21.DaySeven.resolveFixedCost
import fs2.io.file.{Files, Path}
import fs2.{Stream, hash, text}

import scala.annotation.tailrec
import scala.io.Source

object DaySeven extends IOApp.Simple {

  val run: IO[Unit] =
    IO.println("---\n-- Day seven Part 1 - Test Sample File \n---")
      >> part1("day_seven_sample.txt")
      >> IO.println("---\n-- Day seven Part 1 - Input File \n--- ")
      >> part1("day_seven_input.txt")
      >> IO.println("---\n-- Day seven Part 2 - Test Sample File \n---")
      >> part2("day_seven_sample.txt")
      >> IO.println("---\n-- Day seven Part 2 - Input File \n--- ")
      >> part2("day_seven_input.txt")
      >> IO.unit

  def resolveFixedCost(positions: List[Int], position: Int, maxPosition: Int, costToMinimize: Int): IO[Int] =
    position match
      case pos if position > maxPosition => IO.pure(costToMinimize)
      case pos => for {
        cost <- IO(positions.map(v => Math.abs(v - pos)).sum)
        result <- if cost > costToMinimize then IO(costToMinimize) else resolveFixedCost(positions, pos + 1, maxPosition, cost)
      } yield result

  def resolveWithLinearCost(positions: List[Int], position: Int, maxPosition: Int, costToMinimize: Int): IO[Int] =
      position match
        case pos if position > maxPosition => IO.pure(costToMinimize)
        case pos => for {
          cost <- IO(positions.map(v => {
            val dist = Math.abs(v - pos)
            (dist * (dist + 1)) / 2
          }).sum)
          result <- if cost > costToMinimize then IO(costToMinimize) else resolveWithLinearCost(positions, pos + 1, maxPosition, cost)
        } yield result

  def part1(inputFile: String): IO[Int] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      inputs: List[List[Int]] <- Utils.fileAsStream[List[Int]](path, s => IO(s.split(",").map(Integer.parseInt).toList)).compile.toList
      min <- IO(inputs.head.min)
      max <- IO(inputs.head.max)
      result <- resolveFixedCost(inputs.head, min, max, Integer.MAX_VALUE)
      _ <- IO.println(s"All crabs moves results to costs a total of $result fuel. This is the cheapest possible outcome.")
    } yield result

  def part2(inputFile: String): IO[Long] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      inputs: List[List[Int]] <- Utils.fileAsStream[List[Int]](path, s => IO(s.split(",").map(Integer.parseInt).toList)).compile.toList
      min <- IO(inputs.head.min)
      max <- IO(inputs.head.max)
      result <- resolveWithLinearCost(inputs.head, min, max, Integer.MAX_VALUE)
      _ <- IO.println(s"All crabs moves results to costs a total of $result fuel. This is the cheapest possible outcome.")
    } yield result

}
