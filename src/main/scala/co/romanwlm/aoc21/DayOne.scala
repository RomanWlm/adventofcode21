package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import fs2.{Stream, hash, text}
import fs2.io.file.{Files, Path}
import co.romanwlm.aoc21.DayOne.getClass

import scala.io.Source

object DayOne extends IOApp.Simple {

  val run: IO[Unit] = dayOnePart1("day_one_0.txt")
    >> dayOnePart1("day_one_1.txt")
    >> dayOnePart2("day_one_0.txt")
    >> dayOnePart2("day_one_1.txt")
    >> IO.unit

  def dayOnePart1(inputFile: String): IO[Int] =
    for {
      path <- IO(Path(getClass.getClassLoader.getResource(inputFile).getPath))
      lst <- Utils.fileAsStream(path, f => IO(Integer.parseInt(f))).compile
        .fold((0, Int.MaxValue))((acc, value) =>
          if (acc._2 < value) (acc._1 + 1, value) else (acc._1, value)
        )
      _ <- IO.println(s"Day One - Part 1 - Sample $inputFile - found ${lst._1} increased values")
    } yield lst._1

  def dayOnePart2(inputFile: String): IO[Int] =
    for {
      path <- IO.pure(Path(getClass.getClassLoader.getResource(inputFile).getPath))
      lst <- Utils.fileAsStream(path, f => IO(Integer.parseInt(f)))
        .zipWithPreviousAndNext
        .map(v =>
          for {
            a <- v._1
            b <- Some(v._2)
            c <- v._3
          } yield a + b + c
        ).compile.fold((0, Int.MaxValue))((acc, v) =>
        v match {
          case None => (acc._1, Int.MaxValue)
          case Some(value) => if (acc._2 < value) (acc._1 + 1, value) else (acc._1, value)
        }
      )
      _ <- IO.println(s"Day One - Part 2 - Sample $inputFile - found ${lst._1} increased values")
    } yield lst._1
}
