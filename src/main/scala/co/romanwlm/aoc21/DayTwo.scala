package co.romanwlm.aoc21

import cats.Eval.False
import cats.effect.{Concurrent, IO, IOApp, Resource}
import fs2.{Stream, hash, text}
import fs2.io.file.{Files, Path}
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import co.romanwlm.aoc21.DayOne.getClass

import scala.io.Source

object DayTwo extends IOApp.Simple {

  val run: IO[Unit] = dayTwoPart1("day_two_sample.txt")
    >> dayTwoPart1("day_two_input.txt")
    >> dayTwoPart2("day_two_sample.txt")
    >> dayTwoPart2("day_two_input.txt")
    >> IO.unit

  enum Move:
    case UP, DOWN, FORWARD

  case class Command(move: Move, distance: Int)

  case class Position(x: Int, y: Int, aim: Int = 0) {
    lazy val product: Int = x * y

    def move(cmd: Command): Position = {
      cmd match {
        case Command(Move.UP, d) => Position(x, y - d)
        case Command(Move.DOWN, d) => Position(x, y + d)
        case Command(Move.FORWARD, d) => Position(x + d, y)
      }
    }

    def moveWithAim(cmd: Command): Position = cmd match {
      case Command(Move.UP, d) => Position(x, y, aim - d)
      case Command(Move.DOWN, d) => Position(x, y, aim + d)
      case Command(Move.FORWARD, d) => Position(x + d, y + aim * d, aim)
    }
  }

  def dayTwoPart1(inputFile: String): IO[Int] =
    for {
      path <- IO.pure(Path(getClass.getClassLoader.getResource(inputFile).getPath))
      pos: Position <- fileAsStream(path).compile
        .fold(Position(0, 0))((pos, cmd) => pos.move(cmd))
      _ <- IO.println(s"Day Two - Part 1 - Sample $inputFile - " +
        s"Position (horizontal : ${pos.x}, depth : ${pos.y}) => ${pos.product}")    } yield pos.product

  def dayTwoPart2(inputFile: String): IO[Int] =
    for {
      path <- IO.pure(Path(getClass.getClassLoader.getResource(inputFile).getPath))
      pos: Position <- fileAsStream(path).compile
        .fold(Position(0, 0))((pos, cmd) => pos.moveWithAim(cmd))
      _ <- IO.println(s"Day Two - Part 2 - Sample $inputFile - " +
        s"Position (horizontal : ${pos.x}, depth : ${pos.y}, aim: ${pos.aim}) => ${pos.product}")
    } yield pos.product

  private def fileAsStream(path: Path): Stream[IO, Command] = {
    Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .evalMap(Command.asCommand)
  }

  object Command {
    def asCommand(str: String): IO[Command] = {
      for {
        split <- IO(str.split(' '))
      } yield Command(Move.valueOf(split(0).toUpperCase), Integer.parseInt(split(1)))
    }
  }
}