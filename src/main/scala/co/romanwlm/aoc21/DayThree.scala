package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import fs2.io.file.{Files, Path}
import fs2.{Stream, hash, text}

import scala.collection.BitSet
import scala.io.Source

object DayThree extends IOApp.Simple {

  val run: IO[Unit] =
    dayThreePart1("day_three_sample.txt")
    >> dayThreePart1("day_three_input.txt")
    >> IO.unit

  case class SignalRecorder(sumSignal: Array[Int] = Array[Int](), count: Int = 0) {
    def record(value: Array[Int]): SignalRecorder = SignalRecorder(SignalRecorder.sum(sumSignal, value), count + 1)
  }

  def dayThreePart1(inputFile: String): IO[Int] =
    for {
      path <- resourceAsPath(inputFile)
      binarySignal <- Utils.fileAsStream[Array[Int]](path, str => asIntArray(str)).compile
        .fold(SignalRecorder())((rec: SignalRecorder, value: Array[Int]) => rec.record(value))
        .flatMap(asBinary)
      onesComplement <- oneSComplementOf(binarySignal)
      gammaBin <- asBinaryString(binarySignal)
      epsilonBin <- asBinaryString(onesComplement)
      gamma <- binaryStringToInt(gammaBin)
      epsilon <- binaryStringToInt(epsilonBin)
      _ <- IO.println(s"Day Three - Part 1 - Sample $inputFile - ($gammaBin / $gamma) x ($epsilonBin / $epsilon) => ${gamma * epsilon}")
    }
    yield gamma * epsilon

  def asIntArray(line: String): IO[Array[Int]] = IO(line.toCharArray.map(l => Integer.parseInt(l.toString)))

  def asBinary(rec: SignalRecorder): IO[Array[Boolean]] = IO(rec.sumSignal.map(v => v > (rec.count / 2)))

  def asBinaryString(barr: Array[Boolean]): IO[String] = IO(String(barr.map(v => if v then '1' else '0')))

  def binaryStringToInt(binaryStr: String): IO[Int] = IO(Integer.parseInt(binaryStr, 2))

  def oneSComplementOf(arr: Array[Boolean]): IO[Array[Boolean]] = IO(arr.map(v => !v))

  def resourceAsPath(inputFile: String): IO[Path] = IO(Path(getClass.getClassLoader.getResource(inputFile).getPath))

  object SignalRecorder {
    def sum(left: Array[Int], right: Array[Int]): Array[Int] = if left.isEmpty then right else left.zip(right).map((l, r) => l + r)
  }
}
