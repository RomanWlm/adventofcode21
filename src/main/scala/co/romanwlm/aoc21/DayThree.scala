package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import fs2.io.file.{Files, Path}
import fs2.{Stream, hash, text}

import java.math.MathContext
import scala.collection.BitSet
import scala.io.Source
import scala.math.BigDecimal.RoundingMode

object DayThree extends IOApp.Simple {

  val run: IO[Unit] =
    dayThreePart1("day_three_sample.txt")
      >> dayThreePart1("day_three_input.txt")
      >> dayThreePart2("day_three_sample.txt")
      >> dayThreePart2("day_three_input.txt")
      >> IO.unit

  case class SignalRecorder(sumSignal: Array[Int] = Array[Int](), count: Int = 0) {
    def record(value: Array[Int]): SignalRecorder = SignalRecorder(SignalRecorder.sum(sumSignal, value), count + 1)
  }

  enum ReportType:
    case O2, CO2

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

  def dayThreePart2(inputFile: String): IO[Int] =
    for {
      path <- resourceAsPath(inputFile)
      binarySignalSummary <- Utils.fileAsStream[Array[Int]](path, str => asIntArray(str)).compile
        .fold(SignalRecorder())((rec: SignalRecorder, value: Array[Int]) => rec.record(value))
        .flatMap(asBinary)
      retained: List[Array[Boolean]] <-
        Utils.fileAsStream[Array[Int]](path, str => asIntArray(str)).evalMap(asBinary).compile.toList
      _ <- IO.println(s" ${Console.GREEN} Compute O2 Report... ${Console.RESET}")
      // O2 Rating report
      O2RatingBinary: Array[Boolean] <- diagnosticReport(binarySignalSummary, retained, ReportType.O2)
      O2RatingBStr <- asBinaryString(O2RatingBinary)
      O2Rating <- binaryStringToInt(O2RatingBStr)

      _ <- IO.println(s" ${Console.BLUE} Compute CO2 Report... ${Console.RESET}")
      // C2O Rating report
      CO2RatingBinary: Array[Boolean] <- diagnosticReport(binarySignalSummary, retained, ReportType.CO2)
      CO2RatingBStr <- asBinaryString(CO2RatingBinary)
      CO2Rating <- binaryStringToInt(CO2RatingBStr)

      _ <- IO.println(s"Day Three - Part 2 - Sample $inputFile - " +
        s"Reported O2($O2RatingBStr) - CO2($CO2RatingBStr) => $O2Rating * $CO2Rating = ${O2Rating * CO2Rating}")
    } yield O2Rating * CO2Rating

  def diagnosticReport(summary: Array[Boolean], signals: List[Array[Boolean]], reportType: ReportType, indexOfBit: Int = 0): IO[Array[Boolean]] =
    signals match {
      case el :: Nil => IO(el)
      case _ =>
        for {
          // Filter O2 or CO2 indicator
          candidates: List[Array[Boolean]] <- IO(signals.filter(sig =>
            reportType match {
              case ReportType.O2 => sig(indexOfBit) == summary(indexOfBit)
              case ReportType.CO2 => sig(indexOfBit) != summary(indexOfBit)
            }))
          // Recompute most common bits for next iteration
          summaryNext: Array[Boolean] <- asBinary(candidates.foldLeft[SignalRecorder](SignalRecorder())
            ((recorder: SignalRecorder, value: Array[Boolean]) => recorder.record(asIntArray(value))))
          _ <- IO.println(
            s"Position $indexOfBit - " +
              s"Bit retained to discriminate [${Console.MAGENTA}${if summaryNext(indexOfBit) then '1' else '0'}${Console.RESET}] - " +
              s"Candidates = ${Console.YELLOW}${candidates.map(ar => String(ar.map(v => if v then '1' else '0'))).mkString(",")}${Console.RESET}"
          )
          v <- diagnosticReport(summaryNext, candidates, reportType, indexOfBit + 1)
        } yield v
    }

  def asIntArray(line: String): IO[Array[Int]] = IO(line.toCharArray.map(l => Integer.parseInt(l.toString)))

  def asIntArray(array: Array[Boolean]): Array[Int] = array.map(v => if v then 1 else 0)

  def asBinary(rec: SignalRecorder): IO[Array[Boolean]] =
    IO(rec.sumSignal.map(v => v >= BigDecimal(rec.count) / (BigDecimal(2)).setScale(0, RoundingMode.UP).intValue))

  def asBinary(arr: Array[Int]): IO[Array[Boolean]] = IO(arr.map(v => v > 0))

  def asBinaryString(barr: Array[Boolean]): IO[String] = IO(String(barr.map(v => if v then '1' else '0')))

  def binaryStringToInt(binaryStr: String): IO[Int] = IO(Integer.parseInt(binaryStr, 2))

  def oneSComplementOf(arr: Array[Boolean]): IO[Array[Boolean]] = IO(arr.map(v => !v))

  def resourceAsPath(inputFile: String): IO[Path] = IO(Path(getClass.getClassLoader.getResource(inputFile).getPath))

  object SignalRecorder {
    def sum(left: Array[Int], right: Array[Int]): Array[Int] = if left.isEmpty then right else left.zip(right).map((l, r) => l + r)
  }
}