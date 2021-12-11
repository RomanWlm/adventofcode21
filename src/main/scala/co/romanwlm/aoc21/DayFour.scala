package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import cats.implicits._

import fs2.io.file.{Files, Path}
import fs2.{Stream, hash, text}

import java.math.MathContext
import scala.collection.BitSet
import scala.io.Source

import scala.annotation.tailrec

object DayFour extends IOApp.Simple {

  val run: IO[Unit] =
    IO.println("---\n-- Day Four Part 1 - Test Sample File \n---")
      >> dayFourPart1("day_four_sample.txt")
      >> IO.println("---\n-- Day Four Part 1 - Input File \n--- ")
      >> dayFourPart1("day_four_input.txt")
      >> IO.unit

  type Board = Array[Array[Int]]

  case class BoardScore(won: Boolean, score: Int, remainingDraws: Int)

  implicit class RichBoard(board: Board) {

    def asString: String = board.toList.map(_.toList).map(lst => s"${lst.mkString("|")}\n").mkString

    def score(draw: List[Int]): IO[BoardScore] = scoreBoard(draw, extractColumns ++ extractRows)

    @tailrec
    private def scoreBoard(draw: List[Int], remainingBoard: List[List[Int]]): IO[BoardScore] = {
      draw match {
        case currentDraw :: nextDraws =>
          val remaining = remainingBoard.map(_.filter(_ != currentDraw))
          remaining.find(_.isEmpty) match {
            case Some(empty) =>
              val sumRemaining = sumMatrix(remaining) / 2 // Divide by two as columns and rows has been duplicated
              IO.println(s"Board has met${Console.CYAN} bingo ${Console.YELLOW}=>${Console.RESET} " +
                s"Draw=$currentDraw Sum=$sumRemaining RemainingDraws=${nextDraws.length} " +
                s"Score=${sumRemaining * currentDraw}")
                >> IO(BoardScore(true, sumRemaining * currentDraw, nextDraws.length))
            case None => scoreBoard(nextDraws, remaining)
          }
        case _ => IO(BoardScore(false, 0, 0))
      }

    }

    def sumMatrix(sparseMatrix: List[List[Int]]): Int = sparseMatrix.map(_.sum).sum

    private def walkRows(row: Int, col: Int): List[Int] =
      if row >= board.length then Nil else board(row)(col) :: walkRows(row + 1, col)

    /**
     * Extract all columns as list of column (List of Int)
     */
    def extractColumns: List[List[Int]] = (for (col <- board(0).indices) yield walkRows(0, col)).toList

    /**
     * Extract all rows as list of rows (List of Int)
     */
    def extractRows: List[List[Int]] = board.toList.map(_.toList)
  }

  def dayFourPart1(inputFile: String): IO[Int] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      st: List[String] <- Files[IO].readAll(path)
        .through(text.utf8.decode)
        .through(text.lines)
        .filter(_.nonEmpty).compile.toList
      // Read draws
      draw <- IO(st.head.split(",").map(Integer.parseInt).toList)
      // Read boards
      boards <- IO(st.drop(1).map(_.split(" ").filter(_.nonEmpty).map(Integer.parseInt)).toArray.grouped(5).toArray)

      _ <- IO.println(s"Draws : ${draw.length} values => ${draw.mkString(",")}\n" +
        s"All Boards (${boards.length}) : \n${boards.map(_.asString).mkString("\n")}")

      // Compute all results and retain the best one (order by keeping the one who has maximum remaining draws
      result: List[BoardScore] <- boards.map(board => board.score(draw)).toList.sequence
      best: BoardScore <- IO(result.maxBy(_.remainingDraws))
      _ <- IO.println(s"Best board won at draw number ${draw.length - best.remainingDraws} with score=${best.score}")
    } yield best.score
}