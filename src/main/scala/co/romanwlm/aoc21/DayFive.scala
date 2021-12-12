package co.romanwlm.aoc21

import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, IO, IOApp, Resource}
import cats.implicits.*
import fs2.io.file.{Files, Path}
import fs2.{Stream, hash, text}

import scala.annotation.tailrec
import scala.io.Source

object DayFive extends IOApp.Simple {

  val run: IO[Unit] =
    IO.println("---\n-- Day Five Part 1 - Test Sample File \n---")
      >> dayFivePart1("day_five_sample.txt")
      >> IO.println("---\n-- Day Five Part 1 - Input File \n--- ")
      >> dayFivePart1("day_five_input.txt")
      >> IO.println("---\n-- Day Five Part 2 - Test Sample File \n---")
      >> dayFivePart2("day_five_sample.txt")
      >> IO.println("---\n-- Day Five Part 2 - Input File \n--- ")
      >> dayFivePart2("day_five_input.txt")
      >> IO.unit

  case class Line(p1: Point, p2: Point)

  case class Point(x: Int, y: Int)

  type Plan = Array[Array[Int]]

  implicit class EnrichedLine(line: Line) {
    def asString: String = s"${line.p1.x},${line.p1.y} -> ${line.p2.x},${line.p2.y}"

    def isHorizontal: Boolean = line.p1.y == line.p2.y

    def isVertical: Boolean = line.p1.x == line.p2.x

    def isDiagonal: Boolean = Math.abs(line.p1.x - line.p2.x) == Math.abs(line.p1.y - line.p2.y)

    def intersect(x: Int, y: Int): Boolean =
      if x == line.p1.x then (y <= line.p1.y && y >= line.p2.y) || (y <= line.p2.y && y >= line.p1.y)
      else if y == line.p1.y then (x <= line.p1.x && x >= line.p2.x) || (x <= line.p2.x && x >= line.p1.x)
      else false

    def allPoints: List[Point] =
      if isHorizontal then
        def nextHorizontalPoints(current: Point, maxX: Int): List[Point] =
          current match {
            case Point(x, y) if x == maxX => List(current)
            case Point(x, y) => current :: nextHorizontalPoints(Point(x + 1, y), maxX)
          }

        nextHorizontalPoints(line.minXPoint, line.maxX)
      else if isVertical then
        def nextVerticalPoints(current: Point, maxY: Int): List[Point] =
          current match {
            case Point(x, y) if y == maxY => List(current)
            case Point(x, y) => current :: nextVerticalPoints(Point(x, y + 1), maxY)
          }

        nextVerticalPoints(line.minYPoint, line.maxY)
      else // diagonal case

        def nextDiagonalPoints(current: Point, target: Point): List[Point] =
          current match {
            case Point(x, y) if x == target.x && y == target.y => List(current)
            case Point(x, y) => current :: nextDiagonalPoints(
              Point(if x > target.x then x - 1 else x + 1, if y > target.y then y - 1 else y + 1), target)
          }

        nextDiagonalPoints(line.p1, line.p2)

    def maxX: Int = Math.max(line.p1.x, line.p2.x)

    def minX: Int = Math.min(line.p1.x, line.p2.x)

    def minXPoint: Point = if line.p1.x < line.p2.x then line.p1 else line.p2

    def minYPoint: Point = if line.p1.y < line.p2.y then line.p1 else line.p2

    def maxY: Int = Math.max(line.p1.y, line.p2.y)

    def minY: Int = Math.min(line.p1.y, line.p2.y)
  }

  implicit class EnrichedPlan(plan: Plan) {
    def asString: String = plan.map(_.map(v => if v == 0 then "." else Integer.toString(v)).mkString(" ")).mkString("\n")

    def intersections: Int = plan.toList.map(_.toList.count(_ > 1)).sum
  }

  def asLine(str: String): IO[Line] =
    for {
      spt <- IO(str.split("->").map(_.split(",").map(_.trim)))
    } yield Line(
      Point(Integer.parseInt(spt(0)(0)), Integer.parseInt(spt(0)(1))),
      Point(Integer.parseInt(spt(1)(0)), Integer.parseInt(spt(1)(1)))
    )

  def asPlan(lines: List[Line]): Plan = {
    val maxX = lines.maxBy(_.maxX).maxX
    val maxY = lines.maxBy(_.maxY).maxY
    val matrix = Array.ofDim[Int](maxY + 1, maxX + 1)
    lines.foreach(line => line.allPoints.foreach(p => matrix(p.y)(p.x) = matrix(p.y)(p.x) + 1))
    matrix
  }

  def dayFivePart1(inputFile: String): IO[Int] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      inputs: List[Line] <- Utils.fileAsStream(path, s => asLine(s)).compile.toList
      plan: Plan <- IO(asPlan(inputs.filter(l => l.isVertical || l.isHorizontal)))
      _ <- IO.println(s"Lines : \n${inputs.map(l => s"${l.asString}\n").mkString}")
      _ <- IO.println(s"Plan : \n${plan.asString}")
      intersections <- IO(plan.intersections)
      _ <- IO.println(s"Day Five - Part 1 - $inputFile - considering horizontal and vertical" +
        s" lines, has found ${plan.intersections} intersections")
    } yield intersections

  def dayFivePart2(inputFile: String): IO[Int] =
    for {
      path <- Utils.resourceAsPath(inputFile)
      inputs: List[Line] <- Utils.fileAsStream(path, s => asLine(s)).compile.toList
      plan: Plan <- IO(asPlan(inputs.filter(l => l.isVertical || l.isHorizontal || l.isDiagonal)))
      _ <- IO.println(s"Lines : \n${inputs.map(l => s"${l.asString}\n").mkString}")
      _ <- IO.println(s"Plan : \n${plan.asString}")
      intersections <- IO(plan.intersections)
      _ <- IO.println(s"Day Five - Part 2 - $inputFile - considering horizontal, vertical and diagonal" +
        s" lines, has found ${plan.intersections} intersections")
    } yield intersections
}
