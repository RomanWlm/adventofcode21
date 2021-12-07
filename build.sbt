import Dependencies._

ThisBuild / scalaVersion := "3.1.0"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "co.romanwlm"
ThisBuild / organizationName := "romanwlm"

lazy val aoc21 = (project in file("."))
  .settings(
    name := "AOC21",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += fs2Core,
    libraryDependencies += fs2Io,
    libraryDependencies += catsCore
  )
