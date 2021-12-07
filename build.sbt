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

// https://github.com/sbt/sbt/issues/3963
run := Defaults.runTask(fullClasspath in Runtime, mainClass in run in Compile, runner in run).evaluated

