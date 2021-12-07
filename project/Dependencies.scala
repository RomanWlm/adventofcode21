import sbt._

object Dependencies {
  lazy val scalatestVersion = "3.2.9"
  lazy val fs2Version = "3.2.0"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion
  lazy val fs2Core = "co.fs2" %% "fs2-core" % fs2Version
  // optional I/O library
  lazy val fs2Io = "co.fs2" %% "fs2-io" % fs2Version

  lazy val catsCore = "org.typelevel" %% "cats-effect" % "3.3.0"
}