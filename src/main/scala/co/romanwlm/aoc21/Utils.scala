package co.romanwlm.aoc21

import cats.effect.IO
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

object Utils {
  def fileAsStream[T](path: Path, f: String => IO[T]): Stream[IO, T] =
    Files[IO].readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .evalMap(f)
}
