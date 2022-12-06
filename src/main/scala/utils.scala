package dev.wogan.advent

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}

def loadFile(filename: String): Stream[IO, String] =
  Files[IO].readUtf8Lines(Path("src/main/resources").absolute / Path(filename))

abstract class Day(number: Int) extends IOApp.Simple {

  def input: Stream[IO, String] =
    loadFile(f"day$number%02d/input.txt")

  extension (input: String)
    def lines: Stream[IO, String] =
      Stream.emits[IO, String](input.split("\n"))

  extension[A: Show] (stream: Stream[IO, A])
    def printLast: IO[Unit] =
      stream.compile.lastOrError >>= IO.println
}
