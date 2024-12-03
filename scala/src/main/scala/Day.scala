package dev.wogan.advent.scala

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}

type Input = Stream[IO, String]
type Output = Stream[IO, String]

abstract class Day(val number: Int)(using val year: Year) extends IOApp.Simple {

  def part1(input: Input): Output = Stream("?")

  def part2(input: Input): Output = Stream("?")

  override def run: IO[Unit] =
    val input = loadFile(f"input/${year.value}%d/day$number%02d.txt")
    val program = for
      p1 <- part1(input)
      p2 <- part2(input)
    yield p1 -> p2
    program.compile.lastOrError >>= { case (p1, p2) =>
      IO.println(s"Part One: $p1") *> IO.println(s"Part Two: $p2")
    }

}

def loadFile(filename: String): Input =
  Files[IO].readUtf8Lines(Path(filename)).dropLastIf(_ == "")
