package dev.wogan.advent.scala

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}

type Input = Stream[IO, String]
type Output = Stream[IO, String]

abstract class Day(val number: Int) extends IOApp.Simple {

  def part1(input: Input = input): Output = Stream("???")

  def part2(input: Input = input): Output = Stream("???")

  def input: Input =
    loadFile(f"day$number%02d.txt")

  override def run: IO[Unit] =
    val x = for {
      p1 <- part1()
      p2 <- part2()
    } yield p1 -> p2
    x.compile.lastOrError >>= { case (p1, p2) =>
      IO.println(s"Part One: $p1") *> IO.println(s"Part Two: $p2")
    }

}

def loadFile(filename: String): Input =
  Files[IO].readUtf8Lines(Path("src/main/resources").absolute / Path(filename))
