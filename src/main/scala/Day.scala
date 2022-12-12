package dev.wogan.advent

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.Stream

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
