package dev.wogan.advent.scala
package year2024

import cats.effect.IO
import cats.parse.{Numbers, Parser}
import fs2.Stream
import cats.syntax.all.*

object Day01 extends Day(1) {

  val parser = Numbers.digits.map(_.toInt) ~ (Parser.char(' ').rep *> Numbers.digits.map(_.toInt))

  def separate(input: Input): (Stream[IO, Int], Stream[IO, Int]) =
    input.flatMap { string =>
      parser.parseAllStream(string)
    }.unzip

  override def part1(input: Input): Output =
    val (a, b) = separate(input)
    a.sorted.zip(b.sorted).map((a, b) => Math.abs(a - b)).sum

  override def part2(input: Input): Output =
    val (a, b) = separate(input)
    val scores = for
      map <- Stream.eval(b.compile.toList.map(_.groupMapReduce(identity)(_ => 1)(_ + _)))
      i <- a
    yield i * map.getOrElse(i, 0)
    scores.sum

}
