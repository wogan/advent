package dev.wogan.advent.scala
package year2024

import cats.syntax.all.*
import cats.parse.Parser

import scala.annotation.targetName

object Day07 extends Day(7) {

  val parser: Parser[Equation] = (Parsers.long <* Parser.string(": ")) ~ Parsers.long.repSep(Parser.char(' ')) map { (a, b) =>
    Equation(a, b.toList)
  }

  extension (n: Long)
    @targetName("concat")
    infix def ||(m: Long): Long =
      (n.toString + m.toString).toLong

  case class Equation(result: Long, values: List[Long]):
    def validate: Boolean =
      def loop(s: Long, remaining: List[Long]): List[Long] =
        remaining match
          case Nil => List(s)
          case a :: Nil => List(s + a, s * a)
          case a :: tail => loop(s + a, tail) ++ loop(s * a, tail)
      loop(values.head, values.tail).contains(result)

    def validatePart2: Boolean =
      def loop(s: Long, remaining: List[Long]): List[Long] =
        remaining match
          case Nil => List(s)
          case a :: Nil => List(s + a, s * a, s || a)
          case a :: tail => loop(s + a, tail) ++ loop(s * a, tail) ++ loop(s || a, tail)
      loop(values.head, values.tail).contains(result)

  override def part1(input: Input): Output =
    input
      .flatMap(parser.parseAllStream)
      .filter(_.validate)
      .map(_.result)
      .fold(0L)(_ + _)

  override def part2(input: Input): Output =
    input
      .flatMap(parser.parseAllStream)
      .filter(_.validatePart2)
      .map(_.result)
      .fold(0L)(_ + _)

}
