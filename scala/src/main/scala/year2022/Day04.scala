package dev.wogan.advent.scala
package year2022

import cats.effect.IO
import cats.parse.{Numbers, Parser}

object Day04 extends Day(4):

  val parser: Parser[(Assignment, Assignment)] = Parser.char(',').pair(Assignment.parser, Assignment.parser)

  def parse(string: String): IO[(Assignment, Assignment)] =
    IO.fromEither(parser.parseAll(string).left.map(e => IllegalArgumentException(e.toString())))

  override def part1(input: Input): Output =
    input.evalMap(parse).map {
      case (a, b) if a.contains(b) || b.contains(a) => 1
      case _ => 0
    }.sum.asString

  override def part2(input: Input): Output =
    input.evalMap(parse).map {
      case (a, b) if a.overlaps(b) => 1
      case _ => 0
    }.sum.asString

private case class AssignmentTwo(range: Range):
  def contains(other: AssignmentTwo): Boolean =
    false


private case class Assignment(start: Int, end: Int):
  def contains(other: Assignment): Boolean =
    this.start <= other.start && this.end >= other.end

  def overlaps(other: Assignment): Boolean =
    this.end >= other.start && other.end >= this.start

private object Assignment:
  val parser: Parser[Assignment] = Parser.char('-').pair(Numbers.digits, Numbers.digits)
    .map(a => Assignment(a._1.toInt, a._2.toInt))

extension (p: Parser[Any])

  /**
   * Returns tuple of the provided parsers, with the original parser acting as a separator.
   */
  def pair[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] =
    (a <* p) ~ b
