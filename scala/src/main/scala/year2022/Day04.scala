package dev.wogan.advent.scala
package year2022

import cats.effect.IO
import cats.parse.Numbers.digits
import cats.parse.Parser.char
import cats.parse.{Numbers, Parser}

object Day04 extends Day(4):

  val parser: Parser[(Assignment, Assignment)] = char(',').pairOf[Assignment, Assignment]

  def parse(string: String): IO[(Assignment, Assignment)] =
    IO.fromEither(parser.parseAll(string).left.map(e => IllegalArgumentException(e.toString())))

  override def part1(input: Input): Output =
    input.evalMap(parse).map {
      case (a, b) if a.contains(b) || b.contains(a) => 1
      case _ => 0
    }.sum

  override def part2(input: Input): Output =
    input.evalMap(parse).map {
      case (a, b) if a.overlaps(b) => 1
      case _ => 0
    }.sum

private case class AssignmentTwo(range: Range):
  def contains(other: AssignmentTwo): Boolean =
    false

private case class Assignment(start: Int, end: Int):
  def contains(other: Assignment): Boolean =
    this.start <= other.start && this.end >= other.end

  def overlaps(other: Assignment): Boolean =
    this.end >= other.start && other.end >= this.start

private given Parser[Assignment] =
  char('-').pair(digits, digits)
    .map(a => Assignment(a._1.toInt, a._2.toInt))
