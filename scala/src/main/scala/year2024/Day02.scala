package dev.wogan.advent.scala
package year2024

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

object Day02 extends Day(2) {

  private type Level = Int
  private type Report = List[Level]

  enum Direction:
    case Asc, Desc

  case class Delta(direction: Direction, delta: Int):
    def isSafe: Boolean =
      delta > 0 && delta <= 3

    def safeWith(other: Delta): Boolean =
      direction == other.direction && isSafe && other.isSafe

  def calculateDelta(a: Level, b: Level): Delta =
    val delta = Math.abs(a - b)
    val direction = if a < b then Direction.Asc else Direction.Desc
    Delta(direction, delta)

  def toReport(s: String): Report =
    s.split(" ").map(_.toInt).toList

  def isSafe(report: Report): Boolean =
    val deltas = report.zip(report.tail).map(calculateDelta)
    deltas.zip(deltas.tail).forall {
      case (a, b) => a.safeWith(b)
    }

  def isSafeWithDampener(report: Report): Boolean =
    if (isSafe(report)) return true
    Zipper.fromListUnsafe(report).coflatMap(z => {
      val omitted = z.delete.toList.flatMap(_.toList)
      isSafe(omitted)
    }).toList.exists(identity)

  override def part1(input: Input): Output =
    input
      .map(toReport)
      .filter(isSafe)
      .compile
      .count

  override def part2(input: Input): Output =
    input
      .map(toReport)
      .filter(isSafeWithDampener)
      .compile
      .count
}
