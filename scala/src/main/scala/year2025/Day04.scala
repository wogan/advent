package dev.wogan.advent.scala
package year2025

import cats.parse.Parser
import fs2.Stream

import scala.language.implicitConversions

object Day04 extends Day(4) {

  val parseLine = (Parser.char('@').as(true) | Parser.char('.').as(false)).rep0

  extension (grid: Grid[Boolean])

    def canRemove(point: Point): Boolean =
      grid.get(point).get && grid.neighbours(point).map { n =>
        grid.get(n).get
      }.count(identity) < 4

    def remove(points: List[Point]): Grid[Boolean] =
      grid.mapWithPoints { (value, point) =>
          !points.contains(point) && value
      }

  override def part1(input: Input): Output =
    input.flatMap(parseLine.parseAllStream).compile.toList.map { grid =>
      grid.points.map(grid.canRemove).count(identity)
    }

  override def part2(input: Input): Output =
    input.flatMap(parseLine.parseAllStream).compile.toList.flatMap { grid =>
      Stream.iterate(0 -> grid) {
          case (_, current) =>
            val pointsToRemove = current.points.filter(current.canRemove)
            pointsToRemove.size -> current.remove(pointsToRemove)
        }
        .drop(1).map(_._1)
        .takeWhile(_ > 0)
        .sum.compile.lastOrError
    }
}
