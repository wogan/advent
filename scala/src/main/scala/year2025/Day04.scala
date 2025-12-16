package dev.wogan.advent.scala
package year2025

import cats.parse.Parser

import scala.language.implicitConversions

object Day04 extends Day(4) {

  val parseLine = (Parser.char('@').as(true) | Parser.char('.').as(false)).rep0


  extension (grid: Grid[Boolean])
    def remove(points: List[Point]): Grid[Boolean] =
      grid.map { line =>
        line.map { item =>
          ???
        }
      }


  override def part1(input: Input): Output = {
    input.flatMap(parseLine.parseAllStream).compile.toList.map { grid =>
      grid.points.map { p =>
        grid.get(p).get && grid.neighbours(p).map { n =>
          grid.get(n).get
        }.count(identity) < 4
      }.count(identity)
    }
  }
}
