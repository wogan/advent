package dev.wogan.advent.scala
package year2024

import cats.syntax.all.*
import fs2.Stream

object Day04 extends Day(4) {
  enum Direction(val x: Int, val y: Int) {
    case Across extends Direction(1, 0)
    case Down extends Direction(0, 1)
    case DiagonalDown extends Direction(1, 1)
    case DiagonalUp extends Direction(1, -1)
    case BackwardsAcross extends Direction(-1, 0)
    case Up extends Direction(0, -1)
    case DiagonalDownBackwards extends Direction(-1, 1)
    case DiagonalUpBackwards extends Direction(-1, -1)

    def asPoint: Point = (x, y)
  }
  enum XShape(val m: Point, val s: Point) {
    case LeftDown extends XShape((-1, -1), (1, 1))
    case RightDown extends XShape((1, -1), (-1, 1))
    case LeftUp extends XShape((-1, 1), (1, -1))
    case RightUp extends XShape((1, 1), (-1, -1))
  }

  def indices(start: Point, direction: Direction): List[Point] =
    Stream.iterate(start)(_ + direction.asPoint).take(4).toList

  def indices(start: Point, direction: XShape): List[Point] =
    List(start + direction.m, start, start + direction.s)

  override def part1(input: Input): Output =
    input
      .map(_.toList)
      .compile.toList
      .map(grid =>
        for {
          point <- grid.points
          if grid.get(point).contains('X')
          direction <- Direction.values
          if grid.getAll(indices(point, direction)).mkString == "XMAS"
        } yield 1
      )
      .map(_.sum)

  override def part2(input: Input): Output =
    input
      .map(_.toList)
      .compile.toList
      .map(grid => {
        grid.points.filter(grid.get(_).contains('A')).count { point =>
          val s = XShape.values.map { shape =>
            grid.getAll(indices(point, shape)).mkString
          }.count(_ == "MAS")
          s == 2
        }
      })
}
