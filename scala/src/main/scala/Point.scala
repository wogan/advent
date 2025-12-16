package dev.wogan.advent.scala

import scala.annotation.targetName

type Point = (Int, Int)

object Point:
  def apply(x: Int, y: Int): Point = (x, y)

extension (a: Point)
  def x: Int = a._1
  def y: Int = a._2

  @targetName("addPoints")
  infix def +(b: Point): Point = (a.x + b.x, a.y + b.y)

  @targetName("subtractPoints")
  infix def -(b: Point): Point = (a.x - b.x, a.y - b.y)

type Grid[A] = List[List[A]]

extension [A](grid: Grid[A])
  def points: List[Point] =
    grid.head.indices.flatMap(x => grid.indices.map(y => (x, y))).toList

  def get(point: Point): Option[A] =
    grid.lift(point.y).flatMap(_.lift(point.x))

  def getAll(points: List[Point]): List[A] =
    points.flatMap(get)

  def neighbours(point: Point): List[Point] =
    (-1 to 1).pairPermutations
      .filterNot((_, _) == (0, 0))
      .map(a => point + a)
      .filter((a, b) => a >= 0 && b >= 0 && a < grid.size && b < grid.head.size)
      .toList
