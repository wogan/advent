package dev.wogan.advent.scala

import scala.annotation.targetName


type Point = (Int, Int)

extension (a: Point)
  @targetName("addPoints")
  infix def +(b: Point): Point = (a._1 + b._1, a._2 + b._2)

type Grid[A] = List[List[A]]

extension [A](grid: Grid[A])
  def points: List[Point] =
    grid.head.indices.flatMap(x => grid.indices.map(y => (x, y))).toList

  def get(point: Point): Option[A] =
    grid.lift(point._2).flatMap(_.lift(point._1))

  def getAll(points: List[Point]): List[A] =
    points.flatMap(get)