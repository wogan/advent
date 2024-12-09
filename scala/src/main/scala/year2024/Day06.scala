package dev.wogan.advent.scala
package year2024

import cats.syntax.all.*
import scala.annotation.tailrec

object Day06 extends Day(6) {

  enum Direction {
    case North, East, South, West

    def turnRight: Direction =
      this match
        case North => East
        case East => South
        case South => West
        case West => North

    def movement: Point =
      this match
        case North => (0, -1)
        case South => (0, 1)
        case West => (-1, 0)
        case East => (1, 0)
  }

  case class PuzzleMap(
    size: Point,
    obstacles: Set[Point]
  ):
    def outside(point: Point): Boolean =
      point._1 < 0 || point._1 >= size._1 || point._2 < 0 || point._2 >= size._2

    def withObstacle(point: Point): PuzzleMap =
      copy(obstacles = obstacles + point)

  type History = Map[Point, Set[Direction]]
  extension (history: History)
    def add(current: Guard): History =
      history.updatedWith(current.location) {
        case None => Some(Set(current.facing))
        case Some(set) => Some(set + current.facing)
      }
    def totalVisited: Int =
      history.size

  case class Guard(
    location: Point,
    facing: Direction = Direction.North,
    visited: Map[Point, Set[Direction]] = Map.empty,
    done: Boolean = false
  ):
    def turnRight: Guard =
      copy(facing = facing.turnRight)
    def leave: Guard =
      copy(visited = visited.add(this), done = true)
    def move: Guard =
      copy(location = location + facing.movement, visited = visited.add(this))
    def inCycle: Boolean =
      !done && visited.get(location).exists(_.contains(facing))

  def findGuard(lines: List[String]): Guard =
    val y = lines.indexWhere(_.contains('^'))
    val x = lines(y).indexOf('^')
    Guard(x -> y)

  def buildMap(lines: List[String]): PuzzleMap =
    val size = lines.head.length -> lines.length
    val obstacles = for
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
      if c == '#'
    yield x -> y
    PuzzleMap(size, obstacles.toSet)

  def walkGuard(map: PuzzleMap, guard: Guard): Guard =
    @tailrec
    def loop(guard: Guard): Guard =
      if guard.inCycle then return guard
      val next = guard.location + guard.facing.movement
      if map.obstacles.contains(next) then loop(guard.turnRight)
      else if map.outside(next) then guard.leave
      else loop(guard.move)
    loop(guard)

  def findCycles(map: PuzzleMap, guard: Guard): List[Point] =
    val points = for
      x <- 0 until map.size._1
      y <- 0 until map.size._2
      if !map.obstacles.contains(x -> y) && (x, y) != guard.location
      finalGuard = walkGuard(map.withObstacle(x -> y), guard)
      if finalGuard.inCycle
    yield x -> y
    points.toList

  override def part1(input: Input): Output =
    input.compile.toList.map { lines =>
      val map = buildMap(lines)
      val guard = findGuard(lines)
      val finalGuard = walkGuard(map, guard)
      finalGuard.visited.totalVisited
    }

  override def part2(input: Input): Output =
    input.compile.toList.map { lines =>
      val map = buildMap(lines)
      val guard = findGuard(lines)
      val cycles = findCycles(map, guard)
      cycles.size
    }

}
