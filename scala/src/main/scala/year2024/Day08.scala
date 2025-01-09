package dev.wogan.advent.scala
package year2024

object Day08 extends Day(8) {

  case class InputMap(size: Point, antennas: Map[Char, Set[Point]]):
    def isValid(p: Point): Boolean = p.x >= 0 && p.y >= 0 && p.x < size.x && p.y < size.y

  extension (p: Point)
    def antinodes(o: Point): Seq[Point] =
      val (dx, dy) = p - o
      Seq(
        Point(p.x + dx, p.y + dy),
        Point(o.x - dx, o.y - dy),
      )

  extension (s: Set[Point])
    def antinodes(inputMap: InputMap): Set[Point] =
      val x1 = for
        (a, b) <- s.toSeq.pairCombinations
        if a != b
        x <- a.antinodes(b)
        if inputMap.isValid(x)
      yield x
      x1.toSet

  def buildMap(lines: List[String]): InputMap =
    val size = lines.head.length -> lines.length
    val antenna = for
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
      if c != '.'
    yield c -> Point(x, y)
    InputMap(size, antenna.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap)

  override def part1(input: Input): Output =
    input.compile.toList.map { lines =>
      val map = buildMap(lines)
      val antinodes = for
        (c, antennas) <- map.antennas.toSeq
        as <- antennas.antinodes(map)
      yield as
      antinodes.toSet.size.toString
    }

}

