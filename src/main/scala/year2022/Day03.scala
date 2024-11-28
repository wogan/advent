package dev.wogan.advent
package year2022

object Day03 extends Day(3) {

  override def part1(input: Input): Output =
    input.map { s =>
      val (a, b) = s.cleave()
      val intersection = a.toSet.intersect(b.toSet)
      priority(intersection.head)
    }.sum.asString

  def priority(char: Char): Int =
    if (char.isUpper)
      char.toInt - 'A' + 27
    else char.toInt - 'a' + 1

  override def part2(input: Input): Output =
    input.sliding(3, 3)
      .map(_.toVector.map(_.toSet).reduce(_.intersect(_)).head)
      .map(priority)
      .sum.asString

}
