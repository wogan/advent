package dev.wogan.advent
package day03

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

object Day03 extends Day(3) {

  val example: String = """vJrwpWtwJgWrhcsFMMfFFhFp
                  |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                  |PmmdzqPrVvPwwTWBwg
                  |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                  |ttgJtRGJQctTZtZT
                  |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  extension (s: String)
    def cleave(): (String, String) =
      s.splitAt(s.length / 2)

  def part1: Stream[IO, Int] =
    input.map { s =>
      val (a, b) = s.cleave()
      val intersection = a.toSet.intersect(b.toSet)
      priority(intersection.head)
    }.reduce(_ + _)

  def priority(char: Char): Int =
    if (char.isUpper)
      char.toInt - 'A' + 27
      else char.toInt - 'a' + 1

  def part2: Stream[IO, Int] =
    input.sliding(3, 3)
      .map(_.toVector.map(_.toSet).reduce(_.intersect(_)).head)
      .map(priority)
      .reduce(_ + _)

  override def run: IO[Unit] =
    part2.printLast
}
