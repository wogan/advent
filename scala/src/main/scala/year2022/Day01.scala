package dev.wogan.advent.scala
package year2022

import cats.collections.Heap
import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

object Day01 extends Day(1) {

  private def totals(input: Input): Stream[IO, Int] =
    input
      .map(_.toIntOption)
      .split(_.isEmpty)
      .map(_.toVector.combineAll)
      .unNone

  override def part1(input: Input): Output =
    totals(input).reduce(Math.max).sum

  override def part2(input: Input): Output =
    totals(input)
      .fold(Heap.empty[Int]) { (heap, i) =>
        heap.offer(i, 3)
      }
      .map(_.toList.combineAll)
      .sum
}
