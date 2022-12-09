package dev.wogan.advent
package day01

import cats.{Order, Semigroup}
import cats.collections.Heap
import cats.effect.{ExitCode, IO, IOApp}
import cats.kernel.Monoid
import cats.syntax.all.*
import fs2.{Chunk, Pull, Stream}

object Day01 extends Day(1) {

  private def totals(input: Input): Output =
    input
      .map(_.toIntOption)
      .split(_.isEmpty)
      .map(_.toVector.combineAll)
      .unNone

  override def part1(input: Input): Output =
    totals(input).reduce(Math.max)

  override def part2(input: Input): Output =
    totals(input)
      .fold(Heap.empty[Int]) { (heap, i) =>
        heap.offer(i, 3)
      }
      .map(_.toList.combineAll)
}
