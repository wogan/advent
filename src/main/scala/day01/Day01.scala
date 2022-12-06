package dev.wogan.advent
package day01

import cats.{Order, Semigroup}
import cats.collections.Heap
import cats.effect.{ExitCode, IO, IOApp}
import cats.kernel.Monoid
import cats.syntax.all.*
import fs2.{Chunk, Pull, Stream}

object Day01 extends Day(1) {

  val exampleInput: String =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin

  def totals: Stream[IO, Int] =
    input
      .map(_.toIntOption)
      .split(_.isEmpty)
      .map(_.toVector.combineAll)
      .unNone

  def part1: Stream[IO, Int] =
    totals.reduce(Math.max)

  def part2: Stream[IO, Int] =
    totals
      .fold(Heap.empty[Int]) { (heap, i) =>
        heap.offer(i, 3)
      }
      .map(_.toList.combineAll)

  override def run: IO[Unit] =
    part2.printLast
}

implicit class LimitHeap[A: Order](heap: Heap[A]) {
  def offer(a: A, limit: Int): Heap[A] = {
    if heap.size < limit then
      heap.add(a)
    else if heap.getMin.exists(_ > a) then
      heap
    else
      heap.add(a).remove
  }
}
