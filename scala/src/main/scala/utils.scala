package dev.wogan.advent.scala

import cats.Order
import cats.collections.Heap
import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

extension [A](a: A)
  def repeatWhile(f: A => A, p: A => Boolean): List[A] =
    Stream.iterate(a)(f).takeWhile(p).compile.toList

extension (s: String)
  def cleave(): (String, String) =
    s.splitAt(s.length / 2)

extension (r: Range)
  def contains(other: Range): Boolean =
    r.start <= other.start && r.end >= other.end

  def overlaps(other: Range): Boolean =
    (r.start >= other.start && r.start <= other.end)
      || (r.end >= other.start && r.end <= other.end)

extension [A](s: Seq[A])
  def pairCombinations: Iterator[(A, A)] =
    s.combinations(2).map(s => (s.head, s.last))

extension (stream: Stream[IO, Int])
  def max: Stream[IO, Int] =
    stream.reduce(Math.max)

  def sum: Stream[IO, Int] =
    stream.reduce(_ + _)

given streamIntToString: Conversion[Stream[IO, Int], Output] =
  _.map(_.toString)

given streamLongToString: Conversion[Stream[IO, Long], Output] =
  _.map(_.toString)

given ioStringToStream: Conversion[IO[String], Output] =
  Stream.eval(_)

given ioLongToStream: Conversion[IO[Long], Output] =
  Stream.eval(_).map(_.toString)

given ioIntToStream: Conversion[IO[Int], Output] =
  Stream.eval(_).map(_.toString)

extension [A: Order](heap: Heap[A])
  def offer(a: A, limit: Int): Heap[A] =
    if heap.size < limit then
      heap.add(a)
    else if heap.getMin.exists(_ > a) then
      heap
    else
      heap.add(a).remove
