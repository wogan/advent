package dev.wogan.advent.scala

import cats.Order
import cats.collections.Heap
import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

import scala.annotation.targetName

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
    
  def pairPermutations: Seq[(A, A)] =
    s.flatMap(a => s.map(a -> _))

extension (stream: Stream[IO, Int])
  def max: Stream[IO, Int] =
    stream.reduce(Math.max)

  def sum: Stream[IO, Int] =
    stream.reduce(_ + _)

extension (stream: Stream[IO, Long])
  @targetName("maxLong")
  def max: Stream[IO, Long] =
    stream.reduce(Math.max)

  @targetName("sumLong")
  def sum: Stream[IO, Long] =
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

extension (i: Long)
  def divisors: Seq[Long] =
    val x = math.abs(i)
    val limit = math.sqrt(x.toDouble).toLong

    val small = for {
      i <- 1L to limit
      if x % i == 0L
    } yield i

    val large = small.flatMap { i =>
      val j = x / i
      if (j != i) Some(j) else None
    }

    (small ++ large).sorted
