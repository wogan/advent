package dev.wogan.advent

import cats.{Order, Show}
import cats.collections.Heap
import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}

type Input = Stream[IO, String]
type Output = Stream[IO, Int]

def loadFile(filename: String): Input =
  Files[IO].readUtf8Lines(Path("src/main/resources").absolute / Path(filename))

extension (stream: Stream[IO, Int])
  def max: Stream[IO, Int] =
    stream.reduce(Math.max)

  def sum: Stream[IO, Int] =
    stream.reduce(_ + _)

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
