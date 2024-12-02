package dev.wogan.advent.scala

import cats.effect.IO
import fs2.{Pull, Stream}

extension [A] (stream: Stream[IO, A])

  def sorted(using Ordering[A]): Stream[IO, A] =
    Stream.evalSeq(stream.compile.toList.map(_.sorted))

  def unzip[B, C](using f: A => (B, C)): (Stream[IO, B], Stream[IO, C]) =
    val x = Stream.eval(stream.map(f).compile.toList.map(_.unzip))
    (x.map(_._1).flatMap(Stream.emits), x.map(_._2).flatMap(Stream.emits))


val x = Stream(1,2,3,4,5).zip(Stream(5,4,3,2,1)).toList.unzip

val test = List(1 -> 2, 3 -> 4).unzip

