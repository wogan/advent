package dev.wogan.advent.scala

import cats.effect.IO
import cats.{Alternative, Applicative, MonoidK}
import fs2.Stream
import cats.syntax.all.*

extension [A] (stream: Stream[IO, A])

  /** Will consume the underlying stream. */
  def sorted(using Ordering[A]): Stream[IO, A] =
    Stream.evalSeq(stream.compile.toList.map(_.sorted))

extension [A, B] (stream: Stream[IO, (A, B)])

  /** Will process the underlying stream twice. */
  def unzip: (Stream[IO, A], Stream[IO, B]) =
    stream.separate

// Not a lawful instance of Alternative, but it works for our purposes (separate)
private given (using ap: Applicative[Stream[IO, *]], mk: MonoidK[Stream[IO, *]]): Alternative[Stream[IO, *]] with
  def pure[A](x: A): Stream[IO, A] = ap.pure(x)
  def ap[A, B](ff: Stream[IO, A => B])(fa: Stream[IO, A]): Stream[IO, B] = ap.ap(ff)(fa)
  def empty[A]: Stream[IO, A] = mk.empty
  def combineK[A](x: Stream[IO, A], y: Stream[IO, A]): Stream[IO, A] = mk.combineK(x, y)
