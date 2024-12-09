package dev.wogan.advent.scala

import cats.effect.IO
import cats.{Alternative, Applicative, MonoidK}
import fs2.{Pull, RaiseThrowable, Stream}
import cats.syntax.all.*

import scala.annotation.tailrec

extension [A] (stream: Stream[IO, A])

  /** Will consume the underlying stream. */
  def sorted(using Ordering[A]): Stream[IO, A] =
    Stream.evalSeq(stream.compile.toList.map(_.sorted))

extension [A, B] (stream: Stream[IO, (A, B)])

  /** Will process the underlying stream twice. */
  def unzip: (Stream[IO, A], Stream[IO, B]) =
    stream.separate

extension[F[_]: RaiseThrowable, A] (stream: Stream[F, A])
  def foldWhile[S, R](s0: S)(f: (S, A) => F[Either[S, R]]): Stream[F, (R, Stream[F, A])] =
    def loop(s: S, p: Pull[F, Nothing, Option[(A, Stream[F, A])]]): Pull[F, Nothing, (R, Stream[F, A])] =
      p.flatMap {
        case None => Pull.raiseError(new IllegalStateException)
        case Some((hd, tl)) =>
          Pull.eval(f(s, hd)).flatMap {
            case Left(s1) => loop(s1, tl.pull.uncons1)
            case Right(r) => Pull.pure(r -> tl)
          }
      }
    loop(s0, stream.pull.uncons1).flatMap(Pull.output1).stream

// Not a lawful instance of Alternative, but it works for our purposes (separate)
private given (using ap: Applicative[Stream[IO, *]], mk: MonoidK[Stream[IO, *]]): Alternative[Stream[IO, *]] with
  def pure[A](x: A): Stream[IO, A] = ap.pure(x)
  def ap[A, B](ff: Stream[IO, A => B])(fa: Stream[IO, A]): Stream[IO, B] = ap.ap(ff)(fa)
  def empty[A]: Stream[IO, A] = mk.empty
  def combineK[A](x: Stream[IO, A], y: Stream[IO, A]): Stream[IO, A] = mk.combineK(x, y)
