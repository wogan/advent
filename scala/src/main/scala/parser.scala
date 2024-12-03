package dev.wogan.advent.scala

import cats.Show
import cats.effect.IO
import cats.parse.{Numbers, Parser, Parser0}
import cats.syntax.all.*
import fs2.Stream

/* Parsers may be combined through operators:
   ~            - product. Allows continuing parsing if the left side was successful;
   <*, *>       - productL and productR. Works just like product but drop part of result;
   surroundedBy - identical to border *> parsingResult <* border;
   between      - identical to border1 *> parsingResult <* border2;
   |, orElse    - Parser will be successful if any of sides is successful.
*/

object Parsers {
  val int: Parser[Int] = Numbers.digits.map(_.toInt)
  val whitespace: Parser[Unit] = Parser.char(' ').rep.void
}

extension (p: Parser[Any])

  /**
   * Returns tuple of the provided parsers, with the original parser acting as a separator.
   */
  def pair[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] =
    (a <* p) ~ b

  /**
    * Returns tuple of the provided parsers, with the original parser acting as a separator.
    */
  def pairOf[A : Parser, B : Parser]: Parser[(A, B)] =
    pair(summon[Parser[A]], summon[Parser[B]])

extension[A] (p: Parser0[A])

  def parseAllRaise(string: String): Either[IllegalArgumentException, A] =
    p.parseAll(string).left.map((e: Parser.Error) => IllegalArgumentException(show"Parsing error:\n$e"))

  def parseAllIO(string: String): IO[A] =
    IO.fromEither(p.parseAllRaise(string))

  def parseAllStream(string: String): Stream[IO, A] =
    Stream.fromEither(parseAllRaise(string))
