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
  val long: Parser[Long] = Numbers.digits.map(_.toLong)
  val whitespace: Parser[Unit] = Parser.char(' ').rep.void

  def find[A](parser: Parser[A]): Parser[A] =
    Parser.anyChar.repUntil0(Parser.peek(parser)).with1 *> parser

  def findAll[A](parser: Parser[A]): Parser[List[A]] =
    Parser.repUntil(find(parser), (Parser.not(parser).with1 *> Parser.anyChar).rep0 *> Parser.end).map(_.toList) <* Parser.anyChar.rep0 <* Parser.end
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

extension (stream: Stream[IO, String])
  def parseUntil[P](parser: Parser[P], end: Parser0[Any] = Parser.end): Stream[IO, (List[P], Stream[IO, String])] =
    stream.foldWhile(List.empty[P]) { (s, a) =>
      IO.pure(end.parseAll(a).as(s)).flatMap {
        case Left(_) =>
          parser.parseAllIO(a).map(s.appended).map(Left(_))
        case Right(_) =>
          IO.pure(Right(s))
      }
    }
