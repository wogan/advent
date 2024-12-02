package dev.wogan.advent.scala
package year2022

import cats.data.NonEmptyList
import cats.effect.IO
import cats.parse.Parser.{anyChar, char}
import cats.parse.{Numbers, Parser, Parser0}
import cats.syntax.all.*
import fs2.{Pull, RaiseThrowable, Stream}

private type StackId = Int
private type StackLine = List[(Char, Int)]
private type IdLine = NonEmptyList[(StackId, Int)]

object Day05 extends Day(5) {
  val int = Numbers.digits.map(_.toInt)
  val ` ` = char(' ')

  val stackItem: Parser[Option[Char]] = anyChar.between(char('['), char(']'))
    .map(Option.apply).backtrack
    .orElse(Parser.string("   ").map(_ => Option.empty[Char]))
    .withContext("stackItem")

  val parseLine: Parser[StackLine] = stackItem.repSep(` `).map(_.zipWithIndex).map(_.collect {
    case (Some(c), i) => c -> i
  }).withContext("parseLine")

  val parseIdsLine: Parser[IdLine] = Numbers.digit.between(` `, ` `).map(_.toInt).repSep(` `).map(_.zipWithIndex)

  val parserEither: Parser0[Either[StackLine, IdLine]] = parseIdsLine.backtrack.eitherOr(parseLine)

  val parseCommand: Parser[Move] =
    (Parser.string("move ") *> int ~ (Parser.string(" from ") *> int) ~ (Parser.string(" to ") *> int)).map {
      case ((a, b), c) => Move(a, b, c)
    }

  override def part1(input: Input): Output =
    import cats.effect.unsafe.implicits.global
    // Print the input?
    input.debug().evalMap(s => parserEither.parseAllIO(s)).debug().compile.drain.unsafeRunSync()

    // Do the actual work
    val pull = input.foldWhile[List[StackLine], Stacks](List()) { (s, x) =>
      println(show"Parsing line \"$x\", current state: $s")
      parserEither.parseAllIO(x).map {
        case Left(stackLine) => (s :+ stackLine).asLeft
        case Right(ids) => mkStacks(ids, s).asRight
      }
    }
    pull.flatMap(r => Pull.output1(r)).stream.flatMap {
      case (state, stream) =>
        stream
          .drop(1)
          .evalMap(parseCommand.parseAllIO)
          .fold(state)(_.execute(_))
    }.map(_.bottom)

  def mkStacks(lines: IdLine, stacks: List[StackLine]): Stacks = ???
}

private case class Stacks(map: Map[StackId, Stack], order: List[Int]):
  def bottom: String = order.map(map).map(_.bottom).mkString

  def execute(move: Move): Stacks =
    val (items, removed) = map(move.from).pop(move.qty)
    val added = map(move.to).addAllToTop(items.reverse)
    Stacks(map.updated(move.from, removed).updated(move.to, added), order)

private case class Stack(items: List[Char]): // start of list = top of stack
  def addAllToTop(list: List[Char]): Stack =
    Stack(list ::: items)

  def addToTop(item: Char): Stack =
    Stack(item :: items)

  def addToBottom(item: Char): Stack =
    Stack(items :+ item)

  def bottom: Char =
    items.last

  def top: Char =
    items.head

  def pop(n: Int): (List[Char], Stack) =
    val (a, b) = items.splitAt(n)
    (a, Stack(b))

private case class Move(qty: Int, from: StackId, to: StackId)


extension[F[_] : RaiseThrowable, A] (stream: Stream[F, A])
  def foldWhile[S, R](s: S)(f: (S, A) => F[Either[S, R]]): Pull[F, Nothing, (R, Stream[F, A])] =
    stream.pull.uncons1.flatMap {
      case None => Pull.raiseError(new IllegalStateException)
      case Some((hd, tl)) =>
        Pull.eval(f(s, hd)).flatMap {
          case Left(s1) => tl.foldWhile(s1)(f)
          case Right(r) => Pull.pure(r -> tl)
        }
    }
