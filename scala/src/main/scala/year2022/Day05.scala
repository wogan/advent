package dev.wogan.advent.scala
package year2022

import cats.effect.IO
import cats.parse.{Numbers, Parser, Parser0}
import cats.syntax.all.*
import fs2.{Pull, RaiseThrowable, Stream}

type StackLine = List[(Char, Int)]
type IdLine = List[(Int, Int)]

object Day05 extends Day(5) {
  val `[` = Parser.char('[')
  val `]` = Parser.char(']')
  val int = Numbers.digits.map(_.toInt)
  val ` ` = Parser.char(' ')

  val stackItem: Parser[Option[Char]] = Parser.anyChar.between(`[`, `]`).map(Option.apply).backtrack
    .orElse(Parser.string("   ").map(_ => Option.empty[Char]))

  val parseLine: Parser[StackLine] = stackItem.repSep(` `).map(_.zipWithIndex).map(_.collect {
    case (Some(c), i) => c -> i
  })

  val parseIdsLine: Parser0[IdLine] = Numbers.digit.between(` `, ` `).map(_.toInt).repSep0(` `).map(_.zipWithIndex)

  val parserEither: Parser0[Either[StackLine, IdLine]] = parseIdsLine.backtrack.eitherOr(parseLine)

  val parseCommand: Parser[Move] =
    (Parser.string("move ") *> int ~ (Parser.string(" from ") *> int) ~ (Parser.string(" to ") *> int)).map {
      case ((a, b), c) => Move(a, b, c)
    }


  override def part1(input: Input): Output =
    import cats.effect.unsafe.implicits.global
    input.debug().evalMap(s => parserEither.parseAllIO(s)).debug().compile.drain.unsafeRunSync()
    val pull = input.foldWhile[StackLine, Stacks](List()) { (s, x) =>
      println(show"Parsing line \"$x\", current state: $s")
      parserEither.parseAllIO(x).map {
        case Left(lines) => (s ++ lines).asLeft
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

  def mkStacks(lines: IdLine, stacks: StackLine): Stacks = ???
}

private case class Stacks(map: Map[Int, Stack], order: List[Int]):
  def bottom: String = order.map(map).map(_.bottom).mkString

  def execute(move: Move): Stacks =
    val (items, removed) = map(move.from).pop(move.qty)
    val added = map(move.to).addAllToTop(items.reverse)
    Stacks(map.updated(move.from, removed).updated(move.to, added), order)

private case class Stack(items: List[Char]):
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

private case class Move(qty: Int, from: Int, to: Int)

extension[A] (p: Parser0[A])
  def parseAllIO(string: String): IO[A] =
    IO.fromEither(p.parseAll(string).left.map(e => IllegalArgumentException(show"Parser ${p.toString} error: \n$e")))

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
