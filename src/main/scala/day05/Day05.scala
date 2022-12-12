package dev.wogan.advent
package day05

import cats.data.NonEmptyList
import cats.effect.IO
import cats.parse.{Numbers, Parser}
import fs2.Pull

import scala.collection.immutable.SortedMap

type StackLine = NonEmptyList[(Option[Char], Int)]
type IdLine = NonEmptyList[(Int, Int)]

object Day05 extends Day(5) {
  val `[` = Parser.char('[')
  val `]` = Parser.char(']')
  val int = Numbers.digits.map(_.toInt)
  val ` ` = Parser.char(' ')

  val stackItem: Parser[Option[Char]] = Parser.anyChar.between(`[`, `]`).map(Option.apply)
    .orElse(Parser.string("   ").map(_ => Option.empty[Char]))

  val parseLine: Parser[StackLine] = stackItem.repSep(` `).map(_.zipWithIndex)

  val parseIdsLine: Parser[IdLine] = Numbers.digit.between(` `, ` `).map(_.toInt).repSep(` `).map(_.zipWithIndex)

  val parserEither: Parser[Either[StackLine, IdLine]] = parseIdsLine.eitherOr(parseLine)

  val parseCommand: Parser[Move] =
    (Parser.string("move ") *> int ~ (Parser.string(" from ") *> int) ~ (Parser.string(" to ") *> int)).map {
      case ((a, b), c) => Move(a, b, c)
    }

  override def part1(input: Input): Output =
    super.part1(input)

}

case class Stacks(map: Map[Int, Stack], order: List[Int]):
  def bottom: String = order.map(map).map(_.bottom).mkString

  def execute(move: Move): Stacks =
    val (items, removed) = map(move.from).pop(move.qty)
    val added = map(move.to).addAllToTop(items.reverse)
    Stacks(map.updated(move.from, removed).updated(move.to, added), order)

case class Stack(items: List[Char]):
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

case class Move(qty: Int, from: Int, to: Int)


extension [A](p: Parser[A])
  def parseAllIO(string: String): IO[A] =
    IO.fromEither(p.parseAll(string).left.map(e => IllegalArgumentException(e.toString())))

/*

*/
