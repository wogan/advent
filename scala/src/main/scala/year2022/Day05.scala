package dev.wogan.advent.scala
package year2022

import Parsers.whitespace

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

  val parseLine: Parser[StackLine] = stackItem.repSep(` `).map(_.zipWithIndex).map(_.collect {
    case (Some(c), i) => c -> i
  })

  val parseIdsLine: Parser[IdLine] = whitespace *> Parsers.int.repSep(whitespace).map(_.zipWithIndex) <* ` `.rep0

  val parserEither: Parser0[Either[StackLine, IdLine]] = parseIdsLine.backtrack.eitherOr(parseLine)

  val parseCommand: Parser[Move] =
    (Parser.string("move ") *> int ~ (Parser.string(" from ") *> int) ~ (Parser.string(" to ") *> int)).map {
      case ((a, b), c) => Move(a, b, c)
    }

  override def part1(input: Input): Output =
    input.foldWhile[List[StackLine], Stacks](List()) { (s, x) =>
      parserEither.parseAllIO(x).map {
        case Left(stackLine) => (s :+ stackLine).asLeft
        case Right(ids) => mkStacks(ids, s).asRight
      }
    }.flatMap {
      case (state, stream) =>
        stream
          .dropWhile(_.isEmpty) // blank line
          .evalMap(parseCommand.parseAllIO)
          .fold(state)(_.execute(_))
    }.map(_.bottom)

  def mkStacks(lines: IdLine, stacks: List[StackLine]): Stacks =
    val idMap = lines.groupMap(_._2)(_._1).view.mapValues(_.head)
    val order = idMap.values.toList
    val stackMap = stacks.flatten.groupMap(_._2)(_._1)
      .map { case (k, v) => idMap(k) -> Stack(v) }
    Stacks(stackMap, order)
}

private case class Stacks(map: Map[StackId, Stack], order: List[StackId]):
  def bottom: String = order.map(map).map(_.top).mkString

  def execute(move: Move): Stacks =
    val (items, remainder) = map(move.from).pop(move.qty)
    val added = map(move.to).addAllToTop(items.reverse)
    Stacks(map.updated(move.from, remainder).updated(move.to, added), order)

private case class Stack(items: List[Char]): // start of list = top of stack
  def addAllToTop(list: List[Char]): Stack =
    Stack(list ::: items)

  def top: Char =
    items.head

  def pop(n: Int): (List[Char], Stack) =
    val (a, b) = items.splitAt(n)
    (a, Stack(b))

private case class Move(qty: Int, from: StackId, to: StackId)
