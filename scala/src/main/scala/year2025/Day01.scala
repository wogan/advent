package dev.wogan.advent.scala
package year2025

import cats.effect.IO
import cats.parse.Parser
import fs2.Stream
import scala.language.implicitConversions

object Day01 extends Day(1) {
  final val SIZE = 100

  enum Instruction(val sign: Int, val amount: Int):
    case Left(override val amount: Int) extends Instruction(-1, amount)
    case Right(override val amount: Int) extends Instruction(+1, amount)

    def delta: Int =
      sign * (amount % SIZE)

  case class Spinner(value: Int = 50):
    def move(instruction: Instruction): Spinner =
      copy(value = Math.floorMod(value + instruction.delta, SIZE))

    // Count the clicks every time we hit/pass 0
    def moveWithCount(instruction: Instruction): (Spinner, Int) =
      val next = value + instruction.delta
      val remainderHitsZero = next >= SIZE || (value > 0 && next <= 0)
      val clicks = instruction.amount / SIZE + (if remainderHitsZero then 1 else 0)
      (copy(value = Math.floorMod(next, SIZE)), clicks)

  val parseDirection = Parser.charIn('L', 'R').asInstanceOf[Parser['L' | 'R']]
  val parser = (parseDirection ~ Parsers.int).map {
    case ('L', v) => Instruction.Left(v)
    case ('R', v) => Instruction.Right(v)
  }

  def parse(input: Input): Stream[IO, Instruction] =
    input.flatMap(parser.parseAllStream)

  override def part1(input: Input): Output = {
    parse(input).scan(Spinner()) {_.move(_)}
      .filter(_.value == 0)
      .compile
      .count
  }

  override def part2(input: Input): Output = {
    parse(input).scan((Spinner(), 0)) { case ((s, c), i) =>
        val (s1, c1) = s.moveWithCount(i)
        (s1, c + c1)
      }
      .compile
      .lastOrError
      .map(_._2)
  }
}
