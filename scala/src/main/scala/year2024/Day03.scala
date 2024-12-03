package dev.wogan.advent.scala
package year2024

import cats.effect.IO
import cats.parse.Parser
import fs2.Stream

object Day03 extends Day(3) {

  case class Execution(enabled: Boolean, acc: Int):
    def run(instruction: Instruction): Execution =
      instruction match
        case Instruction.Mul(a, b) => Execution(enabled, if enabled then acc + (a * b) else acc)
        case Instruction.Disable => Execution(false, acc)
        case Instruction.Enable => Execution(true, acc)

  object Execution:
    val initial: Execution = Execution(true, 0)

  enum Instruction:
    case Mul(a: Int, b: Int) extends Instruction
    case Disable extends Instruction
    case Enable extends Instruction

  val mulParser: Parser[Instruction.Mul] = (Parser.string("mul(") *> Parsers.int ~ (Parser.char(',') *> Parsers.int) <* Parser.char(')')).map(Instruction.Mul.apply)
  val disable = Parser.string("don't()").as(Instruction.Disable)
  val enable = Parser.string("do()").as(Instruction.Enable)

  val part1: Parser[List[Instruction.Mul]] = Parsers.findAll(mulParser)

  val part2 = Parsers.findAll(mulParser | disable | enable)

  override def part1(input: Input): Output =
    input
      .flatMap(part1.parseAllStream)
      .flatMap(a => Stream.emits[IO, Instruction.Mul](a))
      .map(m => m.a * m.b)
      .foldMonoid // aka sum

  override def part2(input: Input): Output =
    input
      .flatMap(part2.parseAllStream)
      .flatMap(a => Stream.emits(a))
      .fold(Execution.initial)((acc, i) => acc.run(i))
      .map(_.acc.toString)
}
