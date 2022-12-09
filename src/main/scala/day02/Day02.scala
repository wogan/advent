package dev.wogan.advent
package day02

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

object Day02 extends Day(2) {

  import Choice.*
  import Result.*

  def score(opponent: Choice, player: Choice): Int =
    player.vs(opponent).score + player.value

  val choice: Map[Char, Choice] = Map(
    'A' -> Rock,
    'B' -> Paper,
    'C' -> Scissors,
    'X' -> Rock,
    'Y' -> Paper,
    'Z' -> Scissors
  )

  val result: Map[Char, Result] = Map(
    'X' -> Loss,
    'Y' -> Draw,
    'Z' -> Win
  )

  override def part1(input: Input): Output =
    input.map(s => score(choice(s.head), choice(s.last))).reduce(_ + _)

  override def part2(input: Input): Output =
    input.map(s => {
      val opponent = choice(s.head)
      val desired = result(s.last)
      val pick = desired.forceResult(opponent)
      score(opponent, pick)
    }).reduce(_ + _)

}
