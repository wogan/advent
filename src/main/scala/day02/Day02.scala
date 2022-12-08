package dev.wogan.advent
package day02

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream

object Day02 extends Day(2) {

  import Choice.*
  import Result.*

  enum Result {
    case Win, Loss, Draw

    def score: Int = this match
      case Win => 6
      case Loss => 0
      case Draw => 3

    def forceResult(opponent: Choice): Choice = this match
      case Win => opponent.loses
      case Loss => opponent.beats
      case Draw => opponent
  }

  enum Choice {
    case Rock, Paper, Scissors

    def beats: Choice = this match
      case Rock => Scissors
      case Paper => Rock
      case Scissors => Paper

    def loses: Choice = this match
      case Rock => Paper
      case Paper => Scissors
      case Scissors => Rock

    def vs(other: Choice): Result =
      (this, other) match {
        case (Rock, Scissors) => Win
        case (Scissors, Paper) => Win
        case (Paper, Rock) => Win
        case (x, y) if x == y => Draw
        case _ => Loss
      }

    def value: Int = this.ordinal + 1
  }

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

  val example: String =
    """A Y
      |B X
      |C Z""".stripMargin

  def part1: Stream[IO, Int] =
    input.map(s => score(choice(s.head), choice(s.last))).reduce(_ + _)

  def part2: Stream[IO, Int] =
    input.map(s => {
      val opponent = choice(s.head)
      val desired = result(s.last)
      val pick = desired.forceResult(opponent)
      score(opponent, pick)
    }).reduce(_ + _)

  override def run: IO[Unit] =
    part2.printLast
}
