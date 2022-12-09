package dev.wogan.advent
package day02
import Result.*

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
