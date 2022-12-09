package dev.wogan.advent
package day02

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
