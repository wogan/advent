package dev.wogan.advent.scala
package year2025

import cats.parse.Numbers

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day03 extends Day(3) {
  
  type Battery = Int
  type Bank = List[Int]
  
  val parser = Numbers.digit.map(_.toString.toInt).rep0

  def maxJoltage(bank: Bank): Int =
    val (max, i) = bank.dropRight(1).zipWithIndex.maxBy(_._1)
    val second = bank.drop(i+1).max
    max * 10 + second
    
  def maxJoltageOverride(bank: Bank): Long =
    @tailrec
    def inner(b1: Bank, s: Int = 12, state: Long): Long =
      val (max, i) = b1.dropRight(s - 1).zipWithIndex.maxBy(_._1)
      val newState = state * 10L + max.toLong
      if (s == 1)
        newState
      else
        inner(b1.drop(i + 1), s - 1, newState)
    inner(bank, 12, 0)

  override def part1(input: Input): Output =
    input.flatMap(parser.parseAllStream)
      .filter(_.nonEmpty)
      .map(maxJoltage)
      .sum
      .compile
      .lastOrError

  override def part2(input: Input): Output =
    input.flatMap(parser.parseAllStream)
      .filter(_.nonEmpty)
      .map(maxJoltageOverride)
      .sum
      .compile
      .lastOrError

}
