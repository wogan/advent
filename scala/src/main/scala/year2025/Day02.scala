package dev.wogan.advent.scala
package year2025

import cats.data.NonEmptyList
import cats.effect.IO
import cats.parse.Parser

import scala.language.implicitConversions

object Day02 extends Day(2) {

  val parser = ((Parsers.long <* Parser.char('-')) ~ Parsers.long).repSep(Parser.char(','))

  def isInvalid(number: Long): Boolean =
    val string = number.abs.toString
    val (s1, s2) = string.splitAt(string.length / 2)
    s1 == s2


  def isInvalidPart2(number: Long): Boolean = {
    def isInvalidSeq(seq: List[String]): Boolean =
      seq.size > 1 && seq.toSet.size == 1

    def getCombinations(s: String): List[List[String]] =
      s.length.divisors.toList.map { size =>
        split(s, size.toInt)
      }

    def split(s: String, size: Int): List[String] =
      List.range(0, s.length, size).map { i => s.slice(i, i + size) }

    getCombinations(number.abs.toString).exists(isInvalidSeq)
  }

  def inputAsListOfNumbers(input: Input): IO[NonEmptyList[Long]] =
    input.compile.lastOrError
      .flatMap(parser.parseAllIO)
      .map {
        _.flatMap {
          case (start, end) =>
            NonEmptyList.fromListUnsafe(List.range(start, end + 1))
        }
      }

  override def part1(input: Input): Output =
    inputAsListOfNumbers(input).map { itemsToCheck =>
      itemsToCheck.filter(isInvalid).sum
    }

  override def part2(input: Input): Output =
    inputAsListOfNumbers(input).map { itemsToCheck =>
      itemsToCheck.filter(isInvalidPart2).sum
    }
}
