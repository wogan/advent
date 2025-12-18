package dev.wogan.advent.scala
package year2025

import cats.parse.Parser

object Day05 extends Day(5) {

  case class Range(begin: Long, end: Long) {
    def contains(value: Long): Boolean =
      value >= begin && value <= end

    def overlaps(other: Range): Boolean = 
      begin <= other.end && other.begin <= end

    def merge(other: Range): List[Range] =
      if (overlaps(other) || begin + 1 == other.end || other.begin + 1 == end)
        List(Range(Math.min(begin, other.begin), Math.max(end, other.end)))
      else 
        List(this, other)
    
    def size: Long =
      end - begin + 1
  }

  val parseRange: Parser[Range] = ((Parsers.long <* Parser.char('-')) ~ Parsers.long).map((a, b) => Range(a, b))

  override def part1(input: Input): Output =
    input
      .parseUntil(parseRange, Parser.end)
      .flatMap { (ranges, stream) =>
        stream.flatMap(Parsers.long.parseAllStream)
          .filter(i => ranges.exists(_.contains(i)))
          .compile.count
      }

  override def part2(input: Input): Output =
    input
      .parseUntil(parseRange, Parser.end)
      .map { (ranges, _) =>
        val merged = ranges.sortBy(_.begin).foldLeft(List[Range]()) {
          case (Nil, r) =>
            List(r)
          case (head :: tail, r) =>
            r.merge(head) ::: tail
        }
        merged.map(_.size).sum.toString
      }
}
