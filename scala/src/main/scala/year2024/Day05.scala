package dev.wogan.advent.scala
package year2024

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.all.*

object Day05 extends Day(5) {
  // page `page` has to be printed before page `before`
  // case class Ordering(page: Int, before: Set[Int])

  type PageList = Map[Int, Set[Int]] // page -> indices
  type PagesToPrint = NonEmptyList[Int]

  case class PageOrdering(map: Map[Int, Set[Int]]):
    def validate(pagesToPrint: PagesToPrint): Boolean =
      Zipper.fromNonEmptyList(pagesToPrint).coflatMap(z => {
        // if this page is printed after it's dependents, return true
        map.get(z.extract).exists(indices => z.left.exists(indices.contains))
      }).toList.forall(!_) // ensure no true values

    def fix(pagesToPrint: PagesToPrint): PagesToPrint =
      pagesToPrint.toList.sorted(using ordering(pagesToPrint)).toNel.get

    // takes the pages to print as a parameter to filter out rules we don't need to follow
    def ordering(p: PagesToPrint): Ordering[Int] =
      val items = p.toList.toSet
      (x: Int, y: Int) =>
        def loop(a: Int, b: Int): Option[Int] =
          map.get(a).flatMap { children =>
            if children.contains(b) then Some(-1)
            else children.intersect(items).collectFirst(c => loop(c, b)).flatten
          }
        loop(x, y).getOrElse(1)

  object PageOrdering:
    def apply(pairs: List[(Int, Int)]): PageOrdering =
      PageOrdering(pairs.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap)

  val parserOrdering: Parser[(Int, Int)] = Parsers.int ~ (Parser.char('|') *> Parsers.int)
  val parsePagesToPrint: Parser[PagesToPrint] = Parsers.int.repSep(Parser.char(','))

  def middlePage(pageList: PagesToPrint): Int =
    pageList.get(pageList.size / 2).get

  override def part1(input: Input): Output =
    input
      .parseUntil(parserOrdering, Parser.end)
      .flatMap { (pairs, stream) =>
        val pageList = PageOrdering(pairs)
        stream.
          flatMap(parsePagesToPrint.parseAllStream)
          .map(p => if pageList.validate(p) then middlePage(p) else 0)
      }.sum

  override def part2(input: Input): Output =
    input
      .parseUntil(parserOrdering, Parser.end)
      .flatMap((pairs, stream) =>
        val pageList = PageOrdering(pairs)
        stream
          .flatMap(parsePagesToPrint.parseAllStream)
          .map(p => if !pageList.validate(p) then middlePage(pageList.fix(p)) else 0)
      ).sum
}
