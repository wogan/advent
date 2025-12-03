package dev.wogan.advent.scala
package year2024

object Day09 extends Day(9) {

  type Block = Int

  case class FileSystem(files: List[File]) // Block?

  case class File(id: Int, size: Int, zeros: Int)

  override def part1(input: Input): Output =
    input.compile.string.map { s =>
      val x = s.toSeq.grouped(2).zipWithIndex.map { (x, i) =>
        val size = x.head.toInt
        val zeros = x.last.toInt
        File(i, size, zeros)
      }.toVector

      "0"
    }
}
