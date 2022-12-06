package dev.wogan.advent

import fs2.Stream
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

def loadFile(filename: String): Stream[IO, String] =
  Files[IO].readUtf8Lines(Path("src/main/resources").absolute / Path(filename))

abstract class Day(number: Int) extends IOApp.Simple {

  def input: Stream[IO, String] = 
    loadFile(f"day$number%02d/input.txt")

  def parse(input: String): Stream[IO, String] =
    Stream.emits[IO, String](input.split("\n"))

}