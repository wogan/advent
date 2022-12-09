package dev.wogan.advent

import day01.Day01

class Day01Example extends DayExample {
  Day01.example(
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin
  )
    .part1(24000)
    .part2(45000)

}
