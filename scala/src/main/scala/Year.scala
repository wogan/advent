package dev.wogan.advent.scala

opaque type Year = Int
object Year:
  def apply(year: Int): Year = year
  
  extension (y: Year)
    def value: Int = y
