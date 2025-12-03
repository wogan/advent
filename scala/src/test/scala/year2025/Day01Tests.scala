package dev.wogan.advent.scala
package year2025

import year2025.Day01.{Instruction, Spinner}

import munit.FunSuite

class SpinnerSuite extends FunSuite {

  // ---------- basic no-op / non-wrap ----------

  test("no movement does nothing, no clicks") {
    val s = Spinner(42)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(0))
    assertEquals(s2.value, 42)
    assertEquals(clicks, 0)
  }

  test("move right without wrap does not hit 0") {
    val s = Spinner(10)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(20)) // 10 -> 30
    assertEquals(s2.value, 30)
    assertEquals(clicks, 0)
  }

  test("move left without wrap and not reaching 0 does not hit 0") {
    val s = Spinner(50)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(10)) // 50 -> 40
    assertEquals(s2.value, 40)
    assertEquals(clicks, 0)
  }

  // ---------- landing exactly on 0 (no extra wrap) ----------

  test("move left landing exactly on 0 counts as hit") {
    val s = Spinner(10)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(10)) // 10..1 -> 0
    assertEquals(s2.value, 0)
    assertEquals(clicks, 1)
  }

  test("move right landing exactly on 0 via wrap counts as hit") {
    val s = Spinner(99)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(1)) // 99 -> 0
    assertEquals(s2.value, 0)
    assertEquals(clicks, 1)
  }

  // ---------- wraps that cross 0 once ----------

  test("right: wrap over boundary counts a hit") {
    val s = Spinner(99)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(2)) // 99 -> 0 -> 1
    assertEquals(s2.value, 1)
    assertEquals(clicks, 1)
  }

  test("left: wrap over boundary counts a hit") {
    val s = Spinner(2)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(10)) // 2 -> 1 -> 0 -> 99..92
    assertEquals(s2.value, 92)
    assertEquals(clicks, 1)
  }

  // ---------- starting at 0 special cases ----------

  test("start at 0 and move right without reaching 0 again: no hit") {
    val s = Spinner(0)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(20)) // 0 -> 20
    assertEquals(s2.value, 20)
    assertEquals(clicks, 0)
  }

  test("start at 0 and move left without reaching 0 again: no hit") {
    val s = Spinner(0)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(20)) // 0 -> 99..80
    assertEquals(s2.value, 80)
    assertEquals(clicks, 0)
  }

  // ---------- pure full spins (multiples of SIZE, remainder = 0) ----------

  test("pure full spins from non-zero hit 0 once per spin") {
    val s = Spinner(37)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(300)) // 3 full spins, rem=0
    assertEquals(s2.value, 37)
    assertEquals(clicks, 3) // each full spin hits 0 once
  }

  test("pure full spins from 0 hit 0 once per spin") {
    val s = Spinner(0)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(200)) // 2 full spins, rem=0
    assertEquals(s2.value, 0)
    assertEquals(clicks, 2)
  }

  // ---------- full spins + remainder (right) ----------

  test("right: full spins + remainder that does NOT cross 0") {
    val s = Spinner(10)
    // 120 = 1 full spin (1 hit) + rem 20 => 10 -> 30, no extra hit
    val (s2, clicks) = s.moveWithCount(Instruction.Right(120))
    assertEquals(s2.value, 30)
    assertEquals(clicks, 1)
  }

  test("right: full spins + remainder that DOES cross 0") {
    val s = Spinner(90)
    // 120 = 1 full spin (1 hit) + rem 20 => 90..99 -> 0..10 (one more hit)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(120))
    assertEquals(s2.value, 10)
    assertEquals(clicks, 2) // spin + remainder
  }

  // ---------- full spins + remainder (left) ----------

  test("left: full spins + remainder that does NOT cross 0") {
    val s = Spinner(80)
    // 120 = 1 spin (1 hit) + rem 20 => 80 -> 60 (does not cross 0 in remainder)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(120))
    assertEquals(s2.value, 60)
    assertEquals(clicks, 1)
  }

  test("left: full spins + remainder that DOES cross 0") {
    val s = Spinner(10)
    // 120 = 1 spin (1 hit) + rem 20
    // remainder: 10 -> 0 -> 99..90 (one more hit)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(120))
    assertEquals(s2.value, 90)
    assertEquals(clicks, 2)
  }

  // ---------- gnarlier edge cases around 0 ----------

  test("left: starting at 1 with large amount that wraps multiple times") {
    val s = Spinner(1)
    // 201 = 2 spins (2 hits) + rem 1
    // remainder: 1 -> 0 (hit again)
    val (s2, clicks) = s.moveWithCount(Instruction.Left(201))
    assertEquals(s2.value, 0)
    assertEquals(clicks, 3)
  }

  test("right: starting near end with large amount that wraps multiple times") {
    val s = Spinner(99)
    // 201 = 2 spins (2 hits) + rem 1
    // remainder: 99 -> 0 (hit again)
    val (s2, clicks) = s.moveWithCount(Instruction.Right(201))
    assertEquals(s2.value, 0)
    assertEquals(clicks, 3)
  }
}
