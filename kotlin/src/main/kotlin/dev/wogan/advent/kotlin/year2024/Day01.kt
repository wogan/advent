package dev.wogan.advent.kotlin.year2024

import dev.wogan.advent.kotlin.partitionOn
import dev.wogan.advent.kotlin.readInput
import dev.wogan.advent.kotlin.separate
import kotlin.math.abs

val whitespace = "\\s+".toRegex()

fun wellFormed(input: List<String>): Pair<List<Int>, List<Int>> =
    input.map {
        val (a, b) = it.partitionOn(whitespace)
        a.toInt() to b.toInt()
    }.separate()

fun part1(input: List<String>): Int {
    val (a, b) = wellFormed(input)
    return a.sorted().zip(b.sorted()).sumOf { (a, b) -> abs(a - b) }
}

fun part2(input: List<String>): Int {
    val (a, b) = wellFormed(input)
    val map = b.groupingBy { it }.eachCount()
    return a.sumOf { it * (map[it] ?: 0) }
}

fun main() {
    val input = readInput(1, 2024)
    println("Part 1: ${part1(input)}")
    println("Part 2: ${part2(input)}")
}

