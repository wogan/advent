package dev.wogan.advent.kotlin

abstract class KotlinDay {
    abstract fun part1(input: List<String>): Int
    abstract fun part2(): Int

    companion object {
        @JvmStatic
        fun main() {
            val input = listOf("a", "b", "c")

        }
    }
}
