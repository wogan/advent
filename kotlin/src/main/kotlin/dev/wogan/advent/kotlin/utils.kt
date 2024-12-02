package dev.wogan.advent.kotlin

import java.math.BigInteger
import java.security.MessageDigest
import java.util.Locale
import kotlin.io.path.Path
import kotlin.io.path.readText

/**
 * Reads lines from the given input txt file.
 */
fun readInput(day: Int, year: Int): List<String> =
    Path(String.format(Locale.ENGLISH, "input/%04d/day%02d.txt", year, day)).readText().trim().lines()

/**
 * Converts string to md5 hash.
 */
fun String.md5(): String =
    BigInteger(1, MessageDigest.getInstance("MD5").digest(toByteArray()))
    .toString(16)
    .padStart(32, '0')

fun <A, B> List<Pair<A, B>>.separate(): Pair<List<A>, List<B>> =
    Pair(map { it.first }, map { it.second })

fun String.partitionOn(regex: Regex): Pair<String, String> {
    val match = regex.find(this) ?: return Pair(this, "")
    val first = take(match.range.first)
    val second = takeLast(length - match.range.last - 1)
    return first to second
}
