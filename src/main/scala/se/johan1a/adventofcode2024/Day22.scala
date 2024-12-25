package se.johan1a.adventofcode2024

import scala.annotation.tailrec

object Day22:

  def part1(input: Seq[String]): Long =
    val results = input.map(str => getSecretValue(str.toLong, 2000))
    results.sum

  def part2(input: Seq[String]): Long =
    -1

  @tailrec
  private def getSecretValue(original: Long, n: Int): Long =
    if n == 0 then
      original
    else
      var secret = original
      val a = secret * 64
      secret = mix(a, secret)
      secret = prune(secret)
      val c = secret / 32
      secret = mix(c, secret)
      secret = prune(secret)
      val d = secret * 2048
      secret = mix(d, secret)
      secret = prune(secret)
      getSecretValue(secret, n - 1)

  private def mix(n: Long, secret: Long): Long =
    n ^ secret

  private def prune(secret: Long): Long =
    secret % 16777216L
