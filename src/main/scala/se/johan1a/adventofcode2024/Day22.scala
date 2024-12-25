package se.johan1a.adventofcode2024

import scala.annotation.tailrec

object Day22:

  def part1(input: Seq[String]): Long =
    val results = input.map(str => getSecretValue(str.toLong, 2000))
    results.sum

  def part2(input: Seq[String]): Long =
    val allDiffs = input.map(line =>
      val secrets = getSecrets(line.toLong, 2000)
      val prices = secrets.map(n => n % 10)
      val diffs = getDiffs(prices)
      diffs
    )

    var i = 0
    val others = allDiffs.drop(1)
    val first = allDiffs.head
    var best = 0L
    var bestSequence = Seq[Long]()
    while i < first.size do
      val diff = first(i)
      val firstPrice = diff.price
      val sum = firstPrice + others.map(otherDiffs =>
        otherDiffs.find(_._1 == diff.diffs).map(_.price).getOrElse(0L)
      ).sum
      if sum > best then
        best = sum
        bestSequence = diff.diffs

      i += 1
    best

  case class Diff(diffs: Seq[Long], price: Long)

  private def getDiffs(prices: Seq[Long]): Seq[Diff] =
    prices.sliding(5).map { nn =>
      val diffs = nn.sliding(2).map { pair =>
        pair.last - pair.head
      }.toSeq
      Diff(
        diffs,
        nn.last
      )
    }.toSeq

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

  private def getSecrets(original: Long, n: Int): Seq[Long] =
    if n == 0 then
      Seq(original)
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
      original +: getSecrets(secret, n - 1)

  private def mix(n: Long, secret: Long): Long =
    n ^ secret

  private def prune(secret: Long): Long =
    secret % 16777216L
