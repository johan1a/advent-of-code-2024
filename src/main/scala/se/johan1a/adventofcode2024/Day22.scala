package se.johan1a.adventofcode2024

import scala.annotation.tailrec

object Day22:

  def part1(input: Seq[String]): Long =
    val results = input.map(str => getSecretValue(str.toLong, 2000))
    results.sum

  def part2(input: Seq[String]): Long =
    var j = 0
    var allDiffs: Seq[Seq[Diff]] = input.map(line =>
      val secrets = getSecrets(line.toLong, 2000)
      val prices = secrets.map(n => n % 10)
      val diffs = getDiffs(prices)
      val sorted = sort(diffs)
      println(s"j: $j")
      j += 1
      sorted
    )

    var best = 0L
    var bestSequence = Seq[Long]()

    var seen = Set[Seq[Long]]()
    println("Start")

    var i = 0
    while allDiffs.nonEmpty do
      if i % 100 == 0 then
        println(i)
      i += 1
      val firstElements = allDiffs.map(d => d.head)
      val firstDiff = firstElements.minBy(nn => (nn.diffs(0), nn.diffs(1), nn.diffs(2), nn.diffs(3)))
      assert(!seen.contains(firstDiff.diffs))
      seen = seen + firstDiff.diffs
      val sum = firstElements.filter(_.diffs == firstDiff.diffs).map(_.price).sum
      if sum > best then
        best = sum
        bestSequence = firstDiff.diffs
      allDiffs = allDiffs.map { diffs =>
        if diffs.head.diffs == firstDiff.diffs then
          diffs.drop(1)
        else
          diffs
      }
      allDiffs = allDiffs.filter(_.nonEmpty)

    println(s"best: $best, best sequence: $bestSequence")
    best

  case class Diff(diffs: Seq[Long], price: Long)

  private def sort(diffs: Seq[Diff]) =
    val withoutDuplicates = removeDuplicates(diffs)
    withoutDuplicates.sortBy(nn => (nn.diffs(0), nn.diffs(1), nn.diffs(2), nn.diffs(3)))

  private def removeDuplicates(diffs: Seq[Diff]) =
    var seen = Set[Seq[Long]]()
    var without = Seq[Diff]()
    diffs.foreach(diff =>
      if !seen.contains(diff.diffs) then
        without = without :+ diff
        seen = seen + diff.diffs
    )
    without

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
