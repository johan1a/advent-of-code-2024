package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

def parse(input: Seq[String]) =
  input.head.split("\\s").map(_.toString.toLong)

object Day11:

  def part1(input: Seq[String], n: Int = 25): Long =
    val stones = parse(input)

    val result = iterate(stones, n)
    result.size

  private var lookup = Map[Long, Seq[Long]]()

  def part2(input: Seq[String], n: Int = 75): Long =
    val stones = parse(input)
    lookup = Map[Long, Seq[Long]]()

    var counts: Map[Long, Long] = stones.map(stone => stone -> 1L).toMap

    0.until(n).foreach { i =>
      var newCounts = Map[Long, Long]()
      counts.foreach { case (stone, count) =>
        val results: Seq[Long] = iterate2(stone)
        results.foreach(otherStone =>
          newCounts = newCounts + (otherStone -> (newCounts.getOrElse(otherStone, 0L) + count))
        )
      }
      counts = newCounts
    }
    counts.values.sum

  private def iterate2(stone: Long): Seq[Long] =
    if lookup.contains(stone) then
      lookup(stone)
    else
      var stones = Seq(stone)
      var prevSeenSize = -1
      var seen = Set[Long]()
      prevSeenSize = seen.size
      seen = seen ++ stones.distinct
      val newStones = stones.flatMap { stone =>
        if stone == 0 then
          Seq(1L)
        else
          val digits = stone.toString
          if digits.length % 2 == 0 then
            val a = digits.take(digits.length / 2).toLong
            val b = digits.drop(digits.length / 2).toLong
            Seq(a, b)
          else
            Seq(stone * 2024L)
      }
      stones = newStones
      lookup = lookup + (stone -> stones)
      stones

  private def iterate(firstStones: Seq[Long], seconds: Int) =
    var stones = firstStones
    0.until(seconds).foreach: i =>
      val newStones = stones.flatMap: stone =>
        if stone == 0 then
          Seq(1L)
        else
          val digits = stone.toString
          if digits.length % 2 == 0 then
            val a = digits.take(digits.length / 2).toLong
            val b = digits.drop(digits.length / 2).toLong
            Seq(a, b)
          else
            Seq(stone * 2024L)
      stones = newStones
    stones
