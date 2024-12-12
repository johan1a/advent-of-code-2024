package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

def parse(input: Seq[String]) =
  input.head.split("\\s").map(_.toString.toLong)

object Day11:

  def part1(input: Seq[String], n: Int = 25): Long =
    val stones = parse(input)

    val result = iterate(stones, n)
    result.size

  def part1Counts(input: Seq[String], n: Int = 25): Map[Long, Int] =
    val stones = parse(input)

    val result = iterate(stones, n)
    result.groupBy(identity).mapValues(_.size).toMap

  var lookup = Map[Long, Seq[Long]]()

  def part2(input: Seq[String], n: Int = 75): Long =
    val stones = parse(input)
    lookup = Map[Long, Seq[Long]]()

    var counts: Map[Long, Long] = stones.map(stone => stone -> 1L).toMap

    0.until(n).foreach { i =>
      var newCounts = Map[Long, Long]()
      counts.foreach { case (stone, count) =>
        val results: Seq[Long] = iterate2(stone)
        results.foreach(otherStone => {
          newCounts = newCounts + (otherStone -> (newCounts.getOrElse(otherStone, 0L) + count))
        })
      }
      counts = newCounts
    }
    counts.values.sum
    // for each number in input
    // run part 1 until number of distinct / seen numbers don't increase
    // for each seen number, compute one step / second and put it in a lookup table
    // then 75 times, do
    // for key in map
    //   next = lookup(n) // obs multiple values
    //   new_map(next) += map(key)

  private def assertEquals(counts: Map[Long, Int], part1Result: Map[Long, Int]): Boolean ={
    part1Result.forall{ (k,v ) =>
      counts.getOrElse(k, 0) == v
    } && counts.forall{ (k,v ) =>
      part1Result.getOrElse(k, 0) == v
    }
  }

  private def iterate2(stone: Long): Seq[Long] =
    if lookup.contains(stone) then
      lookup(stone)
    else {
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
    }

  private def iterate(firstStones: Seq[Long], seconds: Int) =
    var stones = firstStones
    0.until(seconds).foreach { i =>
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
    }
    stones

  // 0
  // 1
  // 2024
  // 20, 24
  // 2, 0, 2, 4
  // 4048, 1, 4048, 8096
  // 40, 48, ..., 40, 48, 80, 96
  // 4, 0, 4, 8, ..., 4, 0, 4, 8, 8, 0, 9, 6
