package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day11:

  def part1(input: Seq[String], n: Int = 25): Long =
    var stones: Seq[Long] = input.head.split("\\s").map(_.toString.toLong)
    0.until(n).foreach { _ =>
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
    stones.size

  def part2(input: Seq[String]): Long =
    -1
