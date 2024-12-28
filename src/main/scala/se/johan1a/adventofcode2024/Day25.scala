package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.split

object Day25:

  case class Lock(heights: Seq[Int])
  case class Key(heights: Seq[Int])

  def part1(input: Seq[String]): Int =
    val (locks, keys) = parse(input)
    var sum = 0
    keys.foreach { key =>
      locks.foreach { lock =>
        if fits(key, lock) then
          sum += 1
      }
    }
    sum

  private def fits(key: Key, lock: Lock) =
    key.heights.indices.forall { i =>
      key.heights(i) + lock.heights(i) <= 5
    }

  private def parse(input: Seq[String]) =
    val splitted = split(input)
    val objects = splitted.map { lines =>
      if lines.head == "#####" then
        var heights = Seq[Int]()
        var height = 0
        var i = 0
        while i < lines.head.size do
          var j = 1
          while j < lines.size do
            if lines(j).charAt(i) == '#' then
              height += 1
            j += 1
          heights = heights :+ height
          height = 0
          i += 1
        Lock(heights)
      else
        var heights = Seq[Int]()
        var height = 0
        var i = 0
        while i < lines.head.size do
          var j = 1
          while j < lines.size - 1 do
            if lines(j).charAt(i) == '#' then
              height += 1
            j += 1
          heights = heights :+ height
          height = 0
          i += 1
        Key(heights)
    }
    val locks = objects.collect { case l: Lock => l }
    val keys = objects.collect { case k: Key => k }
    (locks, keys)
