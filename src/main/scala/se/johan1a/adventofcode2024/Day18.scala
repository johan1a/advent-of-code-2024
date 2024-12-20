package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day18:

  var cache = Map[(Pos, Int), Option[Int]]()

  // only works through sbt due to xss / max stack size
  def part1(input: Seq[String], w: Int = 71, n: Int = 1024): Int =
    cache = Map()
    val bytes = parse(input).take(n).toSet
    val start = Vec2(0, 0)
    val target = Vec2(w - 1, w - 1)
    shortestPath(bytes, w, Set.empty, start, target, 0).get

  def part2(input: Seq[String]): Int =
    -1

  private def shortestPath(bytes: Set[Pos], w: Int, seen: Set[Pos], pos: Pos, target: Pos, dist: Int): Option[Int] =
    if pos == target then
      Some(dist)
    else
      if cache.contains((pos, dist)) then
        cache((pos, dist))
      else
        val neighbors = Utils.neighbors(pos, min = Vec2(0, 0), max = Vec2(w, w), includeDiagonals = false)
          .filter(n => !seen.contains(n) && !bytes.contains(n))
        val result = neighbors.flatMap(n => shortestPath(bytes, w, seen + n, n, target, dist + 1)).minOption
        cache = cache + ((pos, dist) -> result)
        result

  private def parse(input: Seq[String]): Seq[Pos] =
    input.map { line =>
      val splitted = numbers(line)
      Vec2(splitted.head, splitted.last)
    }
