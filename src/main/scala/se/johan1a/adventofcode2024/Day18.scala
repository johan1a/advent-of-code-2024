package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day18:

  var cache = Map[(Pos, Int), Option[Int]]()

  def part1(input: Seq[String], w: Int = 71, n: Int = 1024): Int =
    cache = Map()
    val bytes = parse(input).take(n).toSet
    val start = Vec2(0, 0)
    val target = Vec2(w - 1, w - 1)
    shortestPath(bytes, w, Set.empty, start, target, 0).get

  def part2(input: Seq[String], w: Int = 71, lowStart: Int = 1024): Vec2 =
    val bytes = parse(input)
    val start = Vec2(0, 0)
    val target = Vec2(w - 1, w - 1)
    var low = lowStart
    var high = bytes.size - 1
    while low <= high do
      val mid = (high - low) / 2 + low
      cache = Map()
      val beforeMidVal = shortestPath(bytes.take(mid - 1).toSet, w, Set.empty, start, target, 0)
      cache = Map()
      val midVal = shortestPath(bytes.take(mid).toSet, w, Set.empty, start, target, 0)
      if beforeMidVal.isDefined && midVal.isEmpty then
        return bytes(mid - 1)
      else if beforeMidVal.isEmpty && midVal.isEmpty then
        high = mid - 1
      else
        low = mid + 1
    Vec2(-1, -1)

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
