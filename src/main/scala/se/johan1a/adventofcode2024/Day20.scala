package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day20:

  var cache = Map[(Pos, Int, Boolean), Int]()

  def part1(input: Seq[String], targetSaved: Int = 100): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    val (originalLength, originalShortestPath) = shortestPath(grid, start, end)
    val posToIndex = originalShortestPath.zipWithIndex.map { (pos, i) => pos -> i }.toMap
    cache = Map()
    shortestPathCheat(grid, start, 0, false, end, originalLength - targetSaved, posToIndex)

  private def shortestPathCheat(
      grid: Grid,
      pos: Vec2,
      dist: Int,
      hasCheated: Boolean,
      end: Vec2,
      target: Int,
      posToIndex: Map[Pos, Int]
  ): Int =
    if cache.contains((pos, dist, hasCheated)) then
      val res = cache((pos, dist, hasCheated))
      res
    else
      val result = if dist > target then
        0
      else if pos == end then
        if dist <= target then
          1
        else
          0
      else
        val possibleNeighbors = getNeighbors(grid, pos, hasCheated)
        val betterNeighbors = possibleNeighbors.filter { neighbor =>
          val i = posToIndex(neighbor._1)
          i > posToIndex(pos)
        }

        val results = betterNeighbors
          .map { (neighbor, cheatedNow) =>
            val d = manhattan(pos, neighbor).toInt
            val cheat = cheatedNow || hasCheated
            val res = shortestPathCheat(grid, neighbor, dist + d, cheatedNow || hasCheated, end, target, posToIndex)
            (neighbor, cheat, res)
          }

        results.map(_._3).sum

      if cache.contains((pos, dist, hasCheated)) && result != cache((pos, dist, hasCheated)) then
        var x = 3

      cache = cache + ((pos, dist, hasCheated) -> result)
      result

  private def isFree(grid: Grid, pos: Pos): Boolean =
    getOpt(grid, pos).contains('.') || getOpt(grid, pos).contains('E')

  private def printPath(grid: Grid, path: Seq[Vec2]) =
    grid.indices.foreach(y =>
      grid.head.indices.foreach { x =>
        if path.contains(Vec2(x, y)) then
          print('O')
        else
          print(get(grid, Vec2(x, y)))
      }
      println()
    )

  //  xcx
  //  cnc
  // cnPnc
  //  cnc
  //  xcx

  private def getNeighbors(grid: Grid, pos: Pos, hasCheated: Boolean): Seq[(Pos, Boolean)] =
    if pos == Vec2(7, 6) && !hasCheated then
      var x = 3
    val normalNeighbors =
      neighbors(pos, min = Vec2(0, 0), max = max(grid), includeDiagonals = false).map(p => (p, hasCheated))

    var neighborsCheating = Seq[Pos]()
    if !hasCheated then
      if !isFree(grid, pos + Vec2(1, 0)) then
        neighborsCheating = neighborsCheating :+ Vec2(2, 0)
      if !isFree(grid, pos + Vec2(-1, 0)) then
        neighborsCheating = neighborsCheating :+ Vec2(-2, 0)
      if !isFree(grid, pos + Vec2(0, -1)) then
        neighborsCheating = neighborsCheating :+ Vec2(0, -2)
      if !isFree(grid, pos + Vec2(0, 1)) then
        neighborsCheating = neighborsCheating :+ Vec2(0, 2)

      if !isFree(grid, pos + Vec2(-1, 0)) && !isFree(grid, pos + Vec2(0, -1)) then
        neighborsCheating = neighborsCheating :+ Vec2(-1, -1)
      if !isFree(grid, pos + Vec2(1, 0)) && !isFree(grid, pos + Vec2(0, -1)) then
        neighborsCheating = neighborsCheating :+ Vec2(1, -1)
      if !isFree(grid, pos + Vec2(-1, 0)) && !isFree(grid, pos + Vec2(0, 1)) then
        neighborsCheating = neighborsCheating :+ Vec2(-1, 1)
      if !isFree(grid, pos + Vec2(1, 0)) && !isFree(grid, pos + Vec2(0, 1)) then
        neighborsCheating = neighborsCheating :+ Vec2(1, 1)

    val result = (neighborsCheating.map(neighbor => (pos + neighbor, true)) ++ normalNeighbors)
      .filter((neighbor, cheated) =>
        getOpt(grid, neighbor).contains('.') || getOpt(grid, neighbor).contains('E')
      )
    result

  private def shortestPath(grid: Grid, start: Vec2, end: Vec2): (Int, Seq[Pos]) =
    var queue = Seq(start)
    var seen = Set[Pos]()
    var dist = Map[Pos, Int](start -> 0).withDefaultValue(Int.MaxValue)
    var prev = Map[Pos, Pos]()
    var found = false
    while queue.nonEmpty && !found do
      val pos = queue.head
      queue = queue.tail

      if pos == end then
        found = true

      if !seen.contains(pos) then
        seen = seen + pos
      neighbors(pos, min = Vec2(0, 0), max = max(grid), includeDiagonals = false)
        .filter(neighbor => getOpt(grid, neighbor).contains('.') || getOpt(grid, neighbor).contains('E'))
        .foreach { neighbor =>
          val d = dist(pos) + 1
          if d < dist(neighbor) then
            dist = dist + (neighbor -> d)
            prev = prev + (neighbor -> pos)
            queue = queue :+ neighbor
        }
    (dist(end), getPath(prev, end))

  private def getPath(prev: Map[Pos, Pos], pos: Vec2): Seq[Pos] =
    prev.get(pos) match
      case None         => Seq(pos)
      case Some(before) => getPath(prev, before) :+ pos

  def part2(input: Seq[String], targetSaved: Int = 100): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    val (originalLength, originalPath) = shortestPath(grid, start, end)
    val posToIndex = originalPath.zipWithIndex.map { (pos, i) => pos -> i }.toMap

    var found = 0
    originalPath.indices.reverse.foreach { i =>
      println(s"i: $i, size: ${originalPath.size}")
      val a = originalPath(i)
      0.until(i - 1).reverse.foreach { j =>
        val b = originalPath(j)
        val pathDist = i - j
        val manhattanDist = manhattan(a, b)
        if manhattanDist < pathDist && pathDist - manhattanDist >= targetSaved then {
          found += 1
        }
      }
    }

    found
