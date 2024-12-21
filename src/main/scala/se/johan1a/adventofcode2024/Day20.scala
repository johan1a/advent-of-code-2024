package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day20:

  var cache = Map[(Pos, Int, Option[Vec2]), Int]()

  def part1(input: Seq[String], targetSaved: Int = 100): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    val (originalLength, originalShortestPath) = shortestPath(grid, start, end)
    val posToIndex = originalShortestPath.zipWithIndex.map { (pos, i) => pos -> i }.toMap
    println(s"originalShortestPath length: $originalLength")
    cache = Map()
    val result = shortestPathCheat(grid, start, 0, None, end, originalLength - targetSaved, Seq(start), posToIndex)
    result

  private def shortestPathCheat(
      grid: Grid,
      pos: Vec2,
      dist: Int,
      hasCheatedAt: Option[Vec2],
      end: Vec2,
      target: Int,
      path: Seq[Vec2],
      posToIndex: Map[Pos, Int]
  ): Int =
    if cache.contains((pos, dist, hasCheatedAt)) then
      val res = cache((pos, dist, hasCheatedAt))
      res
    else
      val result = if dist > target then
        0
      else if pos == end then
        println(dist)
        if dist <= target then
          1
        else
          0
      else
        val possibleNeighbors = getNeighbors(grid, pos, hasCheatedAt.isDefined)
        val betterNeighbors = possibleNeighbors.filter { neighbor =>
          val i = posToIndex(neighbor._1)
          i > posToIndex(pos)
        }

        if path == Seq(Vec2(1, 1), Vec2(1, 2)) then
          var x = 3

        val results = betterNeighbors
          .map { (neighbor, cheatedNow) =>
            val d = manhattan(pos, neighbor).toInt
            val cheat = cheatedNow || hasCheatedAt.isDefined
            val cheatPos = if cheatedNow then Some(pos) else hasCheatedAt
            val res = if path.contains(neighbor) then
              0
            else
              shortestPathCheat(grid, neighbor, dist + d, cheatPos, end, target, path :+ neighbor, posToIndex)
            (neighbor, cheat, res)
          }

        results.map(_._3).sum

      if pos == Vec2(3, 2) && dist == 3 && hasCheatedAt.isDefined then
        var x = 3

      if cache.contains((pos, dist, hasCheatedAt)) && result != cache((pos, dist, hasCheatedAt)) then
        var x = 3
      cache = cache + ((pos, dist, hasCheatedAt) -> result)
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

  def part2(input: Seq[String]): Int =
    -1
