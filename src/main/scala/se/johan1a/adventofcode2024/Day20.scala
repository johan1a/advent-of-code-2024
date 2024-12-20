package se.johan1a.adventofcode2024

import se.johan1a
import se.johan1a.adventofcode2024
import se.johan1a.adventofcode2024.Utils.*

object Day20:

  var cache = Map[(Pos, Int, Boolean), Int]()
  var seen = Set[Pos]()

  def part1(input: Seq[String], targetSaved: Int = 100): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    val originalShortestPath = shortestPath(grid, start, end)
//    assert(originalShortestPath == 84)
    cache = Map()
    seen = Set(start)
    val result = shortestPathCheat(grid, start, 0, false, end, originalShortestPath - targetSaved, Seq(start))
    result

  private def shortestPathCheat(
      grid: Grid,
      pos: Vec2,
      dist: Int,
      hasCheated: Boolean,
      end: Vec2,
      target: Int,
      path: Seq[Vec2]
  ): Int =
    seen = seen + pos
    if dist > target then
      0
    else if false && cache.contains((pos, dist, hasCheated)) then
      cache((pos, dist, hasCheated))
    else
      if pos == end then
        printPath(grid, path)
        println(dist)
        if dist <= target then
          1
        else
          0
      else
        val possibleNeighbors = getNeighbors(grid, pos, hasCheated)
        val nonVisitedNeighbors = possibleNeighbors
//          .filterNot(p => seen.contains(p._1))

        if path == Seq(Vec2(1, 1), Vec2(1, 2)) then
          var x = 3
        val results = nonVisitedNeighbors
          .map { (neighbor, cheatedNow) =>

            if path == Seq(Vec2(1, 1), Vec2(1, 2)) then
              var x = 3
            val d = manhattan(pos, neighbor).toInt

            val cheat = cheatedNow || hasCheated
            if cheatedNow then
              println(s"cheating at $pos -> $neighbor")
            //       seen = seen + pos
            val res = if path.contains(neighbor) then
              0
            else
              shortestPathCheat(grid, neighbor, dist + d, cheat, end, target, path :+ neighbor)
            //      seen = seen - neighbor
            if path == Seq(Vec2(1, 1), Vec2(1, 2), Vec2(3, 2)) then
              var x = 3
            (neighbor, cheat, res)
          }

        val result = results.map(_._3).sum
        cache = cache + ((pos, dist, hasCheated) -> result)

        if pos == Vec2(3, 1) || pos == Vec2(3, 2) then
          var x = 3

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

  private def shortestPath(grid: Grid, start: Vec2, end: Vec2): Int =
    var queue = Seq(start)
    var seen = Set[Pos]()
    var dist = Map[Pos, Int](start -> 0).withDefaultValue(Int.MaxValue)
    while queue.nonEmpty do
      val pos = queue.head
      queue = queue.tail

      if pos == end then
        return dist(pos)

      if !seen.contains(pos) then
        seen = seen + pos
      neighbors(pos, min = Vec2(0, 0), max = max(grid), includeDiagonals = false)
        .filter(neighbor => getOpt(grid, neighbor).contains('.') || getOpt(grid, neighbor).contains('E'))
        .foreach { neighbor =>
          val d = dist(pos) + 1
          if d < dist(neighbor) then
            dist = dist + (neighbor -> d)
            queue = queue :+ neighbor
        }
    -1

  def part2(input: Seq[String]): Int =
    -1
