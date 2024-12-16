package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day16:

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    val start = find(grid, 'S').get
    val end = find(grid, 'E').get
    shortestPath(grid, start, end)

  private def shortestPath(grid: Grid, start: Pos, end: Pos) =
    val heuristic = (pos: Pos) => manhattan(pos, end)
    var cost = Map[(Pos, Dir), Int]((start, Right) -> 0)
    var toVisit = Set[(Pos, Dir)]((start, Right))
    var best = Int.MaxValue
    while toVisit.nonEmpty do
      val (pos, dir) = toVisit.minBy { case (p, d) => heuristic(p) }
      toVisit = toVisit.filter(_ != (pos, dir))

      if pos == end then
        best = Math.min(best, cost((pos, dir)))

      Seq(
        (pos + dir, dir, 1),
        (pos + turnLeft(dir), turnLeft(dir), 1001),
        (pos + turnRight(dir), turnRight(dir), 1001)
      ).foreach { (neighbor, newDir, extraCost) =>
        if get(grid, neighbor) != '#' then
          val c = cost.get((pos, dir)).map(c => c + extraCost).getOrElse(Int.MaxValue)
          val prevCost = cost.getOrElse((neighbor, newDir), Int.MaxValue)
          if c < prevCost && c <= best then
            cost = cost + ((neighbor, newDir) -> c)
            toVisit = toVisit + ((neighbor, newDir))
      }
    best

  def part2(input: Seq[String]): Int =
    -1
