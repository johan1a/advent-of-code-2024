package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day12:

  private var seen = Set[Pos]()

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    seen = Set[Pos]()
    allPositions(grid).filterNot(seen.contains).map(calculate(grid, _)).sum

  private def calculate(grid: Utils.Grid, start: Vec2): Int =
    var queue = Seq(start)
    var area = 0
    var perimiter = 0
    val plantType = get(grid, start)
    while queue.nonEmpty do
      val pos = queue.head
      queue = queue.tail
      if !seen.contains(pos) then
        seen = seen + pos
        area += 1

        Seq(
          Vec2(0, 1),
          Vec2(0, -1),
          Vec2(1, 0),
          Vec2(-1, 0)
        ).map(p => pos + p).foreach(neighbor =>
          if getOpt(grid, neighbor).contains(plantType) then
            queue = queue :+ neighbor
          else if !inRange(grid, neighbor) || get(grid, neighbor) != plantType then
            perimiter += 1
        )

    area * perimiter

  def part2(input: Seq[String]): Int =
    -1
