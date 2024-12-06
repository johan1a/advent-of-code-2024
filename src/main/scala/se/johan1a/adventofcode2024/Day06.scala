package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day06 {

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    var pos = findStartPos(grid)
    walk(grid, pos).size
  }

  def part2(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    var pos = findStartPos(grid)
    val seen = walk(grid, pos)
    seen.filter(producesLoop(grid, pos, _)).size
  }

  def producesLoop(grid: Grid, startPos: Vec2, obstacle: Vec2): Boolean = {
    val original = get(grid, obstacle)
    set(grid, obstacle, '#')

    var pos = startPos
    var dir = Vec2(0, -1)
    var seen = Set[Vec2]()
    var steps = 0
    while (inRange(grid, pos) && steps <= seen.size * 2) {
      seen = seen + pos
      val nextPos = add(pos, dir)
      getOpt(grid, nextPos) match {
        case Some('#') =>
          dir = turn(dir)
        case _ =>
          pos = nextPos
          steps = steps + 1
      }
    }

    set(grid, obstacle, original)
    steps > seen.size * 2
  }

  def walk(grid: Grid, startPos: Vec2) = {
    var pos = startPos
    var dir = Vec2(0, -1)
    var seen = Set[Vec2]()
    while (inRange(grid, pos)) {
      seen = seen + pos
      val nextPos = add(pos, dir)
      getOpt(grid, nextPos) match {
        case Some('#') =>
          dir = turn(dir)
        case _ =>
          pos = nextPos
      }
    }
    seen
  }

  private def turn(dir:Vec2) = {
    dir match {
      case Vec2(1, 0) => Vec2(0, 1)
      case Vec2(0, 1) => Vec2(-1, 0)
      case Vec2(-1, 0) => Vec2(0, -1)
      case Vec2(0, -1) => Vec2(1, 0)
    }
  }

  def findStartPos(grid: Grid) = {
    var pos = Vec2(-1, -1)
    0.until(grid.size).foreach { i =>
      0.until(grid.head.size).foreach { j =>
        if (grid(i)(j) == '^') {
          pos = Vec2(j, i)
        }
      }
    }
    pos
  }
}
