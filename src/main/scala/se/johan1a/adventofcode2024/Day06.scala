package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day06 {

  def part1(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    var pos = findStartPos(grid)
    val (seen, _, _) = walk(grid, pos)
    seen.map(_._1).size
  }

  def part2(input: Seq[String]): Int = {
    val grid = makeGrid(input)
    var pos = findStartPos(grid)
    val seen = walk(grid, pos)._1.map(_._1)
    seen.filter(producesLoop(grid, pos, _)).size
  }

  def producesLoop(grid: Grid, startPos: Vec2, obstacle: Vec2): Boolean = {
    val original = get(grid, obstacle)
    set(grid, obstacle, '#')

    val (seen, pos, dir) = walk(grid, startPos)

    set(grid, obstacle, original)
    seen.contains((pos, dir))
  }

  def walk(grid: Grid, pos: Vec2): (Set[(Vec2, Vec2)], Vec2, Vec2) =
    walk(grid, Set(), pos, Vec2(0, -1))

  def walk(grid: Grid, seen: Set[(Vec2, Vec2)], pos: Vec2, dir: Vec2): (Set[(Vec2, Vec2)], Vec2, Vec2) =
    if (!inRange(grid, pos) || seen.contains((pos, dir))) {
      (seen, pos, dir)
    } else {
      val newSeen = seen + ((pos, dir))
      val nextPos = add(pos, dir)
      getOpt(grid, nextPos) match {
        case Some('#') =>
          walk(grid, newSeen, pos, turn(dir))
        case _ =>
          walk(grid, newSeen, nextPos, dir)
      }
    }

  private def turn(dir: Vec2) =
    dir match {
      case Vec2(1, 0)  => Vec2(0, 1)
      case Vec2(0, 1)  => Vec2(-1, 0)
      case Vec2(-1, 0) => Vec2(0, -1)
      case Vec2(0, -1) => Vec2(1, 0)
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
