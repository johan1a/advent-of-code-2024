package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day06:

  def part1(input: Seq[String]): Int =
    val grid = makeGrid(input)
    var pos = findStartPos(grid)
    val (seen, _, _) = walk(grid, pos)
    seen.map(_._1).size

  def part2(input: Seq[String]): Int =
    val grid = makeGrid(input)
    var pos = findStartPos(grid)
    val seen = walk(grid, pos)._1.map(_._1)
    seen.filter(producesLoop(grid, pos, _)).size

  def producesLoop(grid: Grid, startPos: Pos, obstacle: Pos): Boolean =
    val original = get(grid, obstacle)
    set(grid, obstacle, '#')

    val (seen, pos, dir) = walk(grid, startPos)

    set(grid, obstacle, original)
    seen.contains((pos, dir))

  def walk(grid: Grid, pos: Pos): (Set[(Pos, Dir)], Pos, Dir) =
    walk(grid, Set(), pos, Dir(0, -1))

  def walk(grid: Grid, seen: Set[(Pos, Dir)], pos: Pos, dir: Dir): (Set[(Pos, Dir)], Pos, Dir) =
    if !inRange(grid, pos) || seen.contains((pos, dir)) then
      (seen, pos, dir)
    else
      val newSeen = seen + ((pos, dir))
      val nextPos = move(pos, dir)
      getOpt(grid, nextPos) match
        case Some('#') =>
          walk(grid, newSeen, pos, turnRight(dir))
        case _ =>
          walk(grid, newSeen, nextPos, dir)

  def findStartPos(grid: Grid): Pos = find(grid, '^').get
