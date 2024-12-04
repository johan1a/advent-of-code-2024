package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*
import scala.collection.mutable.ArrayBuffer

object Day04 {

  def part1(input: Seq[String]): Long = {
    val grid = makeGrid(input)
    allPositions(grid)
      .filter(pos => grid(pos.y.toInt)(pos.x.toInt) == 'X')
      .map(pos => count(grid, pos))
      .sum
  }

  private def count(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2): Long = {
    val y = pos.y.toInt
    val x = pos.x.toInt
    val diffs = Seq(
      Vec2(0, 1),
      Vec2(0, -1),
      Vec2(1, 0),
      Vec2(-1, 0),
      Vec2(1, 1),
      Vec2(-1, -1),
      Vec2(1, -1),
      Vec2(-1, 1)
    )
    diffs.filter(diff => checkXmas(grid, pos, diff)).size
  }

  private def checkXmas(grid: ArrayBuffer[ArrayBuffer[Char]], startPos: Vec2, diff: Vec2): Boolean = {
    var i = 0
    var pos = startPos
    while (inRange(grid, pos) && i < "XMAS".size && get(grid, pos) == "XMAS".charAt(i)) {
      i += 1
      pos = add(pos, diff)
    }
    i == "XMAS".size
  }

  def part2(input: Seq[String]): Long = {
    val grid = makeGrid(input)
    allPositions(grid)
      .filter(pos => gridEquals(grid, pos, 'A'))
      .filter(pos => isX(grid, pos))
      .size
  }

  private def isX(grid: ArrayBuffer[ArrayBuffer[Char]], pos: Vec2): Boolean = {
    val topLeft = Vec2(pos.x - 1, pos.y - 1)
    val bottomRight = Vec2(pos.x + 1, pos.y + 1)
    val topRight = Vec2(pos.x + 1, pos.y - 1)
    val bottomLeft = Vec2(pos.x - 1, pos.y + 1)
    val res  =
    ((gridEquals(grid, topLeft, 'M') && gridEquals(grid, bottomRight, 'S'))
      || (gridEquals(grid, topLeft, 'S') && gridEquals(grid, bottomRight, 'M')))
    &&
    ((gridEquals(grid, topRight, 'M') && gridEquals(grid, bottomLeft, 'S'))
      || (gridEquals(grid, topRight, 'S') && gridEquals(grid, bottomLeft, 'M')))
    res
  }
}
