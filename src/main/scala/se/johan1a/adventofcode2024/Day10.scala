package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024
import se.johan1a.adventofcode2024.Utils.*

import scala.collection.mutable.ArrayBuffer

object Day10:

  var found = Set[Pos]()

  def part1(input: Seq[String]): Int =
    val (grid, starts) = parse(input)
    starts.map(pos =>
      found = Set.empty
      val n = search(grid, pos)
      n
    ).sum

  private def search(grid: Grid, pos: Pos): Int =
    val n = get(grid, pos)
    if n == '9' then
      if found.contains(pos) then
        0
      else
        found = found + pos
        1
    else
      neighbors(pos, includeDiagonals = false).filter(neighbor =>
        inRange(grid, neighbor) && isNumeric(get(grid, neighbor)) && get(grid, neighbor).toString.toInt == n.toString
          .toInt + 1
      ).map { neighbor =>
        search(grid, neighbor)
      }.sum

  private def isNumeric(char: Char) = char >= '0' && char <= '9'

  private def parse(input: Seq[String]): (Grid, Seq[Pos]) =
    val grid = makeGrid(input)
    val starts: Seq[Pos] = findAll(grid, '0')
    (grid, starts)

  def part2(input: Seq[String]): Int =
    -1
