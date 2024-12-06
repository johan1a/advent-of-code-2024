package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput
import se.johan1a.adventofcode2024.Utils.*

class Day06Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day06.part1(getInput("day06/test.txt")), 41)
  }

  test("Part 1") {
    assertEquals(Day06.part1(getInput("day06/input.txt")), 4758)
  }

  test("Part 2 test 1") {
    val grid = makeGrid(getInput("day06/test.txt"))
    var pos = Day06.findStartPos(grid)
    val seen = Day06.walk(grid, pos)
    val actual = Day06.producesLoop(grid, pos, Vec2(3, 6))
    assertEquals(actual, true)
  }

  test("Part 2 test") {
    assertEquals(Day06.part2(getInput("day06/test.txt")), 6)
  }

  test("Part 2") {
    assertEquals(Day06.part2(getInput("day06/input.txt")), 1670)
  }

}
