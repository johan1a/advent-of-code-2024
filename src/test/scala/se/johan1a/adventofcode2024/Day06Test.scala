package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day06Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day06.part1(getInput("day06/test.txt")), 41)
  }

  test("Part 1") {
    assertEquals(Day06.part1(getInput("day06/input.txt")), 4758)
  }

  test("Part 2") {
    assertEquals(Day06.part2(getInput("day06/input.txt")), -1)
  }

}
