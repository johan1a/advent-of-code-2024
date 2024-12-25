package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day23Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day23.part1(getInput("day23/test.txt")), 7)
  }

  test("Part 1") {
    assertEquals(Day23.part1(getInput("day23/input.txt")), 1348)
  }

  test("Part 2") {
    assertEquals(Day23.part2(getInput("day23/input.txt")), -1)
  }
