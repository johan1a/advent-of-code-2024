package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day08Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day08.part1(getInput("day08/test.txt")), 14)
  }

  test("Part 1") {
    assertEquals(Day08.part1(getInput("day08/input.txt")), 320)
  }

  test("Part 2 test") {
    assertEquals(Day08.part2(getInput("day08/test.txt")), 34)
  }

  test("Part 2") {
    assertEquals(Day08.part2(getInput("day08/input.txt")), 1157)
  }
