package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day02Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day02.part1(getInput("day02/test.txt")), 2)
  }

  test("Part 1") {
    assertEquals(Day02.part1(getInput("day02/input.txt")), 591)
  }

  test("Part 2 test") {
    assertEquals(Day02.part2(getInput("day02/test.txt")), 4)
  }

  test("Part 2") {
    assertEquals(Day02.part2(getInput("day02/input.txt")), 621)
  }
