package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day24Test extends munit.FunSuite:

  test("Part 1 test0") {
    assertEquals(Day24.part1(getInput("day24/test.txt")), 4L)
  }

  test("Part 1 test1") {
    assertEquals(Day24.part1(getInput("day24/test1.txt")), 2024L)
  }

  test("Part 1") {
    assertEquals(Day24.part1(getInput("day24/input.txt")), 36902370467952L)
  }

  test("Part 2") {
    assertEquals(Day24.part2(getInput("day24/input.txt")), -1)
  }
