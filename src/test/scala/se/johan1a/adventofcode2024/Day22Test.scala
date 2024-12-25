package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day22Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day22.part1(getInput("day22/test.txt")), 37327623L)
  }

  test("Part 1") {
    assertEquals(Day22.part1(getInput("day22/input.txt")), 13753970725L)
  }

  test("Part 2 test") {
    assertEquals(Day22.part2(getInput("day22/test.txt")), 23L)
  }

  test("Part 2") {
    assertEquals(Day22.part2(getInput("day22/input.txt")), -1L)
  }
