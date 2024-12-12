package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day11Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day11.part1(getInput("day11/test.txt")), 55312L)
  }

  test("Part 1") {
    assertEquals(Day11.part1(getInput("day11/input.txt")), 186175L)
  }

  test("Part 2 test") {
    val n = 25
    val day1Result = Day11.part1(getInput("day11/test.txt"), n)
    assertEquals(Day11.part2(getInput("day11/test.txt"), n), day1Result)
  }

  test("Part 2") {
    assertEquals(Day11.part2(getInput("day11/input.txt")), 220566831337810L)
  }
