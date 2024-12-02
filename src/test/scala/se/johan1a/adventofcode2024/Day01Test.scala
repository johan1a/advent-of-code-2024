package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day01Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day01.part1(getInput("day01/test.txt")), 11)
  }

  test("Part 1") {
    assertEquals(Day01.part1(getInput("day01/input.txt")), 1889772)
  }

  test("Part 2 test") {
    assertEquals(Day01.part2(getInput("day01/test.txt")), 31)
  }

  test("Part 2") {
    assertEquals(Day01.part2(getInput("day01/input.txt")), 23228917)
  }

}
