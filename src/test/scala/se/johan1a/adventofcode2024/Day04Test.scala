package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day04Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day04.part1(getInput("day04/test.txt")), 18L)
  }

  test("Part 1") {
    assertEquals(Day04.part1(getInput("day04/input.txt")), 2414L)
  }

  test("Part 2 test1") {
    assertEquals(Day04.part2(getInput("day04/test.txt")), 9L)
  }

  test("Part 2") {
    assertEquals(Day04.part2(getInput("day04/input.txt")), 1871L)
  }

}
