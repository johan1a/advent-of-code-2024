package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day05Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day05.part1(getInput("day05/test.txt")), 143)
  }

  test("Part 1") {
    assertEquals(Day05.part1(getInput("day05/input.txt")), 5452)
  }

  test("Part 2 test") {
    assertEquals(Day05.part2(getInput("day05/test.txt")), 123)
  }

  test("Part 2") {
    assertEquals(Day05.part2(getInput("day05/input.txt")), 4598)
  }

}
