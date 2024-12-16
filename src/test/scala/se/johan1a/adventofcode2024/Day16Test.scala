package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day16Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day16.part1(getInput("day16/test.txt")), 7036)
  }

  test("Part 1 test 1") {
    assertEquals(Day16.part1(getInput("day16/test1.txt")), 11048)
  }

  test("Part 1") {
    assertEquals(Day16.part1(getInput("day16/input.txt")), 104516)
  }

  test("Part 2 test") {
    assertEquals(Day16.part2(getInput("day16/test.txt")), 45)
  }

  test("Part 2 test 1") {
    assertEquals(Day16.part2(getInput("day16/test1.txt")), 64)
  }

  test("Part 2") {
    assertEquals(Day16.part2(getInput("day16/input.txt")), 545)
  }
