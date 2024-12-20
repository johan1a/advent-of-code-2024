package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day19Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day19.part1(getInput("day19/test.txt")), 6)
  }

  test("Part 1") {
    assertEquals(Day19.part1(getInput("day19/input.txt")), 338)
  }

//  test("Part 2") {
//    assertEquals(Day19.part2(getInput("day19/input.txt")), -1)
//  }
