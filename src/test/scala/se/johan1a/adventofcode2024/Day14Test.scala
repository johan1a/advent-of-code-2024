package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day14Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day14.part1(getInput("day14/test0.txt"), w = 11, h = 7), 12)
  }

  test("Part 1") {
    assertEquals(Day14.part1(getInput("day14/input.txt")), 222901875)
  }
//
//  test("Part 2") {
//    assertEquals(Day14.part2(getInput("day14/input.txt")), -1)
//  }
