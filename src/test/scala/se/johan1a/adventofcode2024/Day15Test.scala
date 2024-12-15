package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day15Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day15.part1(getInput("day15/test0.txt")), 2028L)
  }

  test("Part 1 test") {
    assertEquals(Day15.part1(getInput("day15/test0.txt")), 10092L)
  }

  test("Part 1") {
    assertEquals(Day15.part1(getInput("day15/input.txt")), 1495147L)
  }

//  test("Part 2") {
//    assertEquals(Day15.part2(getInput("day15/input.txt")), -1)
//  }
