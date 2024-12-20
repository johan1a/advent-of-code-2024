package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day17Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day17.part1(getInput("day17/test.txt")), "4,6,3,5,6,3,5,2,1,0")
  }

  test("Part 1") {
    assertEquals(Day17.part1(getInput("day17/input.txt")), "1,5,3,0,2,5,2,5,3")
  }

  test("Part 2 test") {
    assertEquals(Day17.part2(getInput("day17/test1.txt")), 117440L)
  }

  // Too low 35184372088831
  // too high 7783986802926190962
  // too high 218534458065266
  test("Part 2") {
    assertEquals(Day17.part2c(getInput("day17/input.txt")), 108107566389757L)
  }
