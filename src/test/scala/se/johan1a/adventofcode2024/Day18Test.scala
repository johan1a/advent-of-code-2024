package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day18Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day18.part1(getInput("day18/test.txt"), w = 7, n = 12), 22)
  }

   test("Part 1") {
     assertEquals(Day18.part1(getInput("day18/input.txt")), 348)
   }

  // test("Part 2") {
  //   assertEquals(Day18.part2(getInput("day18/input.txt")), -1)
  // }
