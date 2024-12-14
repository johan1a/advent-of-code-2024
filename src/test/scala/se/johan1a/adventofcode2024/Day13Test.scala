package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day13Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day13.part1(getInput("day13/test.txt")), 480)
  }

   test("Part 1") {
     assertEquals(Day13.part1(getInput("day13/input.txt")), 36758)
   }

  // test("Part 2") {
  //   assertEquals(Day13.part2(getInput("day13/input.txt")), -1)
  // }
