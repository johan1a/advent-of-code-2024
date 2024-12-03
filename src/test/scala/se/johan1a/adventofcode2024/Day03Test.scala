package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day03Test extends munit.FunSuite {

  test("Part 1 test") {
    assertEquals(Day03.part1(getInput("day03/test.txt")), 161L)
  }

  test("Part 1 test1") {
    assertEquals(Day03.part1(getInput("day03/test1.txt")), 0L)
  }

   test("Part 1") {
     assertEquals(Day03.part1(getInput("day03/input.txt")), 170778545)
   }

  test("Part 2") {
    assertEquals(Day03.part2(getInput("day03/input.txt")), -1L)
  }

}
