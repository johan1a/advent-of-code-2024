package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day12Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day12.part1(getInput("day12/test0.txt")), 140)
  }

  test("Part 1 test") {
    assertEquals(Day12.part1(getInput("day12/test1.txt")), 772)
  }

  test("Part 1 test") {
    assertEquals(Day12.part1(getInput("day12/test2.txt")), 1930)
  }

  test("Part 1") {
    assertEquals(Day12.part1(getInput("day12/input.txt")), 1446042)
  }

  test("Part 2 test") {
    assertEquals(Day12.part2(getInput("day12/test0.txt")), 80)
  }

  test("Part 2 test 1") {
    assertEquals(Day12.part2(getInput("day12/test1.txt")), 436)
  }

  test("Part 2 test 2") {
    assertEquals(Day12.part2(getInput("day12/test3.txt")), 236)
  }

  test("Part 2 test 3") {
    assertEquals(Day12.part2(getInput("day12/test4.txt")), 368)
  }

  test("Part 2 test 4") {
    assertEquals(Day12.part2(getInput("day12/test2.txt")), 1206)
  }

   test("Part 2") {
     assertEquals(Day12.part2(getInput("day12/input.txt")), 902742)
   }
