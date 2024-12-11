package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day10Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day10.part1(getInput("day10/test.txt")), 1)
  }

  test("Part 1 test") {
    assertEquals(Day10.part1(getInput("day10/test2.txt")), 2)
  }

  test("Part 1 test") {
    assertEquals(Day10.part1(getInput("day10/test3.txt")), 4)
  }

  test("Part 1 test") {
    assertEquals(Day10.part1(getInput("day10/test4.txt")), 3)
  }

  test("Part 1 test") {
    assertEquals(Day10.part1(getInput("day10/test1.txt")), 36)
  }

  test("Part 1") {
    assertEquals(Day10.part1(getInput("day10/input.txt")), 607)
  }

//   test("Part 2") {
//     assertEquals(Day10.part2(getInput("day10/input.txt")), -1)
//   }
