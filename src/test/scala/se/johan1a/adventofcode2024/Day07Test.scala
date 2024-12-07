package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day07Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day07.part1(getInput("day07/test.txt")), 3749L)
  }

  test("Part 1") {
    assertEquals(Day07.part1(getInput("day07/input.txt")), 66343330034722L)
  }

  // test("Part 2") {
  //   assertEquals(Day07.part2(getInput("day07/input.txt")), -1)
  // }
