package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day07Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day07.part1(getInput("day07/test.txt")), 3749L)
  }

  test("Part 1") {
    assertEquals(Day07.part1(getInput("day07/input.txt")), 66343330034722L)
  }

  test("Part 2 test") {
    assertEquals(Day07.part2(getInput("day07/test.txt")), 11387L)
  }

  test("Part 2 test 1") {
    assertEquals(Day07.part2(Seq("7290: 6 8 6 15 ")), 7290L)
  }

  test("Part 2 test 2") {
    assertEquals(Day07.combine(48, 6), 486L)
  }

  test("Part 2") {
    assertEquals(Day07.part2(getInput("day07/input.txt")), 637696070419031L)
  }
