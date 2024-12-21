package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day20Test extends munit.FunSuite:

  test("Part 1 test 0") {
    assertEquals(Day20.part1(getInput("day20/test1.txt"), 2), 2)
  }

  test("Part 1 test 1") {
    assertEquals(Day20.part1(getInput("day20/test2.txt"), 2), 4)
  }

  test("Part 1 test 1b") {
    assertEquals(Day20.part1(getInput("day20/test2b.txt"), 2), 4)
  }

  test("Part 1 test 2") {
    assertEquals(Day20.part1(getInput("day20/test3.txt"), 2), 2)
  }

  test("Part 1 test 3") {
    assertEquals(Day20.part1(getInput("day20/test4.txt"), 2), 2)
  }

  test("Part 1 test") {
    assertEquals(Day20.part1(getInput("day20/test.txt"), 2), 44)
  }

  // -Xss20M
  test("Part 1") {
    assertEquals(Day20.part1(getInput("day20/input.txt")), 1490)
  }

  test("Part 2 test") {
    assertEquals(Day20.part2(getInput("day20/test.txt"), 74), 7)
  }

  test("Part 2") {
    assertEquals(Day20.part2(getInput("day20/input.txt")), -1)
  }
