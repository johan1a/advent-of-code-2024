package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput

class Day13Test extends munit.FunSuite:

  test("Part 1 test") {
    assertEquals(Day13.part1(getInput("day13/test.txt")), 480)
  }

  test("Part 1") {
    assertEquals(Day13.part1(getInput("day13/input.txt")), 36758)
  }

  test("Part 2 test") {
    assertEquals(Day13.part2(getInput("day13/test1.txt"), k = 0), BigInt(280))
  }

  test("Part 2 test 2") {
    assertEquals(Day13.part2(getInput("day13/test2.txt"), k = 0), BigInt(480))
  }

  // a=118679050709
  // b=103199174542
  test("Part 2 test 3") {
    assertEquals(Day13.part2(getInput("day13/test3.txt")), BigInt(875318608908L))
  }

  // Too low 114835
  // Too high 151170889039412
  // Too high 151170889039056
  // Wrong    68246630720807
  // Wrong    66242756861809
  // Wrong    2370801832749
  // Wrong    2949033126401
  // Wrong    67216329353816
  // Wrong    69242541924166
  test("Part 2") {
    assertEquals(Day13.part2(getInput("day13/input.txt")), BigInt(76358113886726L))
  }
