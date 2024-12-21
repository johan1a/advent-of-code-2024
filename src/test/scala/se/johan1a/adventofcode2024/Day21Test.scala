package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput
import se.johan1a.adventofcode2024.Utils.Vec2

class Day21Test extends munit.FunSuite:

  test("Part 1 test0") {
    assertEquals(Day21.shortestPaths(Day21.arrows, Vec2(3, 1), Vec2(2, 2)).toSet, Set(Seq('<', 'v'), Seq('v', '<')))
  }

  test("Part 1 test1") {
    val expected: Set[Seq[Char]] = Set(
      "<A^A>^^AvvvA".toCharArray,
      "<A^A^>^AvvvA".toCharArray,
      "<A^A^^>AvvvA".toCharArray
    )
    assertEquals(Day21.shortestSequences(Seq('0', '2', '9', 'A'), Day21.numpad).toSet, expected)
  }

  test("Part 1 test") {
    assertEquals(Day21.part1(getInput("day21/test.txt")), 126384L)
  }
//
//  test("Part 1") {
//    assertEquals(Day21.part1(getInput("day21/input.txt")), -1)
//  }
//
//  test("Part 2") {
//    assertEquals(Day21.part2(getInput("day21/input.txt")), -1)
//  }
