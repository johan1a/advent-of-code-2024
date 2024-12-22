package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput
import se.johan1a.adventofcode2024.Utils.Vec2

class Day21Test extends munit.FunSuite:

  test("Part 1 test0") {
    assertEquals(
      Day21.shortestPaths(Day21.arrows, Vec2(3, 1), Vec2(2, 2), true).toSet,
      Set(Seq('<', 'v'), Seq('v', '<'))
    )
  }

  test("Part 1 test1") {
    val expected: Set[Seq[Char]] = Set(
      "<A^A>^^AvvvA".toCharArray,
      "<A^A^>^AvvvA".toCharArray,
      "<A^A^^>AvvvA".toCharArray
    )
    assertEquals(Day21.shortestSequences(Seq('0', '2', '9', 'A'), Day21.numpad, true).toSet, expected)
  }

  test("Part 1 test2") {
    val code = "<A^A>^^AvvvA".toCharArray
    val actual = Day21.shortestSequences(code, Day21.arrows, true)
    assertEquals(actual.toSet.size, 32)
  }

  test("Part 1 test") {
    assertEquals(Day21.part1(getInput("day21/test.txt")), 126384L)
  }

  test("Part 1 test3") {
    val actual = Day21.topCost(Seq('8'), n = 4).toInt
    assertEquals(actual, "v<A<AA>^>AvA^<A>AAAvA^A".size)
  }

  test("Part 1 test4") {
    val actual = Day21.topCost(Seq('0'), n = 2).toInt
    assertEquals(actual, 10)
  }

  test("Part 1") {
    assertEquals(Day21.part1(getInput("day21/input.txt")), 156714L)
  }

  test("Part 2 test") {
    assertEquals(Day21.part2(getInput("day21/test.txt")), -1L)
  }

//  test("Part 2") {
//    assertEquals(Day21.part2(getInput("day21/input.txt")), -1)
//  }
