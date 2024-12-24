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

//  test("Part 1 test3") {
//    val actual = Day21.topCost(Seq('8'), n = 4).toInt
//    assertEquals(actual, "v<A<AA>^>AvA^<A>AAAvA^A".size)
//  }

  test("Part 1 test4b") {
    val actual = Day21.topCost(Seq('0'), n = 1).toInt
    assertEquals(actual, 8)
  }

  test("Part 1 test5") {
    val actual = Day21.topCost(Seq('0', '2'), n = 1).toInt
    assertEquals(actual, 12)
  }

  test("Part 1 test6") {
    val actual = Day21.topCost(Seq('0'), n = 2).toInt
    assertEquals(actual, 18)
  }

  test("Part 1 test7") {
    val actual = Day21.topCost("02".toCharArray, n = 2).toInt
    assertEquals(actual, 30)
  }

  test("Part 1 test7b") {
    val actual = Day21.topCost(Seq('0'), n = 3).toInt
    assertEquals(actual, 46)
  }

  test("Part 1 test8") {
    val actual = Day21.topCost("029A".toCharArray, n = 2).toInt
    assertEquals(actual, 68)
  }

  test("Part 1 test9a") {
    val actual = Day21.topCost("029".toCharArray, n = 3).toInt
    assertEquals(actual, 122)
  }

  test("Part 1 test9bb") {
    val actual = Day21.topCost("9".toCharArray, n = 3).toInt
    assertEquals(actual, 30)
  }

  test("Part 1 test9bbb") {
    val actual = Day21.topCost("A".toCharArray, n = 3).toInt
    assertEquals(actual, 1)
  }

  test("Part 1 test9ba") {
    val actual = Day21.topCost("9A".toCharArray, n = 2).toInt
    assertEquals(actual, 32)
  }

  test("Part 1 test9b") {
    val actual = Day21.topCost("9A".toCharArray, n = 3).toInt
    assertEquals(actual, 72)
  }

  test("Part 1 test9c") {
    val actual = Day21.topCost("029A".toCharArray, n = 2).toInt
    assertEquals(actual, 68)
  }

  test("Part 1 test9") {
    val actual = Day21.topCost("029A".toCharArray, n = 3).toInt
    assertEquals(actual, 164)
  }

  test("Part 1") {
    assertEquals(Day21.part1(getInput("day21/input.txt")), 156714L)
  }

  test("Part 2 test") {
    assertEquals(Day21.part2(getInput("day21/test.txt")), -1L)
  }

  // Too high 351184180821832
  test("Part 2") {
    assertEquals(Day21.part2(getInput("day21/input.txt")), -1L)
  }
