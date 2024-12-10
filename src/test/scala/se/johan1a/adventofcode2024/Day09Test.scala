package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.TestInputUtil.getInput
import scala.math.BigInt

class Day09Test extends munit.FunSuite:

  test("Part 1 test") {
    // 0..111....22222
    // 022111222
    //
    assertEquals(Day09.part1(getInput("day09/test0.txt")), 60L)
  }

  test("Part 1 test") {
    // 0.........11.....2222
    // 02222.....11.....
    // 0222211
    // 0*0+1*2+2*2+3*2+4*2+5*1+6*1
    assertEquals(Day09.part1(Seq("19254")), 31L)
  }

  test("Part 1 test") {
    // 035
    // ...11111
    // 11111
    // 0+1+2+3+4
    assertEquals(Day09.part1(Seq("035")), 10L)
  }

  test("Part 1 test") {
    assertEquals(Day09.part1(getInput("day09/test.txt")), 1928L)
  }

  test("Part 1") {
    assertEquals(Day09.part1(getInput("day09/input.txt")), 6353658451014L)
  }

  test("Part 2 test") {
    assertEquals(Day09.part2(getInput("day09/test.txt")), BigInt(2858))
  }

  test("Part 2 test 1") {
    assertEquals(Day09.part2(Seq("7951996395451")), BigInt(-1))
  }

  // Too low 6379331450703
  test("Part 2") {
    assertEquals(Day09.part2(getInput("day09/input.txt")), BigInt(-1))
  }
