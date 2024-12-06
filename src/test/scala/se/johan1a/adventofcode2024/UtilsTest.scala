package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

class UtilsTest extends munit.FunSuite:

  test("numbers should match all the numbers in a string") {
    val input = " abla 0anteiosratno829-4 5  x"

    val expected = Seq[Long](0, 829, -4, 5)

    val actual = numbers(input)

    assertEquals(actual, expected)
  }

  test("find element in grid") {
    val grid = makeGrid(Seq("123", "456", "789"))

    val actual = find(grid, '8')

    assertEquals(actual, Some(Vec2(1, 2)))
  }
