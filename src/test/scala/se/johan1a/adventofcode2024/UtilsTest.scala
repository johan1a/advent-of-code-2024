package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils

class UtilsTest extends munit.FunSuite {

  test("numbers should match all the numbers in a string") {
    val input = " abla 0anteiosratno829-4 5  x"

    val expected = Seq[Long](0, 829, -4, 5)

    val actual = Utils.numbers(input)

    assertEquals(actual, expected)
  }

}
