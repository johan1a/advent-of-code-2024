package se.johan1a.adventofcode2024

object Day01 {

  def part1(input: Seq[String]): Int = {
    val splitted = input.map(line => line.split("\\s+"))
    val left = splitted.map(pair => pair.head.toInt).sorted
    val right = splitted.map(pair => pair.last.toInt).sorted

    left
      .zip(right)
      .map((a, b) => (a - b).abs)
      .sum
  }

  def part2(input: Seq[String]): Int = {
    val splitted = input.map(line => line.split("\\s+"))
    val left = splitted.map(pair => pair.head.toInt)
    val right = splitted.map(pair => pair.last.toInt)
    val counts = right.groupBy(identity).mapValues(_.size)
    left.map(n => n * counts.getOrElse(n, 0)).sum
  }
}
