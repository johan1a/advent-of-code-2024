package se.johan1a.adventofcode2024

object Day01:

  def part1(input: Seq[String]): Int =
    val (left, right) = parse(input)
    left
      .zip(right)
      .map((a, b) => (a - b).abs)
      .sum

  def part2(input: Seq[String]): Int =
    val (left, right) = parse(input)
    val counts = right.groupBy(identity).view.mapValues(_.size)
    left.map(n => n * counts.getOrElse(n, 0)).sum

  private def parse(input: Seq[String]) =
    val splitted = input.map(line => line.split("\\s+"))
    val left = splitted.map(pair => pair.head.toInt).sorted
    val right = splitted.map(pair => pair.last.toInt).sorted
    (left, right)
