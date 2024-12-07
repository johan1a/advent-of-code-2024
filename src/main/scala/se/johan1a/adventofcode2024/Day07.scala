package se.johan1a.adventofcode2024

object Day07:

  def part1(input: Seq[String]): Long =
    input
      .map(parse)
      .map((n, nn) => calculate(n, nn))
      .filter(_._2)
      .map(_._1)
      .sum

  def part2(input: Seq[String]): Long =
    -1

  private def calculate(n: Long, numbers: Seq[Long]): (Long, Boolean) =
    val result = calculate(n, numbers.head, numbers.tail)
    (n, result)

  private def calculate(n: Long, sum: Long, remaining: Seq[Long]): Boolean =
    if remaining.isEmpty then n == sum
    else
      val next = remaining.head
      calculate(n, sum + next, remaining.tail) || calculate(n, sum * next, remaining.tail)

  private def parse(line: String) =
    val splitted = line.split(":").map(_.trim)
    val numbers = splitted.last.split("\\s").map(_.toLong)
    (splitted.head.toLong, numbers)
