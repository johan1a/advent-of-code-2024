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
    input
      .map(parse)
      .map((n, nn) => calculate2(n, nn))
      .filter(_._2)
      .map(_._1)
      .sum

  private def calculate(n: Long, numbers: Seq[Long]): (Long, Boolean) =
    val result = calculate(n, numbers.head, numbers.tail)
    (n, result)

  private def calculate(n: Long, sum: Long, remaining: Seq[Long]): Boolean =
    remaining match
      case Seq() => n == sum
      case next +: tail =>
        calculate(n, sum + next, tail)
        || calculate(n, sum * next, tail)

  private def calculate2(n: Long, numbers: Seq[Long]): (Long, Boolean) =
    val result = calculate2(n, numbers.head, numbers.tail)
    (n, result)

  private def calculate2(n: Long, sum: Long, remaining: Seq[Long]): Boolean =
    remaining match
      case Seq() => n == sum
      case next +: tail =>
        calculate2(n, sum + next, tail)
        || calculate2(n, sum * next, tail)
        || calculate2(n, combine(sum, next), tail)

  def combine(a: Long, b: Long) =
    (a.toString + b.toString).toLong

  private def parse(line: String) =
    val splitted = line.split(":").map(_.trim)
    val numbers = splitted.last.split("\\s").map(_.toLong)
    (splitted.head.toLong, numbers)
