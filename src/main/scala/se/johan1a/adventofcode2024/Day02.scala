package se.johan1a.adventofcode2024

object Day02:

  def part1(input: Seq[String]): Int = parse(input).filter(isSafe).size

  private def isSafe(levels: Seq[Int]): Boolean =
    if levels(0) == levels(1) then
      false
    else
      val firstSign = sign(levels(0), levels(1))
      levels
        .sliding(2)
        .map(ab => (ab(0), ab(1)))
        .forall { (a, b) =>
          val diff = (b - a).abs
          diff >= 1 && diff <= 3 && sign(a, b) == firstSign
        }

  private def sign(a: Int, b: Int) = (b - a) / (b - a).abs

  def part2(input: Seq[String]): Int =
    parse(input).filter { level =>
      isSafe(level) || remove(level).exists(isSafe)
    }.size

  private def remove(line: Seq[Int]): Seq[Seq[Int]] =
    0.until(line.size).map { n =>
      line.take(n) ++ line.drop(n + 1)
    }

  private def parse(input: Seq[String]) =
    input.map(line => line.split("\\s+").map(_.toInt))
