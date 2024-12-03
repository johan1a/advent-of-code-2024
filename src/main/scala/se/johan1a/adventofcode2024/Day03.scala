package se.johan1a.adventofcode2024

import scala.util.Try

object Day03 {

  def part1(input: Seq[String]): Long = {
    input.map(parse).sum
  }

  def part2(input: Seq[String]): Long = {
    var enabled = true
    input.map { line =>
      val chars = line.toCharArray
      var i = 0
      var sum = 0L
      while (i < chars.size) {
        val (newEnabled, j) = tryParseToggle(chars, i, enabled)
        enabled = newEnabled
        i = j
        tryParseMul(chars, i)
          .map { (num, j) =>
            if (enabled) {
              sum += num
            }
            i = j
          }
          .getOrElse {
            i = i + 1
          }
      }
      sum
    }.sum
  }

  private def tryParseToggle(chars: Seq[Char], i: Int, original: Boolean): (Boolean, Int) =
    tryParse(chars, i, "do()") match {
      case Some(j) => (true, j)
      case None =>
        tryParse(chars, i, "don't()")
          .map(j => (false, j))
          .getOrElse((original, i))
    }

  private def parse(line: String): Long = {
    val chars = line.toCharArray
    var i = 0
    var sum = 0L
    while (i < chars.size) {
      tryParseMul(chars, i)
        .map { (num, j) =>
          sum += num
          i = j
        }
        .getOrElse {
          i = i + 1
        }
    }
    sum
  }

  private def tryParseMul(chars: Seq[Char], i: Int) =
    for {
      i0 <- tryParse(chars, i, "mul(")
      (a, i1) <- tryParseNum(chars, i0)
      i2 <- tryParse(chars, i1, ",")
      (b, i3) <- tryParseNum(chars, i2)
      i4 <- tryParse(chars, i3, ")")
    } yield (a * b, i4)

  private def tryParse(chars: Seq[Char], i: Int, s: String): Option[Int] = {
    val res = tryParse(chars, i, s.toCharArray)
    if (res.size == s.size) {
      Some(i + s.size)
    } else {
      None
    }
  }

  private def tryParse(chars: Seq[Char], i: Int, s: Seq[Char]): Seq[Char] =
    if (chars.size - i < s.size) {
      Seq.empty
    } else if (chars(i) == s.head) {
      val res = chars(i)
      if (s.size > 1) {
        chars(i) +: tryParse(chars, i + 1, s.drop(1))
      } else {
        Seq(chars(i))
      }
    } else {
      Seq.empty
    }

  private def tryParseNum(chars: Seq[Char], i: Int): Option[(Long, Int)] = {
    var digits = Seq[Char]()
    var j = i
    while (j < chars.size && chars(j).isDigit && digits.size < 3) {
      digits = digits :+ chars(j)
      j += 1
    }
    Try(digits.mkString("").toLong).toOption.map(num => (num, j))
  }
}
