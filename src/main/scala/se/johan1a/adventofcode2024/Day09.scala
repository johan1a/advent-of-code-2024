package se.johan1a.adventofcode2024

object Day09:


  def part1(input: Seq[String]): Long =
    val line = input.head.toCharArray().map(_.toString.toInt)
    var sum = 0L
    var i = 0
    var j = line.size-1
    var pos = 0
    println(line.toSeq)
    while (i <= j)
      if i % 2 == 0 then
        val id = i / 2
        0.until(line(i)).foreach { _ =>
          println(s"pos $pos id $id sum $sum")
          sum += pos * id
          pos += 1
        }
        i += 1
      else
        val id = j / 2
        while (line(i) > 0 && line(j) > 0)
          println(s"pos $pos id $id sum $sum")
          line(i) -= 1
          line(j) -= 1
          sum += pos * id
          pos += 1
        if line(j) == 0 then
          j -= 2
        if line(i) == 0 then
          i += 1
    sum

  def part2(input: Seq[String]): Int =
    -1
