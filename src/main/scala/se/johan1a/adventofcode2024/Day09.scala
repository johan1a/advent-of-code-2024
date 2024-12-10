package se.johan1a.adventofcode2024

import scala.math.BigInt

object Day09:

  def part1(input: Seq[String]): Long =
    val line = input.head.toCharArray().map(_.toString.toInt)
    var sum = 0L
    var i = 0
    var j = line.size - 1
    var pos = 0
    while i <= j do
      if i % 2 == 0 then
        val id = i / 2
        0.until(line(i)).foreach { _ =>
          sum += pos * id
          pos += 1
        }
        i += 1
      else
        val id = j / 2
        while line(i) > 0 && line(j) > 0 do
          line(i) -= 1
          line(j) -= 1
          sum += pos * id
          pos += 1
        if line(j) == 0 then
          j -= 2
        if line(i) == 0 then
          i += 1
    sum

  def part2(input: Seq[String]): BigInt =
    val line = input.head.toCharArray().map(_.toString.toInt)
    val original = input.head.toCharArray().map(_.toString.toInt)
    var sum = BigInt(0)
    var used = Set[Int]()
    var i = 0
    var j = line.size - 1
    var pos = 0
    while i < line.size do
      assert(sum >= 0)
      println(s"i $i j $j pos $pos")

      if i % 2 == 0 then
        val id = i / 2
        if line(i) == 0 then
          pos += original(i)
        else
          0.until(line(i)).foreach { _ =>
            sum += pos * id
            assert(!used.contains(pos))
            used = used + pos
            println(s"pos $pos id $id i $i j $j k sum $sum")
            pos += 1
          }
        i += 1
        println(s"if i $i pos $pos")
      else
        if i <= j then
          val id = j / 2
          var k = i
          // keep track of max blcok size that didn't fit anywhere
          val originalPos = pos
          while k < line.size - 1 && line(k) < line(j) do
            if k == i then
              pos += line(k)
            else
              pos += original(k)
            pos += original(k + 1)
            k += 2
          if k < line.size then
            println(s"originalPos $originalPos pos $pos k $k line(k) ${line(k)}")
          else
            println(s"originalPos $originalPos pos $pos k $k")
          if k < j then
            while line(k) > 0 && line(j) > 0 do
              line(k) -= 1
              line(j) -= 1
              assert(!used.contains(pos))
              used = used + pos
              sum += pos * id
              println(s"pos $pos id $id i $i j $j k $k sum $sum")
              pos += 1
            if line(j) == 0 then
              j -= 2
          else
            j -= 2
          if i != k then
            pos = originalPos
          if i == k && line(k) == 0 then
            i += 1
        else
//          if (i == j + 1) then
//            pos += line(i)
//          else
          pos += original(i)
          i += 1
          println(s"else i $i j $j pos $pos")
    val posSum = original.sum
    println(s"i $i j $j pos line.size ${line.size} pos $pos posSum $posSum")
    sum
