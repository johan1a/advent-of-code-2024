package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

import scala.collection.mutable

object Day23:

  def part1(input: Seq[String]): Int =
    val neighbors = mutable.Map[String, mutable.Set[String]]()
    input.map(line =>
      val (a, b) = splitOnce(line, "-")

      if !neighbors.contains(a) then
        neighbors.put(a, mutable.Set())
      if !neighbors.contains(b) then
        neighbors.put(b, mutable.Set())

//      neighbors(a).foreach(neighbor =>
//        neighbors(neighbor).add(b)
//      )
//      neighbors(b).foreach(neighbor =>
//        neighbors(neighbor).add(a)
//      )
      neighbors(a).add(b)
      neighbors(b).add(a)
    )
    val withT = neighbors.filter { (a, nn) =>
      a.startsWith("t") && nn.size >= 2
    }
    val result = withT.flatMap { (a, nn) =>
      combinations(neighbors, a, nn.toSeq)
    }.toSet
    result.size

  private def combinations(neighbors: mutable.Map[String, mutable.Set[String]], a: String, nn: Seq[String]): Set[Set[String]] =
    var i = 0
    var combs = Set[Set[String]]()
    while i < nn.size do
      var j = i + 1
      while j < nn.size do
        val b = nn(i)
        val c = nn(j)
        if b != a && c != a && neighbors.getOrElse(b, Set.empty).contains(c) then
          combs = combs + Set(a, b, c)
        j += 1
      i += 1
    combs

  def part2(input: Seq[String]): Int =
    -1
