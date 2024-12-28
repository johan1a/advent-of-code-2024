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

  private def combinations(
      neighbors: mutable.Map[String, mutable.Set[String]],
      a: String,
      nn: Seq[String]
  ): Set[Set[String]] =
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

  def part2(input: Seq[String]): String =
    val allNeighbors = mutable.Map[String, mutable.Set[String]]()
    input.map(line =>
      val (a, b) = splitOnce(line, "-")

      if !allNeighbors.contains(a) then
        allNeighbors.put(a, mutable.Set())
      if !allNeighbors.contains(b) then
        allNeighbors.put(b, mutable.Set())

      allNeighbors(a).add(b)
      allNeighbors(b).add(a)
    )

    var best = Set[String]()
    var seen = Set[String]()

    allNeighbors.foreach { (comp, neighbors) =>
      var set = Set[String](comp)
      seen = seen + comp
      neighbors.foreach { neighbor =>
        if set.forall(c => allNeighbors(neighbor).contains(c)) then
          set = set + neighbor
      }
      if set.size > best.size then
        best = set
    }

    best.toSeq.sorted.mkString(",")
