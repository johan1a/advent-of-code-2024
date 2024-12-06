package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*
import scala.collection.mutable.ArrayBuffer

object Day05:

  def part1(input: Seq[String]): Int =
    val splitted = split(input)
    val dependencies = parseRules(splitted.head)
    parseUpdates(splitted.last)
      .filter(valid(dependencies, _))
      .map(getMiddle)
      .sum

  def part2(input: Seq[String]): Int =
    val splitted = split(input)
    val dependencies = parseRules(splitted.head)
    parseUpdates(splitted.last)
      .filter(!valid(dependencies, _))
      .map(sort(dependencies, _))
      .map(getMiddle)
      .sum

  private def sort(allDependencies: Map[Int, Set[Int]], updates: Seq[Int]): Seq[Int] =
    var indegrees = computeIndegrees(allDependencies, updates)
    var topLevel = updates.filter(u => indegrees(u) == 0)
    var result = Seq[Int]()
    while topLevel.nonEmpty do
      val n = topLevel.head
      topLevel = topLevel.tail
      result = result :+ n
      allDependencies.foreach { case (neighbor, dependents) =>
        if dependents.contains(n) then
          indegrees = indegrees + (neighbor -> (indegrees(neighbor) - 1))
          if indegrees(neighbor) == 0 && updates.contains(neighbor) then topLevel = topLevel :+ neighbor
      }
    result

  private def computeIndegrees(allDependencies: Map[Int, Set[Int]], updates: Seq[Int]) =
    allDependencies
      .map((n, neighbors) => (n -> neighbors.filter(updates.contains).size))
      .toMap
      .withDefaultValue(0)

  private def valid(allDependencies: Map[Int, Set[Int]], updates: Seq[Int]): Boolean =
    var done = Set[Int]()
    updates.forall { n =>
      val dependencies = allDependencies(n).filter(updates.contains)
      if dependencies.isEmpty || dependencies.forall(done.contains) then
        done = done + n
        true
      else false
    }

  private def getMiddle(updates: Seq[Int]) = updates(updates.size / 2)

  private def parseRules(lines: Seq[String]) =
    var dependencies = Map[Int, Set[Int]]().withDefaultValue(Set.empty)
    lines.map(l =>
      val splitted = l.split("\\|")
      val a = splitted.head.toInt
      val b = splitted.last.toInt
      val before = dependencies(b)
      dependencies = dependencies + (b -> (before + a))
    )
    dependencies

  private def parseUpdates(lines: Seq[String]): Seq[Seq[Int]] =
    lines.map(l => l.split(",").map(_.toInt))
