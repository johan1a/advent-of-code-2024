package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*
import scala.collection.mutable.ArrayBuffer

object Day05 {

  def part1(input: Seq[String]): Int = {
    val splitted = split(input)
    val needs = parseRules(splitted.head)
    parseUpdates(splitted.last).filter(valid(needs, _)).map(getMiddle).sum
  }


  def part2(input: Seq[String]): Int = {
    val splitted = split(input)
    val needs = parseRules(splitted.head)
    val updates = parseUpdates(splitted.last)
    val sorted = updates.filter(!valid(needs, _)).map(sort(needs, _))
    println(sorted)
    sorted.map(getMiddle).sum
  }


  private def sort(allNeeds: Map[Int, Set[Int]], updates: Seq[Int]): Seq[Int] = {
    println(s"sorting $updates")
    var indegrees = computeIndegrees(allNeeds, updates)
    println(s"indegrees: $indegrees")

    var topLevel = updates.filter(u => indegrees.getOrElse(u, 0) == 0)
    println(s"toplevel: $topLevel")
    var result = Seq[Int]()
    while (topLevel.nonEmpty) {
      val n = topLevel.head
      topLevel = topLevel.tail
      result = result :+ n
      allNeeds.foreach { case (neighbor, dependents) =>
        if (dependents.contains(n)) {
          indegrees = indegrees + (neighbor -> (indegrees.getOrElse(neighbor,0) - 1))
          if (indegrees.getOrElse(neighbor, 0) == 0 && updates.contains(neighbor)) {
            topLevel = topLevel :+ neighbor
          }
        }
      }
      println(s"new indegrees: $indegrees")
    }

    println(result)
    assert(result.size == updates.size)

    result
  }

  private def computeIndegrees(allNeeds: Map[Int, Set[Int]], updates: Seq[Int]) = {
    var indegrees = Map[Int, Int]()
    allNeeds.foreach { (k, neighbors)  =>
      val nbrIn = neighbors.filter(updates.contains).size
      indegrees = indegrees + (k -> (indegrees.getOrElse(k,0) + nbrIn))
    }
    indegrees
  }

  private def canInsert(allNeeds: Map[Int, Set[Int]], updates: Seq[Int], done: Seq[Int], next: Int): Boolean = {
      val needs = allNeeds.getOrElse(next, Set.empty).filter(updates.contains)
      needs.isEmpty || needs.forall(done.contains)
  }

  private def valid(allNeeds: Map[Int, Set[Int]], updates: Seq[Int]): Boolean = {

    var done = Set[Int]()
    updates.forall { n =>
      val needs = allNeeds.getOrElse(n, Set.empty).filter(updates.contains)
      if (needs.isEmpty || needs.forall(done.contains)) {
        done = done + n
        true
      } else {
        false
      }
    }
  }

  private def getMiddle(updates: Seq[Int]) = {
    updates(updates.size / 2)
  }

  private def parseRules(lines: Seq[String]) = {
    var needs = Map[Int, Set[Int]]()
    lines.map(l =>
      val splitted = l.split("\\|")
      val a = splitted.head.toInt
      val b = splitted.last.toInt
      val before = needs.getOrElse(b, Set.empty)
      needs = needs + (b -> (before + a))
    )
    needs
  }

  private def parseUpdates(lines: Seq[String]): Seq[Seq[Int]] = {
    lines.map(l => l.split(",").map(_.toInt))
  }
}
