package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.*

object Day05 {

  def part1(input: Seq[String]): Int = {

    val splitted = split(input)
    val needs = parseRules(splitted.head)
    println(needs)
    val updates = parseUpdates(splitted.last)
    val ok = updates.filter(valid(needs, _))

    println(ok)
    ok.map(getMiddle).sum
  }

  private def valid(allNeeds: Map[Int, Set[Int]], updates: Seq[Int]): Boolean = {
    println(updates)

    var done = Set[Int]()
    updates.forall { n =>
      val needs = allNeeds.getOrElse(n, Set.empty).filter(updates.contains)
      if (needs.isEmpty || needs.forall(done.contains)) {
        done = done + n
        true
      } else {
        println(n)
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

  def part2(input: Seq[String]): Int = {
    -1
  }
}
