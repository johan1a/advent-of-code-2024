package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.split

object Day19:

  var cache = Map[String, Boolean]()

  def part1(input: Seq[String]): Int =
    cache = Map()
    val (towels, designs) = parse(input)
    designs.count(isPossible(towels, _))

  def part2(input: Seq[String]): Int =
    -1

  private def isPossible(towels: Set[String], design: String): Boolean =
    if cache.contains(design) then
      cache(design)
    else if design.isEmpty then
      true
    else
      val result = towels.exists { towel =>
        design.startsWith(towel) && isPossible(towels, design.drop(towel.length))
      }
      cache = cache + (design -> result)
      result

  private def parse(input: Seq[String]): (Set[String], Seq[String]) =
    val splitted = split(input)
    val towels = splitted.head.head.split(",").map(_.trim).toSet
    val designs = splitted.last
    (towels, designs)
