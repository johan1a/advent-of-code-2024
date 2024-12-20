package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.split

object Day19:

  private var cache = Map[String, Long]()

  def part1(input: Seq[String]): Int =
    val (towels, designs) = parse(input)
    cache = Map()
    designs.count(isPossible(towels, _))

  def part2(input: Seq[String]): Long =
    val (towels, designs) = parse(input)
    designs.map(design =>
      cache = Map()
      nbrPossible(towels, design)
    ).sum

  private def nbrPossible(towels: Seq[String], design: String): Long =
    if cache.contains(design) then
      cache(design)
    else if design.isEmpty then
      1
    else
      val result = towels.filter(design.startsWith)
        .map { towel =>
          nbrPossible(towels, design.drop(towel.length))
        }
      cache = cache + (design -> result.sum)
      result.sum

  private def isPossible(towels: Seq[String], design: String): Boolean =
    nbrPossible(towels, design) > 0

  private def parse(input: Seq[String]): (Seq[String], Seq[String]) =
    val splitted = split(input)
    val towels = splitted.head.head.split(",").map(_.trim)
    val designs = splitted.last
    (towels, designs)
