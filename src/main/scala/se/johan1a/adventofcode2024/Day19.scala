package se.johan1a.adventofcode2024

import se.johan1a.adventofcode2024.Utils.split

object Day19:

  var cache = Map[String, Boolean]()
  var cache2 = Map[String, Long]()

  def part1(input: Seq[String]): Int =
    cache = Map()
    val (towels, designs) = parse(input)
    designs.count(isPossible(towels, _))

  def part2(input: Seq[String]): Long =
    val (towels, designs) = parse(input)
    val nbrWays = designs.map(design =>
      cache2 = Map()
      nbrPossible(towels, design)
    )
    nbrWays.sum

  private def nbrPossible(towels: Seq[String], design: String): Long =
    if cache2.contains(design) then
      cache2(design)
    else if design.isEmpty then
      1
    else
      val startsWith = towels.filter(design.startsWith)
      val result = startsWith
        .map { towel =>
          val res = nbrPossible(towels, design.drop(towel.length))
          res
        }
      cache2 = cache2 + (design -> result.sum)
      result.sum

  private def isPossible(towels: Seq[String], design: String): Boolean =
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

  private def parse(input: Seq[String]): (Seq[String], Seq[String]) =
    val splitted = split(input)
    val towels = splitted.head.head.split(",").map(_.trim)
    val designs = splitted.last
    (towels, designs)
